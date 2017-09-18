knitr::opts_chunk$set(echo = F, cache=T)
library(pacman)
p_load(devtools,httr,roxygen2,RCurl,dplyr)
library(dplyr);library(XML)
library(RCurl);library(selectr);library(clifro)
library(ggplot2)
library(tidyverse)
library(forcats)
library(lubridate)
library(zoo)

options(RCurlOptions = list(proxy = 'http://proxy.pfr.co.nz:8080')) 
me = cf_user("linleyj","PFRCLIFLO")

cf_curl_opts(.opts = list(proxy = "https://proxy.pfr.co.nz:8080", ssl.verifypeer = FALSE))

theme_linley <- function(){
  theme_grey(base_size = 15, base_family = "") %+replace% 
  theme(panel.background = element_rect(fill = "white", 
  colour = NA), panel.border = element_rect(fill = NA, 
  colour = "grey20"), panel.grid.major = element_line(colour = "grey92"), 
  panel.grid.minor = element_line(colour = "grey92", 
  size = 0.25), strip.background = element_rect(fill = "grey85", 
  colour = "black"), legend.key = element_rect(fill = "white", 
                                                                                                                                     colour = NA), complete = TRUE)
}


DATES<-function(start_D,end_D,Number_year,STATION,DATATYPE1,DATATYPE2,DATATYPE3)
{
  
  #set up the new start date for the long term query
  
  start_D2 <- as.Date(start_D) - years(Number_year)
  print(start_D2)
  
  #Queries
  
  New.datalist  <-  cf_query(user = me, 
                             station =  cf_station(STATION), datatype = cf_datatype(DATATYPE1,DATATYPE2,DATATYPE3),
                             start_date = start_D,
                             end_date = end_D)
  
  LTNew.datalist  <-  cf_query(user = me, 
                               station =  cf_station(STATION), datatype = cf_datatype(DATATYPE1,DATATYPE2,DATATYPE3),
                               start_date = start_D2,
                               end_date = end_D)
  
  #turning lists into data frames
  
  Newdata_df <- New.datalist %>% 
    map_df(`[`)
  
  LTNewdata_df <- LTNew.datalist %>% 
    map_df(`[`)
  
  #Change head of columns to avoid any problem
  
  names(LTNewdata_df)[names(LTNewdata_df)=="Date(local)"] <- "DDate"
  names(LTNewdata_df)[names(LTNewdata_df)=="Amount(MJ/m2)"] <- "Amount"
  names(Newdata_df)[names(Newdata_df)=="Date(local)"] <- "DDate"
  names(Newdata_df)[names(Newdata_df)=="Amount(MJ/m2)"] <- "Amount"
  
  #Find NA's
  
  LT_sum <- LTNewdata_df %>%
    mutate(Year=format(DDate, "%Y")) %>% 
    mutate(Monthyear=format(DDate, "%Y-%b")) %>% 
    mutate(MonthDay=format(DDate, "%b-%d")) %>% 
    group_by(MonthDay) %>%
    filter(is.na(Amount)) %>%
    summarise(total =sum(Amount))
  
  out <- list(LTNewdata_df=LTNewdata_df, Newdata_df=Newdata_df)
  print(str(out))
  return(out)
}

DATES(start_D= "2012-03-01 00", end_D="2013-08-01 00" ,Number_year= 10,STATION = "3925",DATATYPE1 =  5,DATATYPE2 =  2, DATATYPE3 =  1)

PLOT<- function(startDay,endDay,numberYear)
{
  Data10years_df <-   as.tibble(DATES(start_D = startDay, end_D = endDay, Number_year =  numberYear, STATION = "3925", 5,2,1)$LTNewdata_df)
  head(Data10years_df)
  start_D2 <- as.Date(startDay) - years(numberYear)
  print(start_D2)
  
  dataperiod_df <- as.tibble(DATES(start_D = startDay, end_D = endDay, Number_year =  numberYear, STATION = "3925", 5,2,1)$Newdata_df)
  print(summary(dataperiod_df$DDate))
  
  YR <- format(as.Date(startDay), "%Y")
  MONTHDIFF <- 12*as.numeric(as.yearmon(as.Date(startDay)) - as.yearmon(paste(YR, "Jan", sep = "-"), "%Y-%b"))
  
  dataperiod_df <- dataperiod_df %>%
    ungroup() %>%
    mutate(DDate=as.Date(DDate)) %>% 
    mutate(MonthYear=format(DDate, "%Y-%b")) %>%
    mutate(YEAR=format(DDate, "%Y")) %>%
    mutate(MONTH=format(DDate, "%b"))
  
  meandata_df <- dataperiod_df %>%
    group_by(MonthYear,YEAR,MONTH) %>% 
    summarise(monthradiation=sum(Amount,na.rm=TRUE)) %>%
    ungroup() %>% 
    arrange(MonthYear) %>% 
    mutate(MONTH=factor(MONTH, levels = month.abb)) %>% 
    mutate(MONTH=fct_shift(MONTH,MONTHDIFF)) %>%
    ungroup() %>%
    group_by(MONTH) %>%
    mutate(AVERAGE=mean(monthradiation))
  
  Data10years_df <- Data10years_df %>%
    ungroup() %>%
    mutate(DDate=as.Date(DDate)) %>% 
    mutate(MonthYear=format(DDate, "%Y-%b")) %>%
    mutate(YEAR=format(DDate, "%Y")) %>%
    mutate(MONTH=format(DDate, "%b"))
  
  Data10yearsF_df <- Data10years_df %>%
    group_by(MonthYear,YEAR,MONTH) %>% 
    summarise(monthradiation=sum(Amount,na.rm=TRUE)) %>%
    ungroup() %>% 
    arrange(MonthYear) %>% 
    mutate(MONTH=factor(MONTH, levels = month.abb)) %>% 
    mutate(MONTH=fct_shift(MONTH,MONTHDIFF)) %>%
    ungroup() %>%
    group_by(MONTH) %>%
    mutate(AVERAGE=mean(monthradiation))
  
  p2 <- ggplot(data = meandata_df) +
    geom_col(mapping = aes(x=MONTH,y=monthradiation),fill="blue",show.legend = FALSE,alpha=0.5)
  wd <- resolution(ggplot_build(p2)$data[[1]]$x, FALSE) * 0.5  # 2365200
  
  p1 <- ggplot(meandata_df, aes(x=MONTH,y=monthradiation,fill=YEAR)) +
    geom_col(show.legend = TRUE,alpha=0.4, width=wd,position=position_dodge()) +
    geom_point(data = Data10yearsF_df ,aes(x=MONTH,y=AVERAGE,color="red"))+
    xlab("Date")+
    ylab("Monthly radiation (MJ/m2)")+
    theme_linley()
  print(p1)
}

PLOT(startDay =  "2009-03-01 00", endDay =  "2013-08-01 00", numberYear =   10)
