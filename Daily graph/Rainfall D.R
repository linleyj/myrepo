
#installation of necessary packages

knitr::opts_chunk$set(echo = F, cache=T)
library(pacman)
p_load(devtools,httr,roxygen2,RCurl,dplyr)
library(dplyr);library(XML)
library(RCurl);library(selectr);library(clifro)
library(ggplot2)
library(tidyverse)
library(forcats)
library(lubridate)

#proxy setup

options(RCurlOptions = list(proxy = 'http://proxy.pfr.co.nz:8080')) 
cf_curl_opts(.opts = list(proxy = "https://proxy.pfr.co.nz:8080", ssl.verifypeer = FALSE))


#plot theme

theme_linley <- function(){
  theme_grey(base_size = 15, base_family = "") %+replace% 
  theme(panel.background = element_rect(fill = "white", 
  colour = NA), panel.border = element_rect(fill = NA, 
  colour = "grey20"), panel.grid.major = element_line(colour = "grey92"), 
  panel.grid.minor = element_line(colour = "grey92", 
  size = 0.25), strip.background = element_rect(fill = "grey85", 
  colour = "black"), legend.key = element_rect(fill = "white",                                                                                                                                    colour = NA), complete = TRUE)
}

#first function is doing the queries


DDATESP<-function(start_D,end_D,Number_year,STATION,DATATYPE1,DATATYPE2,DATATYPE3)
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
  names(LTNewdata_df)[names(LTNewdata_df)=="Amount(mm)"] <- "Amount"
  names(Newdata_df)[names(Newdata_df)=="Date(local)"] <- "DDate"
  names(Newdata_df)[names(Newdata_df)=="Amount(mm)"] <- "Amount"
  
  #Find NA's
 
  LT_sum <- LTNewdata_df %>%
    filter(is.na(Amount)) %>%
    summarise(total =sum(Amount))
  
  print(paste0("You have ", LT_sum$total, " NA's"))
  
  out <- list(LTNewdata_df=LTNewdata_df, Newdata_df=Newdata_df)
}
  


#Plot function "please change change the station number to that of your choice and make sure you did it twice"
 
DPLOTP<- function(startDay,endDay,numberYear, station)
{
  Data10years_df <-   as.tibble(DDATESP(start_D = startDay, end_D = endDay, Number_year =  numberYear, STATION = station, 3,1,1)$LTNewdata_df)
  head(Data10years_df)
  start_D2 <- as.Date(startDay) - years(numberYear)
  print(start_D2)
  
  dataperiod_df <- as.tibble(DDATESP(start_D = startDay, end_D = endDay, Number_year =  numberYear, STATION = station, 3,1,1)$Newdata_df)
  print(summary(dataperiod_df$DDate))
  
  dfstep<-Data10years_df %>%
    ungroup() %>%
    mutate(DDate=as.Date(DDate)) %>% 
    ungroup() %>%
    arrange(DDate) %>% 
    mutate(ordDate=format(DDate, "%j")) %>% 
    mutate(MonthDay=as.factor(format(DDate, "%b-%d")))%>%
    mutate(MonthDay=fct_inorder(MonthDay)) %>% 
    group_by(ordDate) %>% 
    summarise(MEANBYDAY=mean(Amount,na.rm=TRUE))

df_join <- dataperiod_df %>% 
  mutate(ordDate=format(DDate, "%j")) %>% 
  full_join(.,dfstep, by="ordDate") %>% 
  arrange(DDate) %>% 
  ungroup() %>% 
  mutate(cumAmount=cumsum(MEANBYDAY))

p <- ggplot(data = dataperiod_df) +
  geom_col(mapping = aes(x=as.Date(DDate),y=Amount),fill="blue",show.legend = FALSE,alpha=0.5)

wd <- resolution(ggplot_build(p)$data[[1]]$x, FALSE) * 0.5  # 2365200

Check<<-df_join
Check2<<-dfstep

 P1<<-ggplot(data = df_join, aes(x=as.Date(DDate),y=Amount)) +
  geom_col(fill="purple",show.legend = FALSE,alpha=0.4, width=wd) +
  geom_point(aes(x=as.Date(DDate),y=cumsum(Amount/10)),color="red")+
  geom_path(aes(x=as.Date(DDate), y=cumsum(Amount/10),group=1),color="red")+
  geom_point(aes(x=as.Date(DDate), y=cumAmount/10,group=1),color="blue")+
  xlab("Date")+
  ylab("Daily rainfall (mm)")+
  theme_linley()+
  scale_x_date(date_labels = "%Y-%b")+
  scale_y_continuous("Cumulative rainfall (mm)", sec.axis = sec_axis(~.*10, name = derive()))

}

















