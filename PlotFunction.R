
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

#username and password for Cliflo database

me = cf_user("linleyj","PFRCLIFLO")

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
  names(LTNewdata_df)[names(LTNewdata_df)=="Amount(mm)"] <- "Amount"
  names(Newdata_df)[names(Newdata_df)=="Date(local)"] <- "DDate"
  names(Newdata_df)[names(Newdata_df)=="Amount(mm)"] <- "Amount"
  
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
  

#Plot function
 
PLOT<- function(startDay,endDay,numberYear)
{
  Data10years_df <-   as.tibble(DATES(start_D = startDay, end_D = endDay, Number_year =  numberYear, STATION = "3950", 3,1,1)$LTNewdata_df)
  head(Data10years_df)
  start_D2 <- as.Date(startDay) - years(numberYear)
  print(start_D2)
  
New10years <- Data10years_df %>% 
  mutate(DDate=as.Date(DDate)) %>% 
  mutate(MonthDay=format(DDate, "%b-%d")) %>% 
  mutate(Year=format(DDate, "%Y")) %>% 
  arrange(DDate) %>% 
  mutate(MonthDay=fct_inorder(MonthDay)) %>% 
  mutate(Newyear=floor(time_length(interval(start_D2, DDate ), unit = "year")))%>%
  group_by(Newyear)%>%
  mutate(CumAmount=cumsum(Amount)) %>%
  ungroup() %>% 
  group_by(MonthDay) %>% 
  summarise(CumSum=mean(CumAmount, na.rm=T))


dataperiod_df <- as.tibble(DATES(start_D = startDay, end_D = endDay, Number_year =  numberYear, STATION = "3950", 3,1,1)$Newdata_df)
print(summary(dataperiod_df$DDate))

dataperiod_df <- dataperiod_df %>%
  mutate(DDate=as.Date(DDate)) %>% 
  ungroup() %>%
  mutate(MonthDay=format(DDate, "%b-%d")) %>% 
  mutate(Year=format(DDate, "%Y")) %>% 
  inner_join(.,New10years) %>% 
  mutate(NewCumSum=CumSum)

p1 <- ggplot(data = dataperiod_df) +
  geom_col(mapping = aes(x=DDate,y=Amount),fill="blue",show.legend = FALSE,alpha=0.5)

wd <- resolution(ggplot_build(p1)$data[[1]]$x, FALSE) * 0.5  # 2365200


ggplot(data = dataperiod_df, aes(x=DDate,y=Amount)) +
  geom_col(fill="purple",show.legend = FALSE,alpha=0.4, width=wd) +
  geom_point(aes(x=DDate,y=cumsum(Amount/10)),color="red")+
  geom_path(aes(x=DDate, y=cumsum(Amount/10),group=1),color="red")+
  geom_point(aes(x=DDate, y=NewCumSum/10,group=1),color="blue")+
  xlab("Date")+
  ylab("Daily rainfall (mm)")+
  theme_linley()+
  scale_x_date(date_labels = "%b")+
  scale_y_continuous("Cumulative rainfall (mm)", sec.axis = sec_axis(~.*10, name = derive()))

}

PLOT(startDay =  "2013-03-01 00", endDay =  "2013-08-01 00", numberYear =   10)



#test

mutate(Newyear=floor(time_length(interval(start_D2, DDate ), unit = "year")))
DATES(start_D = "2006-09-01 00", end_D = "2016-04-01 00", Number_year =  10, STATION = "3950", 3,1,1)

data10years_df <-   DATES(start_D = "2006-09-01 00", end_D = "2016-04-01 00", Number_year =  10, STATION = "3950", 3,1,1)
