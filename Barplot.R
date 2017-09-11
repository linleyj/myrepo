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




barplot.fun <- function(start_date,end_date,data_types,stations){
  
  my_stations <- stations
  my_data_types <- data_types 
  my_start_date <- start_date
  my_end_date <- end_date
  YR <- format(as.Date(my_start_date), "%Y")
  
MONTHDIFF <- 12*as.numeric(as.yearmon(as.Date(my_start_date)) - as.yearmon(paste(YR, "Jan", sep = "-"), "%Y-%b"))

Dailyrain.datalist  <-  cf_query(user = me, 
                             datatype = data_types, 
                             station = stations,
                             start_date = my_start_date,
                             end_date = my_end_date)


datayear1_df <- Dailyrain.datalist %>% 
  map_df(`[`)
head(datayear1_df)
names(datayear1_df)

names(datayear1_df)[names(datayear1_df)=="Date(local)"] <- "DDate"
names(datayear1_df)[names(datayear1_df)=="Amount(mm)"] <- "Amount"

datayear1_df <- datayear1_df %>%
  ungroup() %>%
  mutate(DDate=as.Date(DDate)) %>% 
  mutate(MonthYear=format(DDate, "%Y-%b")) %>%
  mutate(YEAR=format(DDate, "%Y")) %>%
  mutate(MONTH=format(DDate, "%b"))

meandata_df <- datayear1_df %>%
  group_by(MonthYear,YEAR,MONTH) %>% 
  summarise(monthrain=sum(Amount,na.rm=TRUE)) %>%
  ungroup() %>% 
  arrange(MonthYear) %>% 
  mutate(MONTH=factor(MONTH, levels = month.abb)) %>% 
  mutate(MONTH=fct_shift(MONTH,MONTHDIFF)) %>%
  ungroup() %>%
  group_by(MONTH) %>%
  mutate(AVERAGE=mean(monthrain))

p1 <- ggplot(meandata_df, aes(x=MONTH,y=monthrain,fill=YEAR)) +
  geom_col(show.legend = TRUE,alpha=0.4, width=wd,position=position_dodge()) +
  geom_point(aes(x=MONTH,y=AVERAGE,color="red"))+
  xlab("Date")+
  ylab("Monthly rainfall (mm)")+
  theme_linley()
print(p1)
}

barplot.fun(start_date="2010-10-01 00",end_date = "2013-09-29 00",data_types = cf_datatype(3, 1, 1),stations = cf_station(3950))
