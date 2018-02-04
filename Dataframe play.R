library(pacman)
p_load(devtools,httr,roxygen2,RCurl,dplyr)
library(dplyr);library(XML)
library(RCurl);library(selectr);library(clifro)
library(ggplot2)
library(tidyverse)
library(forcats)
library(lubridate)

data<-read.table("C:/Users/HRHMYG/Desktop/Dataframe try.csv",sep=",",header=TRUE) %>% 
  separate(Sample.ID, c("Site", "X", "SiteNo","Depth"))


  

yellow_df <-  data %>% 
  dplyr::select(Sampling.Date:Mineral.N..mg.kg.DM.) %>% 
  gather("measure","value",AMN.Raw..kg.ha.:Mineral.N..mg.kg.DM.)%>%
  mutate(value=ifelse(value=="<0.5","0.25",value)) %>% 
  mutate(value=as.numeric(value)) %>% 
  group_by(measure, Sampling.Date, Rep,Crop)%>%
  summarise(SumMeasure=sum(value))%>%
  ungroup()%>%
  group_by(measure, Sampling.Date,Crop)%>%
  summarise(Average=mean(SumMeasure))


green_df <- data %>%
  dplyr::select(Sampling.Date:Depth..cm.,AMN.corrected..kg.ha.:Organic.Matter...w.w.)  %>% 
  gather("measure","value", AMN.corrected..kg.ha.: Organic.Matter...w.w.)%>%
  group_by(measure, Depth, Crop)%>%
  summarise(Incaverage=sum(value))%>%
  ungroup()%>%
  group_by(measure,Crop)%>%
  mutate(ProfileAve=mean(Incaverage))
