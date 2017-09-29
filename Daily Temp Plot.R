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


New.test  <-  cf_query(user = me, 
                       station =  cf_station(3925), datatype = cf_datatype(4,1,1),
                       start_date = "2009-03-01 00",
                       end_date = "2013-08-01 00")

LTNew.test  <-  cf_query(user = me, 
                         station =  cf_station(3925), datatype = cf_datatype(4,1,1),
                         start_date = "2003-03-01 00",
                         end_date = "2013-08-01 00")

Newtest_df <- New.test %>% 
  map_df(`[`)

LTNewtest_df <- LTNew.test %>% 
  map_df(`[`)


names(LTNewtest_df)[names(LTNewtest_df)=="Date(local)"] <- "DDate"
names(LTNewtest_df)[names(LTNewtest_df)=="Tair(C)"] <- "Tair"
names(Newtest_df)[names(Newtest_df)=="Date(local)"] <- "DDate"
names(Newtest_df)[names(Newtest_df)=="Tair(C)"] <- "Tair"

Newtestday_df <- Newtest_df %>%
  mutate(MonthYear=format(DDate, "%Y-%b")) %>%
  mutate(YEAR=format(DDate, "%Y")) %>%
  mutate(MONTH=format(DDate, "%b"))

Newtestday_df <- Newtestday_df %>%
  mutate(MonthDay=as.factor(format(DDate, "%Y-%b-%d")))%>%
  mutate(MonthDay=fct_inorder(MonthDay)) %>% 
  mutate(ordDate=format(DDate, "%j")) %>% 
  group_by(MonthDay,YEAR,MONTH,MonthYear,ordDate) %>% 
  summarise(MEANBYDAY=mean(Tair,na.rm=TRUE))

LTNewtestday_df <- LTNewtest_df %>%
  mutate(MonthYear=format(DDate, "%Y-%b")) %>%
  mutate(YEAR=format(DDate, "%Y")) %>%
  mutate(MONTH=format(DDate, "%b"))

LTNewtestday_df <- LTNewtestday_df %>%
  mutate(MonthDay=as.factor(format(DDate, "%Y-%b-%d")))%>%
  mutate(MonthDay=fct_inorder(MonthDay)) %>% 
  mutate(ordDate=format(DDate, "%j")) %>% 
  group_by(MonthDay,YEAR,MONTH,MonthYear,ordDate) %>% 
  summarise(MEAN=mean(Tair,na.rm=TRUE))%>%
  ungroup()%>%
  group_by(ordDate)%>%
  summarise(MEANBYDAY=mean(MEAN,na.rm=TRUE))
  


df_join <- Newtestday_df %>% 
  left_join(.,LTNewtestday_df, by="ordDate") %>% 
  ungroup() %>% 
  mutate(cumAmount.ST=cumsum(MEANBYDAY.x), cumAmount.LT =cumsum(MEANBYDAY.y)) %>% 
  mutate(MonthDay=as.Date(MonthDay,format="%Y-%b-%d"))


ggplot(data = df_join)+
  geom_point(aes(x=MonthDay,y=cumAmount.ST/100),color="red")+
  geom_point(aes(x=MonthDay, y=cumAmount.LT/100,group=1),color="blue")+
  geom_path(aes(x=MonthDay, y=cumAmount.ST/100,group=1),color="red")+
  xlab("Date")+
  ylab("Cumulative Temperature (C)")+
  theme_linley()+
  scale_y_continuous("Cumulative Temperature (C)", sec.axis = sec_axis(~.*100, name = derive()))+
  scale_x_date(date_labels = "%Y-%b")+
  geom_area(aes(x=MonthDay, y=MEANBYDAY.x),fill="purple",show.legend = FALSE,alpha=0.4)
  