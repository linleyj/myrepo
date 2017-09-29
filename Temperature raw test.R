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
  group_by(MonthDay,YEAR,MONTH,MonthYear) %>% 
  summarise(MEANBYDAY=mean(Tair,na.rm=TRUE))
 
Newtestday_df <- Newtestday_df %>%
  group_by(MonthYear,YEAR,MONTH) %>% 
  summarise(monthtemperature=mean(MEANBYDAY,na.rm=TRUE)) %>%
  ungroup() %>% 
  arrange(MonthYear) %>% 
  mutate(MONTH=factor(MONTH, levels = month.abb)) %>% 
  ungroup() %>%
  group_by(MONTH) %>%
  mutate(AVERAGE=mean(monthtemperature))%>%
  arrange(MONTH)

#longterm

LTNewtestday_df <- LTNewtest_df %>%
  mutate(MonthYear=format(DDate, "%Y-%b")) %>%
  mutate(YEAR=format(DDate, "%Y")) %>%
  mutate(MONTH=format(DDate, "%b"))

LTNewtestday_df <- LTNewtestday_df %>%
  mutate(MonthDay=as.factor(format(DDate, "%Y-%b-%d")))%>%
  mutate(MonthDay=fct_inorder(MonthDay)) %>% 
  group_by(MonthDay,YEAR,MONTH,MonthYear) %>% 
  summarise(MEANBYDAY=mean(Tair,na.rm=TRUE))

LTNewtestday_df <- LTNewtestday_df %>%
  group_by(MonthYear,YEAR,MONTH) %>% 
  summarise(monthtemperature=mean(MEANBYDAY,na.rm=TRUE)) %>%
  ungroup() %>% 
  arrange(MonthYear) %>% 
  mutate(MONTH=factor(MONTH, levels = month.abb)) %>% 
  ungroup() %>%
  group_by(MONTH) %>%
  summarise(AVERAGE=mean(monthtemperature))%>%
  arrange(MONTH)

p2 <- ggplot(data = Newtestday_df) +
  geom_col(mapping = aes(x=MONTH,y=AVERAGE),fill="blue",show.legend = FALSE,alpha=0.5)
wd <- resolution(ggplot_build(p2)$data[[1]]$x, FALSE) * 0.5  # 2365200


 p1<- ggplot(data = Newtestday_df) +
  geom_col(mapping= aes(x=MONTH,y=monthtemperature,fill=YEAR), show.legend = TRUE,alpha=0.4, width=wd,position=position_dodge()) +
  geom_point(data = LTNewtestday_df,aes(x=MONTH,y=AVERAGE),position=position_dodge(width=0.2))+
  xlab("Date")+
  ylab("Monthly average air Temp")+
  theme_linley()

  print(p1)
  