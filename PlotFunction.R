knitr::opts_chunk$set(echo = F, cache=T)
library(pacman)
p_load(devtools,httr,roxygen2,RCurl,dplyr)
library(dplyr);library(XML)
library(RCurl);library(selectr);library(clifro)
library(ggplot2)
library(tidyverse)
library(forcats)
library(lubridate)

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

Cumrainfall<-function()
  
  # Daily Rainfall
  Rain_dt  <-  cf_datatype(3, 1, 1)

# Hourly Air Temperature
Temp_dt  <-  cf_datatype(4, 1, 1)

# Daily Radiation
Rad_dt  <-  cf_datatype(5, 2, 1)

all.dts <- Rain_dt + Temp_dt + Rad_dt

daily.datalist  <-  cf_query(user = me, 
                             datatype = all.dts, 
                             station = cf_station(3950),
                             start_date = "2006-09-01 00",
                             end_date = "2016-04-01 00")

daily.datalist2  <-  cf_query(user = me, 
                              datatype = all.dts, 
                              station = cf_station(3950),
                              start_date = "2015-09-01 00",
                              end_date = "2016-04-01 00")


data10years_df <- daily.datalist %>% 
  map_df(`[`)
head(data10years_df)
names(data10years_df)

dataperiod_df <- daily.datalist2 %>% 
  map_df(`[`)
head(dataperiod_df)





summary(dataperiod_df$Date.local)

New10years <- data10years_df %>% 
  ungroup() %>% 
  mutate(Date.local=as.Date(Date.local)) %>% 
  mutate(MonthDay=format(Date.local, "%b-%d")) %>% 
  mutate(Year=format(Date.local, "%Y")) %>% 
  group_by(Year) %>% 
  mutate(CumAmount=cumsum(Amount.mm)) %>%
  ungroup() %>% 
  group_by(MonthDay) %>% 
  summarise(CumSum=mean(CumAmount, na.rm=T)) %>% 
  arrange(CumSum) %>% 
  mutate(MonthDay=fct_inorder(MonthDay))

dataperiod_df <- dataperiod_df %>%
  mutate(Date.local=as.Date(Date.local)) %>% 
  ungroup() %>%
  mutate(MonthDay=format(Date.local, "%b-%d")) %>% 
  left_join(.,New10years) %>% 
  mutate(NewCumSum=CumSum-CumSum[1])


p1 <- ggplot(data = dataperiod_df) +
  geom_col(mapping = aes(x=Date.local,y=Amount.mm),fill="blue",show.legend = FALSE,alpha=0.5)

wd <- resolution(ggplot_build(p1)$data[[1]]$x, FALSE) * 0.5  # 2365200


ggplot(data = dataperiod_df, aes(x=Date.local,y=Amount.mm)) +
  geom_col(fill="purple",show.legend = FALSE,alpha=0.4, width=wd) +
  geom_point(aes(x=Date.local,y=cumsum(Amount.mm/10)),color="red")+
  geom_path(aes(x=Date.local, y=cumsum(Amount.mm/10),group=1),color="red")+
  geom_point(aes(x=Date.local, y=NewCumSum/10,group=1),color="blue")+
  xlab("Date")+
  ylab("Daily rainfall (mm)")+
  theme_linley()+
  scale_x_date(date_labels = "%b")+
  scale_y_continuous("Cumulative rainfall (mm)", sec.axis = sec_axis(~.*10, name = derive()))


