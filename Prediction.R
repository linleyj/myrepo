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

data<-data %>%
  mutate(SampleID=1:dim(data)[1]) 


format<-data %>%
  gather("measure","value", AMN.Raw..kg.ha.: Organic.Matter...w.w.)%>%
  mutate(value=ifelse(value=="<0.5","0.25",value)) %>% 
  mutate(value=as.numeric(value))%>%
  arrange(measure,Depth)%>%
  dplyr::select(Sampling.Date,Crop,Depth,Rep,measure,value)

datatop<-format%>%
  filter(Crop == "Ryegrass")%>%
  filter(Depth=="0")

datadown<-format%>%
  filter(Crop == "Ryegrass")%>%
  filter(Depth!="0")

Jointest<-datatop%>%
  full_join(.,datadown, by=c("Rep","measure","Crop","Sampling.Date")) %>% 
  mutate(measure.x=)




test_lm <- Jointest %>% 
  group_by(measure) %>% 
  do(fit=lm(value.y~ Depth.y, data =.))
test_glance <- broom::glance(test_lm, fit)%>%
  mutate(x=0:1)%>%
  mutate(y=0:1)%>%
  arrange(r.squared)


Jointest%>%
  ggplot(., aes(Depth.y, value.y, group = Rep)) + 
  geom_point(alpha=0.7, aes(colour=as.factor(Rep))) +
  facet_grid(measure~Crop, scales = "free")+
  geom_smooth(method="lm", formula =  y~x )+
  theme_bw()+
  ylab("Depth")+
  xlab("Measure")
  geom_text(data=test_glance, aes(x=1, y=0.1, label=round(r.squared, digits = 2)))

 
  
