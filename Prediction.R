library(pacman)
p_load(devtools,httr,roxygen2,RCurl,dplyr)
library(dplyr);library(XML)
library(RCurl);library(selectr);library(clifro)
library(ggplot2)
library(tidyverse)
library(forcats)
library(lubridate)

Prediction<-function(file.location)
{
dataprediction<-read.table(file.location,sep=",",header=TRUE) %>%
  separate(Sample.ID, c("Site", "X", "SiteNo","Depth"))

dataprediction<-dataprediction %>%
  mutate(SampleID=1:dim(dataprediction)[1]) 


format2<-dataprediction %>%
  gather("measure","value", AMN.Raw..kg.ha.: Organic.Matter...w.w.)%>%
  mutate(value=ifelse(value=="<0.5","0.25",value)) %>% 
  mutate(value=as.numeric(value))%>%
  arrange(measure,Depth)%>%
  dplyr::select(Sampling.Date,Crop,Depth,Rep,measure,value)

datatop2<-format2%>%
  filter(Crop == "Ryegrass")%>%
  filter(Depth=="0")

datadown2<-format2%>%
  filter(Crop == "Ryegrass")%>%
  filter(Depth!="0")

Jointest<-datatop2%>%
  full_join(.,datadown2, by=c("Rep","measure","Crop","Sampling.Date")) %>% 
  mutate(measure.x=)




test_lm2 <- Jointest %>% 
  group_by(measure) %>% 
  do(fit=lm(value.y~ Depth.y, data =.))
test_glance2 <- broom::glance(test_lm2, fit)%>%
  ungroup() %>% 
  mutate(x=0) %>% 
  mutate(y=1) %>%
  arrange(r.squared)


p1 <- Jointest%>%
  ggplot(., aes(Depth.y, value.y, group = Rep)) + 
  geom_point(alpha=0.7, aes(colour=as.factor(Rep))) +
  facet_grid(measure~Crop, scales = "free")+
  geom_smooth(method="lm", formula =  y~x )+
  theme_bw()+
  ylab("Depth")+
  xlab("Measure")
  geom_text(data=test_glance2, aes(x=1, y=0.1, label=round(r.squared, digits = 2)))

  
test_lm2 <- Jointest %>%
  group_by(measure) %>%
  do(fit=lm(value.y~ Depth.y, data =.))
  
test_glance2 <- broom::glance(test_lm2, fit)
test_glance2$x <- seq(0,1, length.out = dim(test_glance2)[1])
test_glance2$y <- seq(0,1, length.out = dim(test_glance2)[1])
test_glance2$Rep <- rep("Rep", times=dim(test_glance2)[1])
  
  
  
Prediction_plot<-Jointest%>%
  mutate(Depth.y=factor(Depth.y)) %>%
  mutate(Depth.x=factor(Depth.x)) %>%
  ggplot(., aes(Depth.y, value.y, group=Rep)) +
  geom_point(alpha=0.7, aes(colour=as.factor(Rep))) +
  facet_grid(measure~Crop, scales = "free")+
  geom_smooth(method="lm", formula =  y~x )+
  theme_bw()+
  ylab("Depth")+
  xlab("Measure")+
  geom_text(data=test_glance2, aes(x=x, y=y, label=round(r.squared, digits = 2))) 

print(Prediction_plot)
}

  
Prediction(file.location="C:/Users/HRHMYG/Desktop/Dataframe try.csv")  
