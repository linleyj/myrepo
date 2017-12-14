library(pacman)
p_load(devtools,httr,roxygen2,RCurl,dplyr)
library(dplyr);library(XML)
library(RCurl);library(selectr);library(clifro)
library(ggplot2)
library(tidyverse)
library(forcats)
library(lubridate)

Depth_Depth<-function(file.location)
{
Depthdata<-read.table(file.location,sep=",",header=TRUE) %>%
  separate(Sample.ID, c("Site", "X", "SiteNo","Depth"))

Depthdata<-Depthdata %>%
  mutate(SampleID=1:dim(Depthdata)[1]) 


format<-Depthdata %>%
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

Joindata<-datatop%>%
  full_join(.,datadown, by=c("Rep","measure","Crop","Sampling.Date")) %>% 
  mutate(measure.x=)



Depth_lm <- Joindata %>% 
  group_by(measure, Depth.y) %>% 
  do(fit=lm(value.y~ value.x, data =.))
Depth_glance <- broom::glance(Depth_lm, fit)%>%
  arrange(r.squared)

FinalModel_lm <<- Joindata %>% 
  group_by(measure) %>% 
  do(fit=lm(value.y~ factor(Depth.y)*value.x, data =.))
summary(FinalModel_lm$fit[[1]])
anova(FinalModel_lm$fit[[1]])

Depthplot <- Joindata%>%
  ggplot(., aes(value.x, value.y)) + 
  geom_point(alpha=0.3, aes(colour=Depth.y)) +
  facet_grid(measure~Depth.y, scales = "free")+
  geom_smooth(method="lm")+
  theme_bw()+
  ylab("Measure at 0-20cm")+
  xlab("Measure at other depths")+
  geom_text(data=Depth_glance, aes(x=1, y=1, label=round(r.squared, digits = 2)))

print(Depthplot)
}

Depth_Depth(file.location = "C:/Users/HRHMYG/Desktop/Dataframe try.csv")

