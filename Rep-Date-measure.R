library(pacman)
p_load(devtools,httr,roxygen2,RCurl,dplyr)
library(dplyr);library(XML)
library(RCurl);library(selectr);library(clifro)
library(ggplot2)
library(tidyverse)
library(forcats)
library(lubridate)
library(grid)
library(gridExtra)

Rep_Date<-function(file.location)
{
datadate<-read.table(file.location,sep=",",header=TRUE) %>%
  separate(Sample.ID, c("Site", "X", "SiteNo","Depth"))

datadate<-datadate %>%
  mutate(SampleID=1:dim(datadate)[1]) 

Longdata<<-datadate %>%
  gather("measure","value", AMN.Raw..kg.ha.: Organic.Matter...w.w.)%>%
  mutate(value=ifelse(value=="<0.5","0.25",value)) %>% 
  mutate(value=as.numeric(value))%>%
  arrange(measure,Depth)%>%
  dplyr::select(Sampling.Date,Crop,Depth,Rep,Sampling.Date,measure,value)

Longdata<-Longdata%>%
  filter(Crop=="Ryegrass")

Comparative_lm <- Longdata %>% 
  group_by(measure,Sampling.Date) %>% 
  do(fit=lm(value~ poly(as.numeric(Depth),2), data =.))
test_glance <- broom::glance(Comparative_lm, fit)%>%
  arrange(r.squared) %>% 
  group_by(measure) %>% 
  mutate(Sampling.Date=factor(Sampling.Date))

newdata_df <- Longdata %>% 
  filter(Depth=="0") %>% 
  group_by(measure, Sampling.Date) %>% 
  select(Depth,value)

predict(Comparative_lm$fit[[1]], newdata = newdata_df[1:3,])
Longdata <- Longdata%>%
  mutate(Sampling.Date=factor(Sampling.Date))

plots_df <- Longdata%>%
  mutate(Sampling.Date=factor(Sampling.Date)) %>% 
  group_by(measure)%>%
  do(plots=  ggplot(., aes(as.numeric(Depth), value)) + 
  geom_point(alpha=0.3, aes(colour=Depth)) +
  geom_smooth(method=lm, formula=y~poly(x,2))+
  theme_bw()+
  ylab("value")+
  xlab("depth")+
  geom_text(data=test_glance[test_glance$measure==.$measure,], aes(x=10, y=1, label=round(r.squared, digits = 4)))+
  facet_grid(~Sampling.Date, scales = "free")+
  ggtitle(.$measure)
    )

grid.arrange(grobs=plots_df$plots, ncol=4, 
             top="top label", bottom="bottom\nlabel", 
             left="left label", right="right label")
}

Rep_Date(file.location ="C:/Users/HRHMYG/Desktop/Dataframe try.csv" )

