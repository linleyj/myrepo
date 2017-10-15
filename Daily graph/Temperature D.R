
#This function allow you to get a barplot of monthly radiation with a long term average
#you just have to change the period your interested in and the station
#first run these packages and the theme_linley function
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


getLoginDetails <- function(){ 
  require(tcltk) 
  tt <- tktoplevel() 
  tkwm.title(tt, "Get login details") 
  Name <- tclVar("Login ID") 
  Password <- tclVar("Password") 
  entry.Name <- tkentry(tt,width="20", textvariable=Name) 
  entry.Password <- tkentry(tt, width="20", show="*",textvariable=Password) 
  tkgrid(tklabel(tt, text="Please enter your login details.")) 
  tkgrid(entry.Name) 
  tkgrid(entry.Password) 
  
  OnOK <- function() 
  {  
    tkdestroy(tt)  
  } 
  OK.but <-tkbutton(tt,text=" OK ", command=OnOK) 
  tkbind(entry.Password, "<Return>", OnOK) 
  tkgrid(OK.but) 
  tkfocus(tt) 
  tkwait.window(tt) 
  
  invisible(c(loginID=tclvalue(Name), password=tclvalue(Password))) 
} 
credentials <- getLoginDetails() 
me<-cf_user(as.vector(credentials[1]),as.vector(credentials[2]))


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



#Now run the DATES function

DDATEST<-function(start_D,end_D,Number_year,STATION,DATATYPE1,DATATYPE2,DATATYPE3)
{
  
  
  start_D2 <- as.Date(start_D) - years(Number_year)
  print(start_D2)
  
  New.test  <-  cf_query(user = me, 
                             station =  cf_station(STATION), datatype = cf_datatype(DATATYPE1,DATATYPE2,DATATYPE3),
                             start_date = start_D,
                             end_date = end_D)
  
  LTNew.test  <-  cf_query(user = me, 
                               station =  cf_station(STATION), datatype = cf_datatype(DATATYPE1,DATATYPE2,DATATYPE3),
                               start_date = start_D2,
                               end_date = end_D)
  
  Newtest_df <- New.test %>% 
    map_df(`[`)
  
  LTNewtest_df <- LTNew.test %>% 
    map_df(`[`)
  
  
  names(LTNewtest_df)[names(LTNewtest_df)=="Date(local)"] <- "DDate"
  names(LTNewtest_df)[names(LTNewtest_df)=="Tair(C)"] <- "Tair"
  names(Newtest_df)[names(Newtest_df)=="Date(local)"] <- "DDate"
  names(Newtest_df)[names(Newtest_df)=="Tair(C)"] <- "Tair"
  
  #Find NA's
  
  LT_sum <- LTNewtest_df %>%
    filter(is.na(Tair)) %>%
    summarise(total =sum(Tair))
  
  print(paste0("You have ", LT_sum$total, " NA's"))
  
  out <- list(LTNewtest_df=LTNewtest_df, Newtest_df=Newtest_df)
}


#Then run the PLOT function but make sure you put your station of interest just below (STAION =), you have to do it twice !

DPLOTT<- function(startDay,endDay,numberYear,station)
{
  Data10years_df <-   as.tibble(DDATEST(start_D = startDay, end_D = endDay, Number_year =  numberYear, STATION = station, 4,1,1)$LTNewtest_df)
  head(Data10years_df)
  start_D2 <- as.Date(startDay) - years(numberYear)
  print(start_D2)
  
  dataperiod_df <- as.tibble(DDATEST(start_D = startDay, end_D = endDay, Number_year =  numberYear, STATION = station, 4,1,1)$Newtest_df)
  print(summary(dataperiod_df$DDate))

  Newtestday_df <- dataperiod_df %>%
    mutate(MonthYear=format(DDate, "%Y-%b")) %>%
    mutate(YEAR=format(DDate, "%Y")) %>%
    mutate(MONTH=format(DDate, "%b"))
  
  Newtestday_df <- Newtestday_df %>%
    mutate(MonthDay=as.factor(format(DDate, "%Y-%b-%d")))%>%
    mutate(MonthDay=fct_inorder(MonthDay)) %>% 
    mutate(ordDate=format(DDate, "%j")) %>% 
    group_by(MonthDay,YEAR,MONTH,MonthYear,ordDate) %>% 
    summarise(MEANBYDAY=mean(Tair,na.rm=TRUE))
  
  LTNewtestday_df <- Data10years_df %>%
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
  
  
 P2<<- ggplot(data = df_join)+
    geom_point(aes(x=MonthDay,y=cumAmount.ST/100),color="red")+
    geom_point(aes(x=MonthDay, y=cumAmount.LT/100,group=1),color="blue")+
    geom_path(aes(x=MonthDay, y=cumAmount.ST/100,group=1),color="red")+
    xlab("Date")+
    ylab("Cumulative Temperature (C)")+
    theme_linley()+
    scale_y_continuous("Cumulative Temperature (C)", sec.axis = sec_axis(~.*100, name = derive()))+
    scale_x_date(date_labels = "%Y-%b")+
    geom_area(aes(x=MonthDay, y=MEANBYDAY.x),fill="purple",show.legend = FALSE,alpha=0.4)
}

#you can now set your period of interest and how many years you want to make a long term average

DPLOTT(startDay =  "2005-03-01 00", endDay =  "2008-08-01 00", numberYear =   10)

  