
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
library(tcltk)

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

DATEST<-function(start_D,end_D,Number_year,STATION,DATATYPE1,DATATYPE2,DATATYPE3)
{
  
  
  
  start_D2 <- as.Date(start_D) - years(Number_year)
  print(start_D2)
  
  choice<-cf_find_station(STATION)
  LAT<-choice$lat[1]
  LON<-choice$lon[1]
  location.st<-cf_find_station(lat = LAT, long = LON, rad = 10,search = "latlong")
  location.df <- location.st %>% 
    map_df(`[`)
  location.df<-location.df%>%
    arrange(distance)
  
  
  for(i in 1:dim(location.df)[1]){
    print(location.df[i,"name"])
    try(New.test  <-  cf_query(user = me, 
                               station =  cf_station(as.character(location.df[i,3][1])), datatype = cf_datatype(DATATYPE1,DATATYPE2,DATATYPE3),
                               start_date = start_D,
                               end_date = end_D))
    
    
    if(exists("New.test")==F) {
      print("Cliflo error - trying next station")
      next
    }
    
    else{
      print("Found a station with data")
      
      Newtest_df <- New.test %>% 
        map_df(`[`)
      
      names(Newtest_df)[names(Newtest_df)=="Tair(C)"] <- "Tair"
      names(Newtest_df)[names(Newtest_df)=="Date(local)"] <- "DDate"
      
      df_nas <- Newtest_df %>% 
        filter(is.na(Tair)) %>%
        summarise(total =sum(Tair))
      print(df_nas)
      if(df_nas$total>5&&i<dim(location.df)[1]){
        print("Too many nas trying next station")
        next
      }
      
      if(df_nas$total<5&&i<dim(location.df)){
        print("Usable station found with success !")
        break
      }
      
      else{
        print("No locations found")
        break
      }
    } 
  }
  
  
  
  
  for(i in 1:dim(location.df)[1]){
    print(location.df[i,"name"])
    try(LTNew.test  <-  cf_query(user = me, 
                                 station =  cf_station(as.character(location.df[i,3][1])), datatype = cf_datatype(DATATYPE1,DATATYPE2,DATATYPE3),
                                 start_date = start_D2,
                                 end_date = end_D))
    
    
    if(exists("LTNew.test")==F) {
      print("Cliflo error - trying next station")
      next
    }
    
    else{
      print("Found a station with data")
      
      LTNewtest_df <- LTNew.test %>% 
        map_df(`[`)
      
      names(LTNewtest_df)[names(LTNewtest_df)=="Tair(C)"] <- "Tair"
      names(LTNewtest_df)[names(LTNewtest_df)=="Date(local)"] <- "DDate"
      
      LTdf_nas <- LTNewtest_df %>% 
        filter(is.na(Tair)) %>%
        summarise(total =sum(Tair))
      print(LTdf_nas)
      if(LTdf_nas$total>5&&i<dim(location.df)[1]){
        print("Too many nas trying next station")
        next
      }
      
      if(LTdf_nas$total<5&&i<dim(location.df)){
        print("Usable station found with success !")
        break
      }
      
      else{
        print("No locations found")
        break
      }
    } 
  }
  
  
  return(list(LTNewtest_df=LTNewtest_df, Newtest_df=Newtest_df))
}

#Then run the PLOT function


PLOTT<- function(startDay,endDay,numberYear,station)
{
  Data10years_df <-   as.tibble(DATEST(start_D = startDay, end_D = endDay, Number_year =  numberYear, STATION = station, 4,1,1)$LTNewtest_df)
  head(Data10years_df)
  start_D2 <- as.Date(startDay) - years(numberYear)
  
  dataperiod_df <- as.tibble(DATEST(start_D = startDay, end_D = endDay, Number_year =  numberYear, STATION = station, 4,1,1)$Newtest_df)
  print(summary(dataperiod_df$DDate))

  Newtestday_df <- dataperiod_df %>%
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

  LTNewtestday_df <- Data10years_df %>%
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

  p <- ggplot(data = Newtestday_df) +
    geom_col(mapping = aes(x=MONTH,y=AVERAGE),fill="blue",show.legend = FALSE,alpha=0.5)
  wd <- resolution(ggplot_build(p)$data[[1]]$x, FALSE) * 0.5  # 2365200


  p2<<- ggplot(data = Newtestday_df) +
    geom_col(mapping= aes(x=MONTH,y=monthtemperature,fill=YEAR), show.legend = TRUE,alpha=0.4, width=wd,position=position_dodge()) +
    geom_point(data = LTNewtestday_df,aes(x=MONTH,y=AVERAGE),position=position_dodge(width=0.2))+
    xlab("Date")+
    ylab("Monthly average air Temp")+
    theme_linley()
}


  