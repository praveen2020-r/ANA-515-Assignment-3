# Get and print current working directory.
rm()
print(getwd())

# Set current working directory.
setwd("C:\\Users\\praveen\\Desktop\\R assign\\assign3")

# Get and print current working directory.
print(getwd())
data <- read.csv("StormEvents_details.csv")
names(data)
myvars <- c("BEGIN_YEARMONTH","BEGIN_DAY","BEGIN_TIME","END_YEARMONTH","END_DAY","END_TIME","EPISODE_ID","EVENT_ID","STATE","STATE_FIPS","CZ_TYPE","CZ_FIPS","CZ_NAME","EVENT_TYPE","BEGIN_LAT","BEGIN_LON","END_LAT","END_LON","DATA_SOURCE")
newdata <- data[myvars]
library(tidyr)
library(tidyr)
newdata<-separate(newdata,BEGIN_YEARMONTH, into = c('BEGIN_YEAR', 'BEGIN_MONTH'), sep = 4,remove = TRUE,convert = TRUE)
newdata<-separate(newdata,BEGIN_TIME, into = c('BEGIN_HOUR', 'BEGIN_MINUTE'), sep = 2,remove = TRUE,convert = TRUE)
newdata<-separate(newdata,END_YEARMONTH, into = c('END_YEAR', 'END_MONTH'), sep = 4,remove = TRUE,convert = TRUE)
newdata<-separate(newdata,END_TIME, into = c('END_HOUR', 'END_MINUTE'), sep = 2,remove = TRUE,convert = TRUE)


library(lubridate)

library(tidyverse)
library(magrittr) # needs to be run every time you start R and want to use %>%
library(dplyr)
library(stringr)
newdata <- newdata %>% 
  mutate(BEGIN_DATE_TIME = dmy_hm(str_c(BEGIN_DAY,BEGIN_MONTH,BEGIN_YEAR,BEGIN_HOUR,BEGIN_MINUTE, sep="-")))

newdata <- newdata %>% 
  mutate(END_DATE_TIME = dmy_hm(str_c(END_DAY,END_MONTH,END_YEAR,END_HOUR,END_MINUTE, sep="-")))



str_to_upper(newdata$STATE)

newdata<-filter(newdata, newdata$CZ_TYPE == "C")

newdata = subset(newdata, select = -c(CZ_TYPE) )

newdata$STATE_FIPS=str_pad(newdata$STATE_FIPS,width=3,side="left",pad = "0")
newdata$CZ_FIPS=str_pad(newdata$CZ_FIPS, width=3,side="left",pad = "0")

 
newdata=unite(newdata,"fips", STATE_FIPS:CZ_FIPS, sep = "", remove = TRUE)

newdata = rename_all(newdata,tolower)

newdataframe<-data.frame(table(newdata$state))

newdataframe1<-rename(newdataframe,c("state"="Var1"))

dfstate <- data.frame(state.name, state.area, state.region)

dfstate<-(mutate_all(dfstate, toupper))

mergeddata<-merge(x=newdataframe1,y=dfstate,by.x="state",by.y = "state.name")

library(ggplot2)
strm_plot<-ggplot(mergeddata,aes(x=state.area,y=Freq))+
  geom_point(aes(color=state.region))+


strm_plot