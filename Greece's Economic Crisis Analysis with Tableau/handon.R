install.packages("lubridate")
install.packages("data.table")
install.packages("ggmap")
install.packages("tidyverse")
library(readxl)
library(lubridate)
library(stringr)
library(data.table)
library(ggmap)
library(tidyverse)


q1 <- read.csv("2016/2016Q1-capitalbikeshare-tripdata.csv")
q2 <- read.csv("2016/2016Q2-capitalbikeshare-tripdata.csv")
q3 <- read.csv("2016/2016Q3-capitalbikeshare-tripdata.csv")
q4 <- read.csv("2016/2016Q4-capitalbikeshare-tripdata.csv")


format.date=function(date){
  return(paste(str_split_fixed(gsub("-", " ", as.character(date)), " ", 4)[,3],
               str_split_fixed(gsub("-", " ", as.character(date)), " ", 4)[,2], 
               str_split_fixed(gsub("-", " ", as.character(date)), " ", 4)[,1],
               str_split_fixed(gsub("-", " ", as.character(date)), " ", 4)[,4], 
               sep="/"))
}

full2016 <- rbind(q1, q2, q3, q4)
full2016$Start.date=format.date(full2016$Start.date)
full2016$End.date=format.date(full2016$End.date)

q1 <- read.csv("2017/2017Q1-capitalbikeshare-tripdata.csv")
q2 <- read.csv("2017/2017Q2-capitalbikeshare-tripdata.csv")
q3 <- read.csv("2017/2017Q3-capitalbikeshare-tripdata.csv")
q4 <- read.csv("2017/2017Q4-capitalbikeshare-tripdata.csv")

full2017 <- rbind(q1, q2, q3, q4)
full2017$Start.date=format.date(full2017$Start.date)
full2017$End.date=format.date(full2017$End.date)

q1 <- read.csv("2018/totalQ1_18.csv")
q2 <- read.csv("2018/totalQ2_18.csv")
q3 <- read.csv("2018/totalQ3_18.csv")
q4 <- read.csv("2018/totalQ4_18.csv")

full2018 <- rbind(q1, q2, q3, q4)
full2018$Start.date=format.date(full2018$Start.date)
full2018$End.date=format.date(full2018$End.date)
full2018$X <- NULL

total <- rbind(full2016, full2017, full2018)



total$dayperiod=as.numeric(str_split_fixed(str_split_fixed(total$Start.date, "/", 4)[,4],":",3)[,1])
total$dayperiod[which(total$dayperiod >= 6 & total$dayperiod <= 11)] = "Morning"
total$dayperiod[which(total$dayperiod >= 12 & total$dayperiod <= 17)] = "Afternoon"
total$dayperiod[which(total$dayperiod >= 18 & total$dayperiod <= 23)] = "Evening"
total$dayperiod[which(total$dayperiod < 6)] = "Early Morning"


fwrite(total, file = "C:/Users/Infernal/Desktop/fulldataset_period.csv")









total=fread("C:/Users/Infernal/Desktop/fulldataset_period.csv")

sources <- distinct(total, Start.station)
destinations <- distinct(total, End.station)
cities <- full_join(sources, destinations, by = c("Start.station" = "End.station"))
cities <- rename(cities, place = Start.station)
cities_df <- as.data.frame(cities)
cities_df$place=as.character(cities_df$place)

register_google(key="AIzaSyBf0dQH6L_DHydV6-XEhUQwZsQ7YYOIUAk")
locations_df <- mutate_geocode(cities_df, place)

summary(locations_df)
summary(total$Start.station)
summary(total$End.station)
total1= merge(x=total,y=locations_df,by.x = c("Start.station"), by.y = c("place"),all.x= TRUE)
summary(total1)

fwrite(total1, file = "C:/Users/Infernal/Desktop/fulldataset_loc.csv")
#locations_df=na.omit(locations_df)


colnames(total1)[colnames(total1)=="lon"] <- "start.lon"
colnames(total1)[colnames(total1)=="lat"] <- "start.lat"

sources <- distinct(total, Start.station)
destinations <- distinct(total, End.station)
cities <- full_join(destinations, sources, by = c("End.station" = "Start.station"))
cities <- rename(cities, place = End.station)
cities_df <- as.data.frame(cities)
cities_df$place=as.character(cities_df$place)

locations_df <- mutate_geocode(cities_df, place)

summary(locations_df)
summary(total1)
total2= merge(x=total1,y=locations_df,by.x = c("End.station"), by.y = c("place"),all.x= TRUE)
summary(total1)
total2$lon.y <- NULL
total2$lat.y <- NULL

colnames(total1)[colnames(total1)=="lon.x"] <- "end.lon"
colnames(total1)[colnames(total1)=="lat.x"] <- "end.lat"

fwrite(total1, file = "C:/Users/Infernal/Desktop/fulldataset_final.csv")

total=fread("C:/Users/Infernal/Desktop/tablo/fulldataset_final_final.csv")
