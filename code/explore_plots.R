# plotting biketown trip data with base R

library(tidyverse)
library(lubridate)

biketown <- read_csv("data/biketown-2018-trips.csv")

summary(biketown)

#this in tidyverse
biketown$hour <- 
  hms(biketown$StartTime) %>%
  hour()

biketown$month <- 
  mdy(biketown$StartDate) %>%
  month(label = T, abbr = T)
  

  
  
  
  
#same as this in base
stime <-  hms(biketown$StartTime)
biketown$hour <-  hour(stime)


freq_by_hour <- table(biketown$hour)
barplot(freq_by_hour)

#investigate hourly bins
hist(biketown$hour, breaks = seq(0, 24, 3))

#focus on the AM peak
am_peak <-  subset(biketown, hour>=7& hour<=10)
hist(am_peak$hour, breaks = seq(7, 10, 1)) #not great bob!!
barplot(table(am_peak$hour))

#looking into seasonal patterns
freq_by_month <- table(biketown$month)
barplot(freq_by_month)

#looking by station
freq_by_station <-table(biketown$StartHub)

#top 25 stations
top_25_stations <- sort(freq_by_station, decreasing = T)[1:25]
dotchart(top_25_stations)
