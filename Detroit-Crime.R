# install.packages("dplyr")
library(dplyr)
# install.packages("lubridate")
library(lubridate)
# install.packages("highcharter")
library("highcharter")
# install.packages("sf")
library(sf)
# install.packages("leaflet")
library(leaflet)
# install.packages("mapview")
library(mapview)
# install.packages("tidyverse")
install.packages("funModeling")
# install.packages("Hmisc")
library(funModeling) 
library(tidyverse) 
library(Hmisc)


# Reading the data file
Detroit_crime <- read.csv("~/detroit-crime/DPD__All_Crime_Incidents__2009_-_Present__Provisional_-DPD__All_Crime_Incidents__2009_-_Present__Provisional_.csv")
# Having a variable to work with 
dc <- Detroit_crime
# View(dc)
# year(dc$INCIDENTDATE[1])

# Adding Null Columns to add Weather data for a different perspective 
dc$MAXIMUM_TEMP<-NA
dc$MINIMUM_TEMP<-NA
dc$AVERAGE_TEMP<-NA
dc$SNOW_DEPTH<-NA

# Changing the format of date using 'lubridate'
dc$INCIDENTDATE <- parse_date_time(dc$INCIDENTDATE, orders = 'mdy')

# Adding Temperature data for each year
# For the year 2009
dc_19 <-dc %>% 
    filter(year(INCIDENTDATE) == 2009) %>% 
    filter(month(INCIDENTDATE) == 01) %>% 
    mutate(MAXIMUM_TEMP = 39,MINIMUM_TEMP = -15, AVERAGE_TEMP = 17.3, SNOW_DEPTH = 14)
dc_29 <-dc %>% 
  filter(year(INCIDENTDATE) == 2009) %>% 
  filter(month(INCIDENTDATE) == 02) %>% 
  mutate(MAXIMUM_TEMP = 59,MINIMUM_TEMP = -4, AVERAGE_TEMP = 28.5, SNOW_DEPTH = 10)
dc_39 <-dc %>% 
  filter(year(INCIDENTDATE) == 2009) %>% 
  filter(month(INCIDENTDATE) == 03) %>% 
  mutate(MAXIMUM_TEMP = 69,MINIMUM_TEMP = 7, AVERAGE_TEMP = 38.7, SNOW_DEPTH = 1)
dc_49 <-dc %>% 
  filter(year(INCIDENTDATE) == 2009) %>% 
  filter(month(INCIDENTDATE) == 04) %>% 
  mutate(MAXIMUM_TEMP = 86,MINIMUM_TEMP = 29, AVERAGE_TEMP = 49.8, SNOW_DEPTH = 4)
dc_59 <-dc %>% 
  filter(year(INCIDENTDATE) == 2009) %>% 
  filter(month(INCIDENTDATE) == 05) %>% 
  mutate(MAXIMUM_TEMP = 83,MINIMUM_TEMP = 36, AVERAGE_TEMP = 59.4, SNOW_DEPTH = 0)
dc_69 <-dc %>% 
  filter(year(INCIDENTDATE) == 2009) %>% 
  filter(month(INCIDENTDATE) == 06) %>% 
  mutate(MAXIMUM_TEMP = 92,MINIMUM_TEMP = 45, AVERAGE_TEMP = 67.8, SNOW_DEPTH = 0)
dc_79 <-dc %>% 
  filter(year(INCIDENTDATE) == 2009) %>% 
  filter(month(INCIDENTDATE) == 07) %>% 
  mutate(MAXIMUM_TEMP = 86,MINIMUM_TEMP = 52, AVERAGE_TEMP = 68.9, SNOW_DEPTH = 0)
dc_89 <-dc %>% 
  filter(year(INCIDENTDATE) == 2009) %>% 
  filter(month(INCIDENTDATE) == 08) %>% 
  mutate(MAXIMUM_TEMP = 94,MINIMUM_TEMP = 49, AVERAGE_TEMP = 71.1, SNOW_DEPTH = 0)
dc_99 <-dc %>% 
  filter(year(INCIDENTDATE) == 2009) %>% 
  filter(month(INCIDENTDATE) == 09) %>% 
  mutate(MAXIMUM_TEMP = 85,MINIMUM_TEMP = 42, AVERAGE_TEMP = 66.0, SNOW_DEPTH = 0)
dc_109 <-dc %>% 
  filter(year(INCIDENTDATE) == 2009) %>% 
  filter(month(INCIDENTDATE) == 10) %>% 
  mutate(MAXIMUM_TEMP = 74,MINIMUM_TEMP = 29, AVERAGE_TEMP = 50.0, SNOW_DEPTH = 0)
dc_119 <-dc %>% 
  filter(year(INCIDENTDATE) == 2009) %>% 
  filter(month(INCIDENTDATE) == 11) %>% 
  mutate(MAXIMUM_TEMP = 68,MINIMUM_TEMP = 28, AVERAGE_TEMP = 45.3, SNOW_DEPTH = 0)
dc_129 <-dc %>% 
  filter(year(INCIDENTDATE) == 2009) %>% 
  filter(month(INCIDENTDATE) == 12) %>% 
  mutate(MAXIMUM_TEMP = 48,MINIMUM_TEMP = 13, AVERAGE_TEMP = 29.3, SNOW_DEPTH = 2)

# For the year 2010
dc_110 <-dc %>% 
  filter(year(INCIDENTDATE) == 2010) %>% 
  filter(month(INCIDENTDATE) == 01) %>% 
  mutate(MAXIMUM_TEMP = 46,MINIMUM_TEMP = 3, AVERAGE_TEMP = 25.1, SNOW_DEPTH = 5)
dc_210 <-dc %>% 
  filter(year(INCIDENTDATE) == 2010) %>% 
  filter(month(INCIDENTDATE) == 02) %>% 
  mutate(MAXIMUM_TEMP = 44,MINIMUM_TEMP = 10, AVERAGE_TEMP = 27.8, SNOW_DEPTH = 10)
dc_310 <-dc %>% 
  filter(year(INCIDENTDATE) == 2010) %>% 
  filter(month(INCIDENTDATE) == 03) %>% 
  mutate(MAXIMUM_TEMP = 69,MINIMUM_TEMP = 22, AVERAGE_TEMP = 42.4, SNOW_DEPTH = 5)
dc_410 <-dc %>% 
  filter(year(INCIDENTDATE) == 2010) %>% 
  filter(month(INCIDENTDATE) == 04) %>% 
  mutate(MAXIMUM_TEMP = 84,MINIMUM_TEMP = 30, AVERAGE_TEMP = 54.2, SNOW_DEPTH = 0)
dc_510 <-dc %>% 
  filter(year(INCIDENTDATE) == 2010) %>% 
  filter(month(INCIDENTDATE) == 05) %>% 
  mutate(MAXIMUM_TEMP = 88,MINIMUM_TEMP = 33, AVERAGE_TEMP = 62.7, SNOW_DEPTH = 0)
dc_610 <-dc %>% 
  filter(year(INCIDENTDATE) == 2010) %>% 
  filter(month(INCIDENTDATE) == 06) %>% 
  mutate(MAXIMUM_TEMP = 89,MINIMUM_TEMP = 50, AVERAGE_TEMP = 71.4, SNOW_DEPTH = 0)
dc_710 <-dc %>% 
  filter(year(INCIDENTDATE) == 2010) %>% 
  filter(month(INCIDENTDATE) == 07) %>% 
  mutate(MAXIMUM_TEMP = 94,MINIMUM_TEMP = 53, AVERAGE_TEMP = 76.6, SNOW_DEPTH = 0)
dc_810 <-dc %>% 
  filter(year(INCIDENTDATE) == 2010) %>% 
  filter(month(INCIDENTDATE) == 08) %>% 
  mutate(MAXIMUM_TEMP = 92,MINIMUM_TEMP = 52, AVERAGE_TEMP = 75.2, SNOW_DEPTH = 0)
dc_910 <-dc %>% 
  filter(year(INCIDENTDATE) == 2010) %>% 
  filter(month(INCIDENTDATE) == 09) %>% 
  mutate(MAXIMUM_TEMP = 92,MINIMUM_TEMP = 46, AVERAGE_TEMP = 64.5, SNOW_DEPTH = 0)
dc_1010 <-dc %>% 
  filter(year(INCIDENTDATE) == 2010) %>% 
  filter(month(INCIDENTDATE) == 10) %>% 
  mutate(MAXIMUM_TEMP = 83,MINIMUM_TEMP = 34, AVERAGE_TEMP = 54.9, SNOW_DEPTH = 0)
dc_1110 <-dc %>% 
  filter(year(INCIDENTDATE) == 2010) %>% 
  filter(month(INCIDENTDATE) == 11) %>% 
  mutate(MAXIMUM_TEMP = 63,MINIMUM_TEMP = 23, AVERAGE_TEMP = 42.0, SNOW_DEPTH = 0)
dc_1210 <-dc %>% 
  filter(year(INCIDENTDATE) == 2010) %>% 
  filter(month(INCIDENTDATE) == 12) %>% 
  mutate(MAXIMUM_TEMP = 52,MINIMUM_TEMP = 6, AVERAGE_TEMP = 25.6, SNOW_DEPTH = 6)

# For the year 2011
dc_111 <-dc %>% 
  filter(year(INCIDENTDATE) == 2011) %>% 
  filter(month(INCIDENTDATE) == 01) %>% 
  mutate(MAXIMUM_TEMP = 53,MINIMUM_TEMP = 2, AVERAGE_TEMP = 21.8, SNOW_DEPTH = 7)
dc_211 <-dc %>% 
  filter(year(INCIDENTDATE) == 2011) %>% 
  filter(month(INCIDENTDATE) == 02) %>% 
  mutate(MAXIMUM_TEMP = 54,MINIMUM_TEMP = -5, AVERAGE_TEMP = 24.8, SNOW_DEPTH = 16)
dc_311 <-dc %>% 
  filter(year(INCIDENTDATE) == 2011) %>% 
  filter(month(INCIDENTDATE) == 03) %>% 
  mutate(MAXIMUM_TEMP = 68,MINIMUM_TEMP = 16, AVERAGE_TEMP = 35.0, SNOW_DEPTH = 5)
dc_411 <-dc %>% 
  filter(year(INCIDENTDATE) == 2011) %>% 
  filter(month(INCIDENTDATE) == 04) %>% 
  mutate(MAXIMUM_TEMP = 83,MINIMUM_TEMP = 29, AVERAGE_TEMP = 47.3, SNOW_DEPTH = 1)
dc_511 <-dc %>% 
  filter(year(INCIDENTDATE) == 2011) %>% 
  filter(month(INCIDENTDATE) == 05) %>% 
  mutate(MAXIMUM_TEMP = 92,MINIMUM_TEMP = 36, AVERAGE_TEMP = 60.7, SNOW_DEPTH = 0)
dc_611 <-dc %>% 
  filter(year(INCIDENTDATE) == 2011) %>% 
  filter(month(INCIDENTDATE) == 06) %>% 
  mutate(MAXIMUM_TEMP = 96,MINIMUM_TEMP = 54, AVERAGE_TEMP = 70.6, SNOW_DEPTH = 0)
dc_711 <-dc %>% 
  filter(year(INCIDENTDATE) == 2011) %>% 
  filter(month(INCIDENTDATE) == 07) %>% 
  mutate(MAXIMUM_TEMP = 100,MINIMUM_TEMP = 58, AVERAGE_TEMP = 79.3, SNOW_DEPTH = 0)
dc_811 <-dc %>% 
  filter(year(INCIDENTDATE) == 2011) %>% 
  filter(month(INCIDENTDATE) == 08) %>% 
  mutate(MAXIMUM_TEMP = 92,MINIMUM_TEMP = 55, AVERAGE_TEMP = 73.2, SNOW_DEPTH = 0)
dc_911 <-dc %>% 
  filter(year(INCIDENTDATE) == 2011) %>% 
  filter(month(INCIDENTDATE) == 09) %>% 
  mutate(MAXIMUM_TEMP = 98,MINIMUM_TEMP = 42, AVERAGE_TEMP = 64.4, SNOW_DEPTH = 0)
dc_1011 <-dc %>% 
  filter(year(INCIDENTDATE) == 2011) %>% 
  filter(month(INCIDENTDATE) == 10) %>% 
  mutate(MAXIMUM_TEMP = 80,MINIMUM_TEMP = 31, AVERAGE_TEMP = 54.4, SNOW_DEPTH = 0)
dc_1111 <-dc %>% 
  filter(year(INCIDENTDATE) == 2011) %>% 
  filter(month(INCIDENTDATE) == 11) %>% 
  mutate(MAXIMUM_TEMP = 69,MINIMUM_TEMP = 24, AVERAGE_TEMP = 46.6, SNOW_DEPTH = 1)
dc_1211 <-dc %>% 
  filter(year(INCIDENTDATE) == 2011) %>% 
  filter(month(INCIDENTDATE) == 12) %>% 
  mutate(MAXIMUM_TEMP = 57,MINIMUM_TEMP = 17, AVERAGE_TEMP = 35.5, SNOW_DEPTH = 3)

# For the year 2012
dc_112 <-dc %>% 
  filter(year(INCIDENTDATE) == 2012) %>% 
  filter(month(INCIDENTDATE) == 01) %>% 
  mutate(MAXIMUM_TEMP = 56,MINIMUM_TEMP = 4, AVERAGE_TEMP = 30.7, SNOW_DEPTH = 3)
dc_212 <-dc %>% 
  filter(year(INCIDENTDATE) == 2012) %>% 
  filter(month(INCIDENTDATE) == 02) %>% 
  mutate(MAXIMUM_TEMP = 50,MINIMUM_TEMP = 11, AVERAGE_TEMP = 32.6, SNOW_DEPTH = 5)
dc_312 <-dc %>% 
  filter(year(INCIDENTDATE) == 2012) %>% 
  filter(month(INCIDENTDATE) == 03) %>% 
  mutate(MAXIMUM_TEMP = 86,MINIMUM_TEMP = 21, AVERAGE_TEMP = 50.7, SNOW_DEPTH = 1)
dc_412 <-dc %>% 
  filter(year(INCIDENTDATE) == 2012) %>% 
  filter(month(INCIDENTDATE) == 04) %>% 
  mutate(MAXIMUM_TEMP = 79,MINIMUM_TEMP = 30, AVERAGE_TEMP = 49.4, SNOW_DEPTH = 0)
dc_512 <-dc %>% 
  filter(year(INCIDENTDATE) == 2012) %>% 
  filter(month(INCIDENTDATE) == 05) %>% 
  mutate(MAXIMUM_TEMP = 95,MINIMUM_TEMP = 41, AVERAGE_TEMP = 65.3, SNOW_DEPTH = 0)
dc_612 <-dc %>% 
  filter(year(INCIDENTDATE) == 2012) %>% 
  filter(month(INCIDENTDATE) == 06) %>% 
  mutate(MAXIMUM_TEMP = 99,MINIMUM_TEMP = 49, AVERAGE_TEMP = 72.3, SNOW_DEPTH = 0)
dc_712 <-dc %>% 
  filter(year(INCIDENTDATE) == 2012) %>% 
  filter(month(INCIDENTDATE) == 07) %>% 
  mutate(MAXIMUM_TEMP = 102,MINIMUM_TEMP = 59, AVERAGE_TEMP = 79.0, SNOW_DEPTH = 0)
dc_812 <-dc %>% 
  filter(year(INCIDENTDATE) == 2012) %>% 
  filter(month(INCIDENTDATE) == 08) %>% 
  mutate(MAXIMUM_TEMP = 95,MINIMUM_TEMP = 52, AVERAGE_TEMP = 72.9, SNOW_DEPTH = 0)
dc_912 <-dc %>% 
  filter(year(INCIDENTDATE) == 2012) %>% 
  filter(month(INCIDENTDATE) == 09) %>% 
  mutate(MAXIMUM_TEMP = 89,MINIMUM_TEMP = 37, AVERAGE_TEMP = 64.0, SNOW_DEPTH = 0)
dc_1012 <-dc %>% 
  filter(year(INCIDENTDATE) == 2012) %>% 
  filter(month(INCIDENTDATE) == 10) %>% 
  mutate(MAXIMUM_TEMP = 78,MINIMUM_TEMP = 31, AVERAGE_TEMP = 52.7, SNOW_DEPTH = 0)
dc_1112 <-dc %>% 
  filter(year(INCIDENTDATE) == 2012) %>% 
  filter(month(INCIDENTDATE) == 11) %>% 
  mutate(MAXIMUM_TEMP = 69,MINIMUM_TEMP = 23, AVERAGE_TEMP = 40.4, SNOW_DEPTH = 1)
dc_1212 <-dc %>% 
  filter(year(INCIDENTDATE) == 2012) %>% 
  filter(month(INCIDENTDATE) == 12) %>% 
  mutate(MAXIMUM_TEMP = 63,MINIMUM_TEMP = 13, AVERAGE_TEMP = 35.9, SNOW_DEPTH = 6)

# For the year 2013
dc_113 <-dc %>% 
  filter(year(INCIDENTDATE) == 2013) %>% 
  filter(month(INCIDENTDATE) == 01) %>% 
  mutate(MAXIMUM_TEMP = 62,MINIMUM_TEMP = -1, AVERAGE_TEMP = 28.7, SNOW_DEPTH = 6)
dc_213 <-dc %>% 
  filter(year(INCIDENTDATE) == 2013) %>% 
  filter(month(INCIDENTDATE) == 02) %>% 
  mutate(MAXIMUM_TEMP = 46,MINIMUM_TEMP = 5, AVERAGE_TEMP = 27.2, SNOW_DEPTH = 6)
dc_313 <-dc %>% 
  filter(year(INCIDENTDATE) == 2013) %>% 
  filter(month(INCIDENTDATE) == 03) %>% 
  mutate(MAXIMUM_TEMP = 65,MINIMUM_TEMP = 16, AVERAGE_TEMP = 34.6, SNOW_DEPTH = 2)
dc_413 <-dc %>% 
  filter(year(INCIDENTDATE) == 2013) %>% 
  filter(month(INCIDENTDATE) == 04) %>% 
  mutate(MAXIMUM_TEMP = 80,MINIMUM_TEMP = 24, AVERAGE_TEMP = 46.4, SNOW_DEPTH = 1)
dc_513 <-dc %>% 
  filter(year(INCIDENTDATE) == 2013) %>% 
  filter(month(INCIDENTDATE) == 05) %>% 
  mutate(MAXIMUM_TEMP = 89,MINIMUM_TEMP = 30, AVERAGE_TEMP = 63.6, SNOW_DEPTH = 0)
dc_613 <-dc %>% 
  filter(year(INCIDENTDATE) == 2013) %>% 
  filter(month(INCIDENTDATE) == 06) %>% 
  mutate(MAXIMUM_TEMP = 89,MINIMUM_TEMP = 46, AVERAGE_TEMP = 69.7, SNOW_DEPTH = 0)
dc_713 <-dc %>% 
  filter(year(INCIDENTDATE) == 2013) %>% 
  filter(month(INCIDENTDATE) == 07) %>% 
  mutate(MAXIMUM_TEMP = 95,MINIMUM_TEMP = 52, AVERAGE_TEMP = 74.0, SNOW_DEPTH = 0)
dc_813 <-dc %>% 
  filter(year(INCIDENTDATE) == 2013) %>% 
  filter(month(INCIDENTDATE) == 08) %>% 
  mutate(MAXIMUM_TEMP = 89,MINIMUM_TEMP = 51, AVERAGE_TEMP = 71.9, SNOW_DEPTH = 0)
dc_913 <-dc %>% 
  filter(year(INCIDENTDATE) == 2013) %>% 
  filter(month(INCIDENTDATE) == 09) %>% 
  mutate(MAXIMUM_TEMP = 93,MINIMUM_TEMP = 42, AVERAGE_TEMP = 64.1, SNOW_DEPTH = 0)
dc_1013 <-dc %>% 
  filter(year(INCIDENTDATE) == 2013) %>% 
  filter(month(INCIDENTDATE) == 10) %>% 
  mutate(MAXIMUM_TEMP = 80,MINIMUM_TEMP = 29, AVERAGE_TEMP = 53.3, SNOW_DEPTH = 0)
dc_1113 <-dc %>% 
  filter(year(INCIDENTDATE) == 2013) %>% 
  filter(month(INCIDENTDATE) == 11) %>% 
  mutate(MAXIMUM_TEMP = 64,MINIMUM_TEMP = 14, AVERAGE_TEMP = 37.5, SNOW_DEPTH = 1)
dc_1213 <-dc %>% 
  filter(year(INCIDENTDATE) == 2013) %>% 
  filter(month(INCIDENTDATE) == 12) %>% 
  mutate(MAXIMUM_TEMP = 59,MINIMUM_TEMP = 5, AVERAGE_TEMP = 26.8, SNOW_DEPTH = 8)

# For the year 2014
dc_114 <-dc %>% 
  filter(year(INCIDENTDATE) == 2014) %>% 
  filter(month(INCIDENTDATE) == 01) %>% 
  mutate(MAXIMUM_TEMP = 45,MINIMUM_TEMP = -14, AVERAGE_TEMP = 16.4, SNOW_DEPTH = 16)
dc_214 <-dc %>% 
  filter(year(INCIDENTDATE) == 2014) %>% 
  filter(month(INCIDENTDATE) == 02) %>% 
  mutate(MAXIMUM_TEMP = 46,MINIMUM_TEMP = -5, AVERAGE_TEMP = 19.4, SNOW_DEPTH = 20)
dc_314 <-dc %>% 
  filter(year(INCIDENTDATE) == 2014) %>% 
  filter(month(INCIDENTDATE) == 03) %>% 
  mutate(MAXIMUM_TEMP = 58,MINIMUM_TEMP = 0, AVERAGE_TEMP = 28.7, SNOW_DEPTH = 16)
dc_414 <-dc %>% 
  filter(year(INCIDENTDATE) == 2014) %>% 
  filter(month(INCIDENTDATE) == 04) %>% 
  mutate(MAXIMUM_TEMP = 80,MINIMUM_TEMP = 22, AVERAGE_TEMP = 48.9, SNOW_DEPTH = 3)
dc_514 <-dc %>% 
  filter(year(INCIDENTDATE) == 2014) %>% 
  filter(month(INCIDENTDATE) == 05) %>% 
  mutate(MAXIMUM_TEMP = 88,MINIMUM_TEMP = 36, AVERAGE_TEMP = 61.4, SNOW_DEPTH = 0)
dc_614 <-dc %>% 
  filter(year(INCIDENTDATE) == 2014) %>% 
  filter(month(INCIDENTDATE) == 06) %>% 
  mutate(MAXIMUM_TEMP = 91,MINIMUM_TEMP = 48, AVERAGE_TEMP = 70.5, SNOW_DEPTH = 0)
dc_714 <-dc %>% 
  filter(year(INCIDENTDATE) == 2014) %>% 
  filter(month(INCIDENTDATE) == 07) %>% 
  mutate(MAXIMUM_TEMP = 94,MINIMUM_TEMP = 51, AVERAGE_TEMP = 69.9, SNOW_DEPTH = 0)
dc_814 <-dc %>% 
  filter(year(INCIDENTDATE) == 2014) %>% 
  filter(month(INCIDENTDATE) == 08) %>% 
  mutate(MAXIMUM_TEMP = 90,MINIMUM_TEMP = 49, AVERAGE_TEMP = 71.5, SNOW_DEPTH = 0)
dc_914 <-dc %>% 
  filter(year(INCIDENTDATE) == 2014) %>% 
  filter(month(INCIDENTDATE) == 09) %>% 
  mutate(MAXIMUM_TEMP = 93,MINIMUM_TEMP = 43, AVERAGE_TEMP = 63.5, SNOW_DEPTH = 0)
dc_1014 <-dc %>% 
  filter(year(INCIDENTDATE) == 2014) %>% 
  filter(month(INCIDENTDATE) == 10) %>% 
  mutate(MAXIMUM_TEMP = 72,MINIMUM_TEMP = 32, AVERAGE_TEMP = 52.1, SNOW_DEPTH = 0)
dc_1114 <-dc %>% 
  filter(year(INCIDENTDATE) == 2014) %>% 
  filter(month(INCIDENTDATE) == 11) %>% 
  mutate(MAXIMUM_TEMP = 64,MINIMUM_TEMP = 11, AVERAGE_TEMP = 35.8, SNOW_DEPTH = 2)
dc_1214 <-dc %>% 
  filter(year(INCIDENTDATE) == 2014) %>% 
  filter(month(INCIDENTDATE) == 12) %>% 
  mutate(MAXIMUM_TEMP = 53,MINIMUM_TEMP = 14, AVERAGE_TEMP = 33.6, SNOW_DEPTH = 1)

# For the year 2015
dc_115 <-dc %>% 
  filter(year(INCIDENTDATE) == 2015) %>% 
  filter(month(INCIDENTDATE) == 01) %>% 
  mutate(MAXIMUM_TEMP = 42,MINIMUM_TEMP = -2, AVERAGE_TEMP = 21.2, SNOW_DEPTH = 6)
dc_215 <-dc %>% 
  filter(year(INCIDENTDATE) == 2015) %>% 
  filter(month(INCIDENTDATE) == 02) %>% 
  mutate(MAXIMUM_TEMP = 43,MINIMUM_TEMP = -13, AVERAGE_TEMP = 14.1, SNOW_DEPTH = 18)
dc_315 <-dc %>% 
  filter(year(INCIDENTDATE) == 2015) %>% 
  filter(month(INCIDENTDATE) == 03) %>% 
  mutate(MAXIMUM_TEMP = 62,MINIMUM_TEMP = 0, AVERAGE_TEMP = 34.5, SNOW_DEPTH = 13)
dc_415 <-dc %>% 
  filter(year(INCIDENTDATE) == 2015) %>% 
  filter(month(INCIDENTDATE) == 04) %>% 
  mutate(MAXIMUM_TEMP = 76,MINIMUM_TEMP = 25, AVERAGE_TEMP = 50.0, SNOW_DEPTH = 0)
dc_515 <-dc %>% 
  filter(year(INCIDENTDATE) == 2015) %>% 
  filter(month(INCIDENTDATE) == 05) %>% 
  mutate(MAXIMUM_TEMP = 87,MINIMUM_TEMP = 39, AVERAGE_TEMP = 64.5, SNOW_DEPTH = 0)
dc_615 <-dc %>% 
  filter(year(INCIDENTDATE) == 2015) %>% 
  filter(month(INCIDENTDATE) == 06) %>% 
  mutate(MAXIMUM_TEMP = 90,MINIMUM_TEMP = 46, AVERAGE_TEMP = 68.8, SNOW_DEPTH = 0)
dc_715 <-dc %>% 
  filter(year(INCIDENTDATE) == 2015) %>% 
  filter(month(INCIDENTDATE) == 07) %>% 
  mutate(MAXIMUM_TEMP = 91,MINIMUM_TEMP = 53, AVERAGE_TEMP = 72.9, SNOW_DEPTH = 0)
dc_815 <-dc %>% 
  filter(year(INCIDENTDATE) == 2015) %>% 
  filter(month(INCIDENTDATE) == 08) %>% 
  mutate(MAXIMUM_TEMP = 92,MINIMUM_TEMP = 54, AVERAGE_TEMP = 71.7, SNOW_DEPTH = 0)
dc_915 <-dc %>% 
  filter(year(INCIDENTDATE) == 2015) %>% 
  filter(month(INCIDENTDATE) == 09) %>% 
  mutate(MAXIMUM_TEMP = 92,MINIMUM_TEMP = 47, AVERAGE_TEMP = 69.3, SNOW_DEPTH = 0)
dc_1015 <-dc %>% 
  filter(year(INCIDENTDATE) == 2015) %>% 
  filter(month(INCIDENTDATE) == 10) %>% 
  mutate(MAXIMUM_TEMP = 78,MINIMUM_TEMP = 31, AVERAGE_TEMP = 54.5, SNOW_DEPTH = 0)
dc_1115 <-dc %>% 
  filter(year(INCIDENTDATE) == 2015) %>% 
  filter(month(INCIDENTDATE) == 11) %>% 
  mutate(MAXIMUM_TEMP = 77,MINIMUM_TEMP = 20, AVERAGE_TEMP = 46.0, SNOW_DEPTH = 5)
dc_1215 <-dc %>% 
  filter(year(INCIDENTDATE) == 2015) %>% 
  filter(month(INCIDENTDATE) == 12) %>% 
  mutate(MAXIMUM_TEMP = 63,MINIMUM_TEMP = 21, AVERAGE_TEMP = 41.1, SNOW_DEPTH = 1)

# For the year 2016
dc_116 <-dc %>% 
  filter(year(INCIDENTDATE) == 2016) %>% 
  filter(month(INCIDENTDATE) == 01) %>% 
  mutate(MAXIMUM_TEMP = 53,MINIMUM_TEMP = 8, AVERAGE_TEMP =28.1, SNOW_DEPTH = 5)
dc_216 <-dc %>% 
  filter(year(INCIDENTDATE) == 2016) %>% 
  filter(month(INCIDENTDATE) == 02) %>% 
  mutate(MAXIMUM_TEMP = 64,MINIMUM_TEMP = -1, AVERAGE_TEMP = 31.2, SNOW_DEPTH = 4)
dc_316 <-dc %>% 
  filter(year(INCIDENTDATE) == 2016) %>% 
  filter(month(INCIDENTDATE) == 03) %>% 
  mutate(MAXIMUM_TEMP = 71,MINIMUM_TEMP = 18, AVERAGE_TEMP = 43.1, SNOW_DEPTH = 4)
dc_416 <-dc %>% 
  filter(year(INCIDENTDATE) == 2016) %>% 
  filter(month(INCIDENTDATE) == 04) %>% 
  mutate(MAXIMUM_TEMP = 81,MINIMUM_TEMP = 20, AVERAGE_TEMP = 46.3, SNOW_DEPTH = 1)
dc_516 <-dc %>% 
  filter(year(INCIDENTDATE) == 2016) %>% 
  filter(month(INCIDENTDATE) == 05) %>% 
  mutate(MAXIMUM_TEMP = 89,MINIMUM_TEMP = 34, AVERAGE_TEMP = 61.0, SNOW_DEPTH = 0)
dc_616 <-dc %>% 
  filter(year(INCIDENTDATE) == 2016) %>% 
  filter(month(INCIDENTDATE) == 06) %>% 
  mutate(MAXIMUM_TEMP = 94,MINIMUM_TEMP = 48, AVERAGE_TEMP = 71.1, SNOW_DEPTH = 0)
dc_716 <-dc %>% 
  filter(year(INCIDENTDATE) == 2016) %>% 
  filter(month(INCIDENTDATE) == 07) %>% 
  mutate(MAXIMUM_TEMP = 98,MINIMUM_TEMP = 54, AVERAGE_TEMP = 76.8, SNOW_DEPTH = 0)
dc_816 <-dc %>% 
  filter(year(INCIDENTDATE) == 2016) %>% 
  filter(month(INCIDENTDATE) == 08) %>% 
  mutate(MAXIMUM_TEMP = 96,MINIMUM_TEMP = 58, AVERAGE_TEMP = 76.7, SNOW_DEPTH = 0)
dc_916 <-dc %>% 
  filter(year(INCIDENTDATE) == 2016) %>% 
  filter(month(INCIDENTDATE) == 09) %>% 
  mutate(MAXIMUM_TEMP = 91,MINIMUM_TEMP = 49, AVERAGE_TEMP = 68.2, SNOW_DEPTH = 0)
dc_1016 <-dc %>% 
  filter(year(INCIDENTDATE) == 2016) %>% 
  filter(month(INCIDENTDATE) == 10) %>% 
  mutate(MAXIMUM_TEMP = 81,MINIMUM_TEMP = 34, AVERAGE_TEMP = 55.9, SNOW_DEPTH = 0)
dc_1116 <-dc %>% 
  filter(year(INCIDENTDATE) == 2016) %>% 
  filter(month(INCIDENTDATE) == 11) %>% 
  mutate(MAXIMUM_TEMP = 78,MINIMUM_TEMP = 21, AVERAGE_TEMP = 46.7, SNOW_DEPTH = 1)
dc_1216 <-dc %>% 
  filter(year(INCIDENTDATE) == 2016) %>% 
  filter(month(INCIDENTDATE) == 12) %>% 
  mutate(MAXIMUM_TEMP = 57,MINIMUM_TEMP = 0, AVERAGE_TEMP = 29.0, SNOW_DEPTH = 9)


# Binding all the months data for year 2009
r_09<-rbind.data.frame(dc_19,dc_29,dc_39,dc_49,dc_59,dc_69,dc_79,dc_89,dc_99,
                     dc_109,dc_119,dc_129)

# Binding all the months data for year 2010
r_10 <- rbind.data.frame(dc_110,dc_210,dc_310,dc_410,dc_510,dc_610,dc_710,dc_810,dc_910,
                       dc_1010,dc_1110,dc_1210)

# Binding all the months data for year 2011
r_11 <- rbind.data.frame(dc_111,dc_211,dc_311,dc_411,dc_511,dc_611,dc_711,dc_811,dc_911,
                       dc_1011,dc_1111,dc_1211)

# Binding all the months data for year 2012
r_12 <- rbind.data.frame(dc_112,dc_212,dc_312,dc_412,dc_512,dc_612,dc_712,dc_812,dc_912,
                       dc_1012,dc_1112,dc_1212)

# Binding all the months data for year 2013
r_13 <- rbind.data.frame(dc_113,dc_213,dc_313,dc_413,dc_513,dc_613,dc_713,dc_813,dc_913,
                       dc_1013,dc_1113,dc_1213)

# Binding all the months data for year 2014
r_14 <- rbind.data.frame(dc_114,dc_214,dc_314,dc_414,dc_514,dc_614,dc_714,dc_814,dc_914,
                       dc_1014,dc_1114,dc_1214)

# Binding all the months data for year 2015
r_15 <- rbind.data.frame(dc_115,dc_215,dc_315,dc_415,dc_515,dc_615,dc_715,dc_815,dc_915,
                       dc_1015,dc_1115,dc_1215)

# Binding all the months data for year 2016
r_16 <- rbind.data.frame(dc_116,dc_216,dc_316,dc_416,dc_516,dc_616,dc_716,dc_816,dc_916,
                       dc_1016,dc_1116,dc_1216)

# Binding all the year data and storing it in a dataframe
detcrime <- rbind.data.frame(r_09,r_10,r_11,r_12,r_13,r_14,r_15,r_16)
# View(detcrime)

# Removing unwanted columns
detcrime = subset(detcrime, select = -c(CASEID,CRIMEID,STATEOFFENSEFILECLASS,CRNO,NEIGHBORHOOD) )

# Separating year, month and date 
detcrime<- detcrime %>%
  dplyr::mutate(year = lubridate::year(INCIDENTDATE), 
                month = lubridate::month(INCIDENTDATE), 
                day = lubridate::day(INCIDENTDATE))

# Omitting columns with NA values
detcrime <- na.omit(detcrime)

# Changing date format
detcrime$INCIDENTDATE<-as.Date(detcrime$INCIDENTDATE)
# class(detcrime$INCIDENTDATE)

# Writing the new dataframe to a csv file 
write.csv(detcrime ,"MyData.csv", row.names = FALSE)

## EXPLORATORY DATA ANALYSIS
# Summary of the whole new dataset
summary(detcrime)

glimpse(detcrime)

# Plotting the number of crime records in each Council using Highcharts
detcrime%>%
  count(COUNCIL)%>%
  arrange(n)%>%
  hchart(type = "column", hcaes(x = COUNCIL, y = n, color = COUNCIL))

# Plotting the categories of Crime using Highcharts
detcrime%>%
  count(CATEGORY)%>%
  arrange(n)%>%
  hchart(type = "column", hcaes(x = CATEGORY, y = n, color = CATEGORY))

# Plotting longitude and latitude using scatter plot
plot(detcrime$LON,detcrime$LAT)

# Grouping the category to get count of each using dplyr
grp<-detcrime %>% 
  group_by(CATEGORY) %>% tally()

# Plotting map of crimes happened in Detroit city
mapview(detcrime, xcol = "LON", ycol = "LAT",crs = 4326 )

# Filtering by year 
y_09<- detcrime %>% 
  filter(year==2009)
# Plotting only those crime happened during year 2009
mapview(y_09, xcol = "LON", ycol = "LAT" )


## Note:
## Due to large amount of data and not having much features in laptop to 
## handle these kind of data i am now switching to Tableau.
## I could have definetely done this if i had a better configuration in laptop
## Or if was able to use College lab computers.