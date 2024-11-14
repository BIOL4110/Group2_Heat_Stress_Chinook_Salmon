library(readr)
library(tidyverse)
library(data.table)
library(dplyr)
library(ggplot2)
library(lubridate)

##Compiling Temp Data

afognak_df <- read_csv("AKTEMP-USFWS_AlaskaOSM-OSM_Afognak-series-daily.csv", 
                       +     skip = 81)
afognak_df$station <- "afognak"

buskin_df <- read_csv("AKTEMP-USFWS_AlaskaOSM-OSM_Buskin-series-daily.csv", 
                      +     skip = 81)
buskin_df$station <- "buskin"

klag_df <- read_csv("AKTEMP-USFWS_AlaskaOSM-OSM_Klag-series-daily.csv", 
                    +     skip = 81)
klag_df$station <- "klag"

neva_df <- read_csv("AKTEMP-USFWS_AlaskaOSM-OSM_Neva-series-daily.csv", 
                    +     skip = 81)
neva_df$station <- "neva"

all_df <- rbind(afognak_df, buskin_df, klag_df, neva_df)

##cleaning##
temp_df<- all_df %>% 
  select(station, date, mean_temp_c)

##annual
temp_df$date <- as.Date(temp_df$date, format = "%Y-%m-%d")

annual_sum_df <- temp_df %>% 
  mutate(year = format(date, "%Y")) %>% 
  group_by(station, year) %>% 
  summarize(annual_mean_T = mean(mean_temp_c, na.rm = TRUE)) %>% 
  filter(year > 2009 & year < 2018)

write.csv(annual_sum_df, "Annual_Temps.csv", row.names = FALSE)

##monthly
monthly_df <- temp_df %>% 
  mutate(month = format(date, "%m")) %>%
  mutate(year = format(date, "%Y")) %>% 
  group_by(station,year, month) %>% 
  summarize(monthly_mean = mean(mean_temp_c, na.rm = TRUE)) %>% 
  filter(month > "04" & month < "10") %>% 
  filter(year > 2009 & year < 2018)
  
write.csv(monthly_df, "Monthly_Temps.csv", row.names = FALSE)

##graphing monthly 
monthly_df %>% 
  ggplot(aes(month, monthly_mean))+
  geom_point()+
  xlab("Year")+
  ylab("Mean temp (C)")+
  theme_minimal()

##graphing annual generally
ggplot(annual_sum_df, aes(as.numeric(year), annual_mean_T))+
  geom_point()+
  geom_smooth(method="lm")+
  theme_bw()+
  ylab("Annual Mean Temp (C)")+
  xlab("Year")

##graphing annual by station
ggplot(annual_sum_df, aes(as.numeric(year), annual_mean_T, col = station))+
  geom_point()+
  geom_smooth(method="lm", se = FALSE)+
  theme_bw()+
  ylab("Annual Mean Temp (C)")+
  xlab("Year")

## SEASONAL TEMP ##
str(temp_df)

seasonal_df <- temp_df %>% 
  mutate(yearmonth = format(date, "%Y-%m")) 

seasonal_df$yearmonth <- ym(seasonal_df$yearmonth)

str(seasonal_df)

filtered_seasonal <- seasonal_df %>% 
  filter(year(yearmonth) >= 2010,
    year(yearmonth) <= 2017,
    month(yearmonth) >= 5,
    month(yearmonth) <= 9) %>% 
  group_by(station, yearmonth) %>% 
  summarize(seasonal_mean = mean(mean_temp_c, na.rm = TRUE))

str(filtered_seasonal)

write.csv(filtered_seasonal, "Filtered_Seasonal_Temps.csv", row.names = FALSE)

##plotting seasonal means by station
filtered_seasonal %>% 
  ggplot(aes(yearmonth, seasonal_mean, col = station))+
  geom_point()+
  geom_smooth(method="lm", se = FALSE)+
  theme_bw()+
  ylab("Seaonal Mean Temp (C)")+
  xlab("Date")

##plotting generally
filtered_seasonal %>% 
  ggplot(aes(yearmonth, seasonal_mean))+
  geom_point()+
  geom_smooth(method="lm", se = FALSE)+
  theme_bw()+
  ylab("Seaonal Mean Temp (C)")+
  xlab("Date")


## looking at summer temps just to see  
summer_temps <- seasonal_df %>% 
  filter(year(yearmonth) >= 2010,
         year(yearmonth) <= 2017,
         month(yearmonth) >= 6,
         month(yearmonth) <= 8) %>% 
  group_by(station, yearmonth) %>% 
  summarize(summer_mean = mean(mean_temp_c, na.rm = TRUE))

str(filtered_seasonal)

summer_temps %>% 
  ggplot(aes(yearmonth, summer_mean))+
  geom_point()+
  geom_smooth(method="lm")+
  theme_bw()+
  ylab("Summer Mean Temp (C)")+
  xlab("Date")
