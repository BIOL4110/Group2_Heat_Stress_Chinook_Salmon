library(readr)
library(tidyverse)
library(data.table)
library(dplyr)

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

write.csv(annual_sm_df)

##monthly
monthly_df <- temp_df %>% 
  mutate(month = format(date, "%m")) %>% 
  group_by(station, month) %>% 
  summarize(monthly_mean = mean(mean_temp_c, na.rm = TRUE)) %>% 
  na.omit()