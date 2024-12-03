library(readr)
library(readxl)
library(tidyverse)
library(data.table)
library(dplyr)
library(ggplot2)
library(lubridate)
library(minpack.lm)
library(broom)

## RAW DATA ##

#Temperature
afognak_df <- read_csv("Raw Data/AKTEMP-USFWS_AlaskaOSM-OSM_Afognak-series-daily.csv", 
                       skip = 81)
afognak_df$station <- "afognak"

buskin_df <- read_csv("Raw Data/AKTEMP-USFWS_AlaskaOSM-OSM_Buskin-series-daily.csv", 
                      skip = 81)
buskin_df$station <- "buskin"

klag_df <- read_csv("Raw Data/AKTEMP-USFWS_AlaskaOSM-OSM_Klag-series-daily.csv", 
                      skip = 81)
klag_df$station <- "klag"

neva_df <- read_csv("Raw Data/AKTEMP-USFWS_AlaskaOSM-OSM_Neva-series-daily.csv", 
                      skip = 81)
neva_df$station <- "neva"

all_stations <- rbind(afognak_df, buskin_df, klag_df, neva_df)

# Juvenile per Spawner
chinook_juvenile_data <- read_excel("Raw Data/Chinook Juvenile Data.xlsx")

# Length



## CLEANING DATA ##

# Temperature
temps <- all_stations %>% 
  select(station, date, mean_temp_c)

str(temps)

seasonal <- temps %>% 
  mutate(month = format(date, "%Y-%m"))

seasonal$month <- ym(seasonal$month)

str(seasonal)

seasonal_monthly_avg <- seasonal %>% 
  filter(year(month) >= 2009,
         year(month) <= 2019,
         month(month) >= 5,
         month(month) <= 9) %>% 
  group_by(month) %>% 
  summarize(seasonal_mean = mean(mean_temp_c, na.rm = TRUE))

seasonal_yearly_avg <- seasonal_monthly_avg %>% 
  mutate(year = format (month, "%Y")) %>% 
  group_by(year) %>% 
  summarize(annual_seasonal = mean(seasonal_mean, na.rm = TRUE))

seasonal_yearly_avg$year <- as.numeric(seasonal_yearly_avg$year)

write_csv(seasonal_monthly_avg, "Cleaned Data/seasonal_monthly_avg.csv")
write_csv(seasonal_yearly_avg, "Cleaned Data/seasonal_yearly_avg.csv")


# Juvenile per Spawner
juvenile_data <- chinook_juvenile_data %>% 
  select(`Juvenile Year`, `Total J/S`) %>% 
  filter(`Juvenile Year` > 2008 & `Juvenile Year` <= 2019)

str(juvenile_data)

juvenile_data <- juvenile_data %>% 
  rename(year = 'Juvenile Year') %>% 
  rename(juvenile_productivity = 'Total J/S')

juvenile_data$juvenile_productivity <- as.numeric(juvenile_data$juvenile_productivity)

write_csv(juvenile_data, "Cleaned Data/juvenile_data.csv")


# Length



## ANALYSIS ##

#Temperature
temp_mod <- lm(annual_seasonal ~ year, data = seasonal_yearly_avg)
summary(temp_mod)

seasonal_yearly_avg %>% 
  ggplot(aes(year, annual_seasonal))+
  geom_point()+
  geom_smooth(method="lm")+
  theme_bw()+
  scale_x_continuous(breaks = seq(min(joined_juv_seas$year),
                                  max(joined_juv_seas$year), by = 1))+
  ylab("Seaonal Mean Temp (C)")+
  xlab("Year")


#Joining Juvenile per Spawner & Temperature Data
joined_juv_seas <- inner_join(juvenile_data, seasonal_yearly_avg, by = "year")


#J/S by Temperature
nls_lm_temp <- nlsLM(juvenile_productivity ~ a * annual_seasonal^2 + b * annual_seasonal + c, 
                     data = joined_juv_seas, 
                     start = list(a=0.1, b=0.1, c=0.1))
summary(nls_lm_temp)

augment_temp <- augment(nls_lm_temp)

joined_juv_seas %>% 
  ggplot(aes(annual_seasonal, juvenile_productivity))+
  geom_point()+
  geom_line(aes(x = annual_seasonal, y = .fitted), data= augment_temp)+
  theme_bw()+
  ylab("Number of Juveniles per Spawner")+
  xlab("Seasonal Temperatures from 2010-2017")


#J/S by Year 
nls_lm_year <- nlsLM(juvenile_productivity ~ a * year^2 + b * year + c, 
                     data = joined_juv_seas, 
                     start = list(a=0.1, b=0.1, c=0.1)) 

summary(nls_lm_year)

augment_year <- augment(nls_lm_year)

joined_juv_seas %>%
  ggplot(aes(x = year, y = juvenile_productivity)) +
  geom_point() +
  geom_line(aes(y = .fitted), data = augment_year) +
  scale_x_continuous(breaks = seq(min(joined_juv_seas$year),
                                  max(joined_juv_seas$year), by = 1)) +
  ylab("Number of Juveniles per Spawner") +
  xlab("Year")+
  theme_bw()

##comparing effects of year vs effects of temperature on J/S
AIC(nls_lm_temp, nls_lm_year)

# Length
