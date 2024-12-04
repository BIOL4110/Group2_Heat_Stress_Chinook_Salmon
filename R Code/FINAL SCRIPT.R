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
kodiak_asl_data <- read_csv("Raw Data/ASL_formatted_SoutheastKodiak(2010 - 2017) - Chinook and Chum.csv")
unuk_asl_data <- read_excel("Raw Data/Unuk 1995 to 2017_Lewis.xlsx", sheet = 3, skip = 5)

## CLEANING DATA ##

# Temperature
temps <- all_stations %>% 
  select(station, date, mean_temp_c) # all else extraneous

str(temps)

seasonal <- temps %>% 
  mutate(month = format(date, "%m")) %>% 
  mutate(year = format(date, "%Y"))

seasonal$month <- as.numeric(seasonal$month)
seasonal$year <- as.numeric(seasonal$year)

str(seasonal)

seasonal_monthly_avg <- seasonal %>% 
  group_by(year, month) %>% 
  summarize(seasonal_mean = mean(mean_temp_c, na.rm = TRUE))

seasonal_monthly_avg_filt <- seasonal_monthly_avg %>% 
  filter(year >= 2010, year <= 2019, month >= 5, month <= 9)

seasonal_yearly_avg <- seasonal_monthly_avg_filt %>% 
  group_by(year) %>% 
  summarize(annual_seasonal = mean(seasonal_mean, na.rm = TRUE))

seasonal_yearly_avg$year <- as.numeric(seasonal_yearly_avg$year)

seasonal_yearly_avg_filt <- seasonal_yearly_avg %>% 
  filter(year >= 2009, year <= 2019)

write_csv(seasonal_monthly_avg_filt, "Cleaned Data/seasonal_monthly_avg_filt.csv")
write_csv(seasonal_yearly_avg_filt, "Cleaned Data/seasonal_yearly_avg_filt.csv")

str(seasonal_yearly_avg_filt)

# Juvenile per Spawner
juvenile_data <- chinook_juvenile_data %>% 
  select(`Juvenile Year`, `Total J/S`) %>% 
  filter(`Juvenile Year` > 2008 & `Juvenile Year` <= 2019) 
  #only selected necessary information, removed Canadian specific stocks, water discharge, and other environmental factors

str(juvenile_data)

juvenile_data <- juvenile_data %>% 
  rename(year = 'Juvenile Year') %>% 
  rename(juvenile_productivity = 'Total J/S')

juvenile_data$juvenile_productivity <- as.numeric(juvenile_data$juvenile_productivity)

write_csv(juvenile_data, "Cleaned Data/juvenile_data.csv")


# Length
kodiak_length <- kodiak_asl_data %>% 
  filter(Species == "chinook") %>% 
  select(c("sampleDate", "Length", "Species"))
  #removing NA columns and extraneous information

kodiak_length$sampleDate <- ymd(kodiak_length$sampleDate)

kodiak_length <- kodiak_length %>%
  mutate(year = format(sampleDate, "%Y"),
         month = format(sampleDate, "%m"))

kodiak_avg <- kodiak_length %>%
  group_by(year, month) %>%
  summarize(average_length = mean(Length, na.rm = TRUE))

kodiak_avg$location <- "kodiak"

write_csv(kodiak_avg, "kodiak_avg.csv")

unuk_length <- unuk_asl_data %>% 
  rename(Length = "MM MEF") %>% 
  select("DATE", "Length")
#removing NA columns and extraneous information

unuk_length$DATE <- ymd(unuk_length$DATE)

unuk_length_filt <- unuk_length %>% 
  mutate(year = format(DATE, "%Y"), month = format(DATE, "%m")) %>%  
  filter(year >= 2010 & year <= 2017)

unuk_avg <- unuk_length_filt %>%
  group_by(year, month) %>%
  summarize(average_length = mean(Length, na.rm = TRUE))

unuk_avg$location <- "unuk"

write_csv(unuk_avg, "unuk_avg.csv")

all_length <- rbind(kodiak_avg, unuk_avg)

all_length$year <- as.numeric(all_length$year)
all_length$month <- as.numeric(all_length$month)

all_length_filt <- all_length %>%
  filter(month >= 5,
         month <= 9)

write_csv(all_length_filt, "avg_length_filt.csv")

## ANALYSIS ##

#Temperature
temp_mod <- lm(annual_seasonal ~ year, data = seasonal_yearly_avg_filt)
summary(temp_mod)

seasonal_yearly_avg_filt %>% 
  ggplot(aes(year, annual_seasonal))+
  geom_point()+
  geom_smooth(method="lm")+
  scale_x_continuous(breaks = c(2009:2019))+
  theme_bw()+
  ylab("Seaonal Mean Temp (°C)")+
  xlab("Year")

#Joining Juvenile per Spawner & Temperature Data
joined_juv_seas <- inner_join(juvenile_data, seasonal_yearly_avg_filt, by = "year")


#J/S by Temperature
nls_lm_temp <- nlsLM(juvenile_productivity ~ a * annual_seasonal^2 + b * annual_seasonal + c, 
                     data = joined_juv_seas, 
                     start = list(a=0.1, b=0.1, c=0.1))
summary(nls_lm_temp)

augment_temp <- augment(nls_lm_temp)

joined_juv_seas %>% 
  ggplot(aes(annual_seasonal, juvenile_productivity))+
  geom_point()+
  geom_line(aes(x = annual_seasonal, y = .fitted), data= augment_temp, col = "blue")+
  theme_bw()+
  ylab("Number of Juveniles per Spawner")+
  xlab("Seasonal Temperatures from 2009-2019 (°C)")


#J/S by Year 
nls_lm_year <- nlsLM(juvenile_productivity ~ a * year^2 + b * year + c, 
                     data = joined_juv_seas, 
                     start = list(a=0.1, b=0.1, c=0.1)) 

summary(nls_lm_year)

augment_year <- augment(nls_lm_year)

joined_juv_seas %>%
  ggplot(aes(x = year, y = juvenile_productivity)) +
  geom_point() +
  geom_line(aes(y = .fitted), col ="blue", data = augment_year) +
  scale_x_continuous(breaks = seq(min(joined_juv_seas$year),
                                  max(joined_juv_seas$year), by = 1)) +
  ylab("Number of Juveniles per Spawner") +
  xlab("Year")+
  theme_bw()

##comparing effects of year vs effects of temperature on J/S
AIC(nls_lm_temp, nls_lm_year)

# Length

#joining temperature with length data
joined_length_seas <- inner_join(all_length_filt, seasonal_monthly_avg_filt, by = c("month", "year"))

joined_length_seas$date <- make_date(year = joined_length_seas$year,
                                     month = joined_length_seas$month)


ggplot(joined_length_seas, aes(x = seasonal_mean, y = average_length, color = location)) +
  geom_point() +
  geom_smooth(method = "lm") +
  xlab("Temperature (°C)") +
  ylab("Salmon Length (mm)") +
  theme_bw()

lm_temp <- lm(average_length~seasonal_mean, data = joined_length_seas)
summary(lm_temp)

res1 <- resid(lm_temp)
plot(fitted(lm_temp), res1)
abline(0,0)
qqnorm(res1)
qqline(res1)
plot(density(res1))

lm_year_temp <- lm(average_length~seasonal_mean + date, data = joined_length_seas)
summary(lm_year_temp)

res2 <- resid(lm_year_temp)
plot(fitted(lm_year_temp), res2)
abline(0,0)
qqnorm(res2)
qqline(res2)
plot(density(res2))

ggplot(joined_length_seas, aes(x = date, y = average_length, color = location)) +
  geom_point() +
  geom_smooth(method = "lm") +
  xlab("Month-Year") +
  ylab("Salmon Length (mm)") +
  theme_bw()

AIC(lm_temp, lm_year_temp)

## Predicting 20 years into the future 

future_years <- expand.grid(
  year = seq(max(joined_length_seas$year) + 1, max(joined_length_seas$year) + 20),
  month = 5:9
)

# Add average seasonal temperature for simplicity (replace with more accurate data if available)
avg_temp <- mean(joined_length_seas$seasonal_mean, na.rm = TRUE)
future_years$seasonal_mean <- avg_temp
future_years$year <- as.numeric(future_years$year)
future_years$date <- as.Date(paste(future_years$year, future_years$month, "01", sep = "-"))

# Predict future salmon lengths using the model
future_predictions <- predict(lm_year_temp, newdata = future_years, interval = "confidence")

# Combine future predictions with future years data
future_years$average_length <- future_predictions[, "fit"]

# Plot the future predictions
ggplot() +
  geom_point(data = joined_length_seas, aes(x = date, y = average_length, color = location)) +
  geom_smooth(data = joined_length_seas, aes(x = date, y = average_length, color = location), method = "lm") +
  geom_line(data = future_years, aes(x = date, y = average_length), color = "red") +
  xlab("Year") +
  ylab("Salmon Length (mm)") +
  theme_bw()

