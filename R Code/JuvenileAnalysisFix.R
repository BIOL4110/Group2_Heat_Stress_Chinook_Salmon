library(readr)
library(tidyverse)
library(data.table)
library(dplyr)
library(readxl)
library(ggplot2)
##Cleaning Juvenile Data

Chinook_Juvenile_Data <- read_excel("Raw Data/Chinook Juvenile Data.xlsx")

Juvenile_Data <- Chinook_Juvenile_Data %>% 
  select(`Juvenile Year`, `Total J/S`) %>% 
  filter(`Juvenile Year` > 2008 & `Juvenile Year` <= 2019)
##Making a new csv
write.csv(Juvenile_Data, "Cleaned Data/Juvenile_Data_2.csv")

## Cleaning temp data to fit juvenile data
juv_temp_df <- temp_df %>% 
  mutate(yearmonth = format(date, "%Y-%m")) 

juv_temp_df$yearmonth <- ym(juv_temp_df$yearmonth)

str(juv_temp_df)

filt_seasonal_juv <- juv_temp_df %>% 
  filter(year(yearmonth) >= 2009,
         year(yearmonth) <= 2019,
         month(yearmonth) >= 5,
         month(yearmonth) <= 9) %>% 
  group_by(yearmonth) %>% 
  summarize(seasonal_mean = mean(mean_temp_c, na.rm = TRUE))

write.csv(filt_seasonal_juv, "Cleaned Data/Filt_temp_for_juv.csv", row.names = FALSE)

##ensuring data loaded in 
juvenile_df <- read_csv("Cleaned Data/Juvenile_Data_2.csv")

Filt_temp_for_juv_df <- read_csv("Cleaned Data/Filt_temp_for_juv.csv")

##building joinable dataframes

seasonal_juv <- Filt_temp_for_juv_df %>% 
  mutate(year = format(yearmonth, "%Y")) %>% 
  group_by(year) %>% 
  summarize(annual_seasonal = mean(seasonal_mean, na.rm = TRUE))


write_csv(seasonal_juv, "Cleaned Data/Seasonal_Juv.csv")

juvenile_df_join <- juvenile_df %>% 
  rename(year = `Juvenile Year`) %>% 
  rename(juvenile_productivity = 'Total J/S') %>% 
  subset(select = c("year", "juvenile_productivity"))


seasonal_juv <- as_tibble(seasonal_juv)
seasonal_juv$year <- as.numeric(seasonal_juv$year)

joined_juv_seas<- inner_join(juvenile_df_join, seasonal_juv, by = "year")

joined_juv_seas <- joined_juv_seas %>%
  select("year", "annual_seasonal", "juvenile_productivity") 

##Running Analysis ##
#Juveniles/spawner vs temperature
library(minpack.lm) 
nls_lm_temp <- nlsLM(juvenile_productivity ~ a * annual_seasonal^2 + b * annual_seasonal + c, 
                     data = joined_juv_seas, 
                     start = list(a=0.1, b=0.1, c=0.1))
summary(nls_lm_temp)

##plotting
augment_temp <- augment(nls_lm_temp)

joined_juv_seas %>% 
  ggplot(aes(annual_seasonal, juvenile_productivity))+
  geom_point()+
  geom_line(aes(x = annual_seasonal, y = .fitted), data= augment_temp)+
  theme_bw()+
  ylab("Number of Juveniles per Spawner")+
  xlab("Seasonal Temperatures from 2010-2017")


#Juveniles/spawner vs year
nls_lm_year <- nlsLM(juvenile_productivity ~ a * year^2 + b * year + c, 
                     data = joined_juv_seas, 
                     start = list(a=0.1, b=0.1, c=0.1)) 

summary(nls_lm_year)
##plotting
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

##comparing effects of year vs effects of temperature
AIC(nls_lm_temp, nls_lm_year)
  #no significant difference, (temp = 83.5, year = 81.5)
