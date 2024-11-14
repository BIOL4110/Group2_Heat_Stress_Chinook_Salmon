library(readr)
library(tidyverse)
library(data.table)
library(dplyr)
library(ggplot2)

juvenile_df <- read_csv("Cleaned Data/Juvenile_Data.csv")

seasonal_df <- read_csv("Cleaned Data/Filtered_Seasonal_Temps.csv")

str(seasonal_df)

##building joinable dataframes
seasonal_by_year <- seasonal_df %>% 
  mutate(year = format(yearmonth, "%Y")) %>% 
  group_by(station, year) %>% 
  summarize(annual_seasonal = mean(seasonal_mean, na.rm = TRUE))

write_csv(seasonal_by_year, "Seasonal_by_Year.csv")

juvenile_df_join <- juvenile_df %>% 
  rename(year = `Juvenile Year`) %>% 
  rename(juvenile_productivity = 'Total J/S') %>% 
  subset(select = c("year", "juvenile_productivity"))

##joining seasonal and juvenile
str(seasonal_by_year)
##data types needs to match
seasonal_by_year <- as_tibble(seasonal_by_year)
seasonal_by_year$year <- as.numeric(seasonal_by_year$year)

str(juvenile_df_join)

joined_df <- full_join(juvenile_df_join, seasonal_by_year, by = "year")

joined_df_ordered <- joined_df %>% 
  select("station", "year", "annual_seasonal", "juvenile_productivity")

write_csv(joined_df_ordered, "Cleaned Data/JS_Temperature_Joined.csv")

#running analysis
model1 <-lm(juvenile_productivity ~ annual_seasonal, data = joined_df_ordered)

summary(model1)


joined_df_ordered %>% 
  ggplot(aes(annual_seasonal, juvenile_productivity))+
  geom_point()+
  geom_smooth(method = 'lm')

##RESULTS? 
#R^2 indicates a weak influence
#p value insignificant
# seasonal temperatures from 2010-2017 not yet high enough to impact juvenile productivity

model2 <-lm(juvenile_productivity ~ annual_seasonal+year, data = joined_df_ordered)

summary(model2)


model3<-lm(juvenile_productivity ~ year, data = joined_df_ordered)

summary(model3)

joined_df_ordered %>% 
  ggplot(aes(year, juvenile_productivity))+
  geom_point()+
  geom_smooth(method = 'lm')

model4 <-lm(juvenile_productivity ~ station, data = joined_df_ordered)

summary(model4)

model5 <-lm(juvenile_productivity ~ station+year+annual_seasonal, data = joined_df_ordered)

summary(model5)

#project into future?
#future research: how do current temps influence


#suggestions from laney
##Plotting all variables: scatterplot matrix
pairs(joined_df_ordered[,2:4])
