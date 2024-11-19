library(readr)
library(tidyverse)
library(data.table)
library(dplyr)
library(ggplot2)

##is there a sig increase in temp over study period
temp_mod <- lm(annual_seasonal ~ year, data = seasonal_by_year)
summary(temp_mod)

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


#project into future?
#future research: how do current temps influence


#suggestions from laney
##Plotting all variables: scatterplot matrix
pairs(joined_df_ordered[,2:4])
#don't include station

seasonal_by_year_fix <- seasonal_df %>% 
  mutate(year = format(yearmonth, "%Y")) %>% 
  group_by(year) %>% 
  summarize(annual_seasonal = mean(seasonal_mean, na.rm = TRUE)) %>% 
  subset(select = c("year", "annual_seasonal"))


seasonal_by_year_fix <- as_tibble(seasonal_by_year_fix)
seasonal_by_year_fix$year <- as.numeric(seasonal_by_year_fix$year)

joined_df_fix <- inner_join(juvenile_df_join, seasonal_by_year_fix, by = "year")

joined_fix <- joined_df_fix %>%
  select("year", "annual_seasonal", "juvenile_productivity") 

joined_fix$logtemp <- log(joined_fix$annual_seasonal)
joined_fix$logprod <- log(joined_fix$juvenile_productivity)

model6 <- lm(logprod ~ logtemp+year, data = joined_fix)

summary(model6)

model7 <-lm(logprod ~ logtemp, data = joined_fix)

summary(model7)
AIC(model1, model2, model3, model4, model5, model6, model7)

pairs(joined_fix[,2:5])

joined

joined_fix %>% 
  ggplot(aes(logtemp, logprod))+
  geom_point()+
  xlab("log(Yearly Seasonal Temperature)")+
  ylab("log(Juvenile Productivity")

joined_fix %>% 
  ggplot(aes(annual_seasonal, juvenile_productivity))+
  geom_point()+
  xlab("Year")+
  ylab("log(Juvenile Productivity")  

joined_fix %>% 
  ggplot(aes(year, logprod))+
  geom_point()+
  xlab("Year")+
  ylab("log(Juvenile Productivity")  

##assumptions checking
durbinWatsonTest(model7)
res1<-resid(model1)
#Produce residual vs. fitted plot.
plot(fitted(model1), res1)
abline(0,0)
##Produce a Q-Q plot.
qqnorm(res1)
qqline(res1) 
##Produce a density plot.
plot(density(res1))


