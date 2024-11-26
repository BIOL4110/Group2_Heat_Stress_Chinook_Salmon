# Load necessary libraries
library(ggplot2)
library(dplyr)
library(lubridate)
library(stringr)

# Convert 'yearmonth' to 'Year' and 'Month' in Filtered_Seasonal_Temps

# Convert 'yearmonth' to 'Year' and 'Month' using lubridate  
# Assuming 'yearmonth' is in the format "YYYY-MM-DD" 
Filtered_Seasonal_Temps <- Filtered_Seasonal_Temps %>% 
  mutate(Year = year(ymd(yearmonth)), Month = month(ymd(yearmonth)))

# Convert 'yearmonth' to 'Year' and 'Month' in Filtered_Seasonal_Temps
Filtered_Seasonal_Temps <- Filtered_Seasonal_Temps %>%
  mutate(
    Year = year(ymd(yearmonth)),
    Month = month(ymd(yearmonth))
  )

# Verify the 'Year' and 'Month' columns are correctly populated
print(head(Filtered_Seasonal_Temps))

# Ensure correct column names in the salmon length data
Average_Length_By_Year_month <- Average_Length_By_Year_month %>%
  rename(Year = year, Month = month)

# Convert 'Month' columns to numeric in both datasets to match Filtered_Seasonal_Temps
Unuk_Monthly_Mean_Lengths <- Unuk_Monthly_Mean_Lengths %>%
  mutate(Month = as.numeric(Month)) %>%
  rename(Length = mean_length)

Average_Length_By_Year_month <- Average_Length_By_Year_month %>%
  mutate(Month = as.numeric(Month)) %>%
  rename(Length = average_length)

# Merge temperature data with salmon length data for Region 1
joined_region1 <- inner_join(Unuk_Monthly_Mean_Lengths, Filtered_Seasonal_Temps, by = c("Year", "Month"))

## PLOT UNUK LENGTH WITH AVG TEMP ##

# Plot for Region 1
ggplot(joined_region1, aes(x = seasonal_mean, y = Length)) +
  geom_point(color = 'blue') +
  geom_smooth(method = "lm", color = 'blue') +
  ggtitle("Salmon Length in Region 1 Over Temperature") +
  xlab("Temperature (°C)") +
  ylab("Mean Salmon Length (mm)") +
  theme_bw()

## PLOT KODIAK WITH AVG TEMP ##

# Similarly, adjust for Region 2
joined_region2 <- inner_join(Average_Length_By_Year_month, Filtered_Seasonal_Temps, by = c("Year", "Month"))

# Plot for Region 2
ggplot(joined_region2, aes(x = seasonal_mean, y = Length)) +
  geom_point(color = 'green') +
  geom_smooth(method = "lm", color = 'green') +
  ggtitle("Salmon Length in Region 2 Over Temperature") +
  xlab("Temperature (°C)") +
  ylab("Average Salmon Length (mm)") +
  theme_bw()

## PLOT COMBINED WITH AVG TEMP

# Combine datasets for overall comparison
combined_salmon <- bind_rows(
  joined_region1 %>% mutate(Region = "Unuk"),
  joined_region2 %>% mutate(Region = "Kodiak")
)

# Plot combined regions
ggplot(combined_salmon, aes(x = seasonal_mean, y = Length, color = Region)) +
  geom_point() +
  geom_smooth(method = "lm") +
  ggtitle("Salmon Length in Both Regions Over Temperature") +
  xlab("Temperature (°C)") +
  ylab("Salmon Length (mm)") +
  theme_bw()

## Analysis seasonal
quantify <- lm(Length~seasonal_mean, data = combined_salmon)
summary(quantify)

## Assumptions / Test for Normality
res1 <- resid(quantify)
plot(fitted(quantify), res1)
abline(0,0)
qqnorm(res1)
qqline(res1)
plot(density(res1))

## Mostly Normal Distribution, but big tails

## Decreasing over time ## By Year

# Filter data for months May to September (5 to 9)
Filtered_Seasonal_Temps <- Filtered_Seasonal_Temps %>%
  filter(Month >= 5 & Month <= 9)

Unuk_Monthly_Mean_Lengths <- Unuk_Monthly_Mean_Lengths %>%
  filter(Month >= 5 & Month <= 9)

Average_Length_By_Year_month <- Average_Length_By_Year_month %>%
  filter(Month >= 5 & Month <= 9)

# Merge temperature data with salmon length data for Region 1
joined_region1_2 <- inner_join(Unuk_Monthly_Mean_Lengths, Filtered_Seasonal_Temps, by = c("Year", "Month"))

# Merge temperature data with salmon length data for Region 2
joined_region2_2 <- inner_join(Average_Length_By_Year_month, Filtered_Seasonal_Temps, by = c("Year", "Month"))

# Combine datasets for overall comparison
combined_salmon_2 <- bind_rows(
  joined_region1_2 %>% mutate(Region = "Region 1"),
  joined_region2_2 %>% mutate(Region = "Region 2")
)

# Verify the combined data
print(head(combined_salmon))

# Plot salmon length over time for filtered months
ggplot(combined_salmon, aes(x = Year + (Month - 1) / 12, y = Length, color = Region)) +
  geom_point() +
  geom_smooth(method = "lm") +
  ggtitle("Salmon Length Over Time (May to September)") +
  xlab("Year") +
  ylab("Salmon Length (mm)") +
  theme_bw()

## Analysis seasonal and year
quantify2 <- lm(Length~seasonal_mean + Year, data = combined_salmon_2)
summary(quantify2)

## Assumptions / Test for Normality
res2 <- resid(quantify2)
plot(fitted(quantify2), res2)
abline(0,0)
qqnorm(res2)
qqline(res2)
plot(density(res2))

## Also Normal Enough Distribution, significant P value --> Better model (R squared value)

## Analysis - Just year
quantify3 <- lm(Length~Year, data = combined_salmon_2)
summary(quantify3)

## Assumptions / Test for Normality
res3 <- resid(quantify3)
plot(fitted(quantify3), res3)
abline(0,0)
qqnorm(res3)
qqline(res3)
plot(density(res3))

## Also Normal Enough Distribution, significant P value

## Next steps - Predicting trend over next 20 years?
## How decrease in length impacts salmon


## Predicting 20 years into the future ##

# Create a sequence of future years and months (May to September)
future_years <- expand.grid(
  Year = seq(max(combined_salmon$Year) + 1, max(combined_salmon$Year) + 20),
  Month = 5:9
)

# Add average seasonal temperature for simplicity (replace with more accurate data if available)
avg_temp <- mean(combined_salmon$seasonal_mean, na.rm = TRUE)
future_years$seasonal_mean <- avg_temp

# Predict future salmon lengths using the model
future_predictions <- predict(quantify2, newdata = future_years, interval = "confidence")

# Combine future predictions with future years data
future_years$Length <- future_predictions[, "fit"]

# Plot the future predictions
ggplot() +
  geom_point(data = combined_salmon, aes(x = Year + (Month - 1) / 12, y = Length, color = Region)) +
  geom_smooth(data = combined_salmon, aes(x = Year + (Month - 1) / 12, y = Length, color = Region), method = "lm") +
  geom_line(data = future_years, aes(x = Year + (Month - 1) / 12, y = Length), color = "red") +
  ggtitle("Predicted Salmon Length Over Next 20 Years (May to September)") +
  xlab("Year") +
  ylab("Salmon Length (mm)") +
  theme_bw()