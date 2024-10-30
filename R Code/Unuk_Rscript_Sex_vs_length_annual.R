# Load the necessary library
library(readxl)

# Read the third sheet from the Excel file, skipping the first 4 rows
data <- read_excel("Unuk 1995 to 2017_Lewis (2).xlsx", sheet = 3, skip = 4)

# Load the necessary library
library(dplyr)

# Rename the columns
data <- data %>%
  rename(
    Date = ...3, # Assuming the Date column is in the second position
    Sex = ...10, # Assuming the Sex column is in the tenth position
    Length = 'LENGTH,'
  )

# Select only the desired columns
selected_data <- data %>%
  select(Date, Sex, Length)

# View the selected data
View(selected_data)

# Convert the Date column to numeric
data$Date <- as.numeric(data$Date)

# Convert the numeric Date column to Date type
data$Date <- as.Date(data$Date, origin = "1899-12-30") # Excel's date origin is 1899-12-30

# Select only the desired columns
selected_data <- data %>%
  select(Date, Sex, Length)

# View the selected data
View(selected_data)

# Extract the year from Date and add it as a new column
selected_data <- selected_data %>%
  mutate(Year = format(Date, "%Y"))

# Filter the data to include only the years 2010-2017
filtered_data <- selected_data %>%
  filter(Year >= 2010 & Year <= 2017)

# View the filtered data
View(filtered_data)

# Convert the Length column to numeric
filtered_data$Length <- as.numeric(filtered_data$Length)

# Check for any non-numeric values that might cause issues
summary(filtered_data$Length)

# Group the data by year and calculate the mean length
annual_mean_lengths <- filtered_data %>%
  group_by(Year) %>%
  summarize(mean_length = mean(Length, na.rm = TRUE))

# View the annual mean lengths
View(annual_mean_lengths)

# Export the annual mean lengths to a CSV file
write.csv(annual_mean_lengths, "Unuk_Annual_Mean_Lengths.csv", row.names = FALSE)

# Extract the month from Date and add it as a new column
filtered_data <- filtered_data %>%
  mutate(Month = format(Date, "%m"))

# Group the data by year and month and calculate the mean length
monthly_mean_lengths <- filtered_data %>%
  group_by(Year, Month) %>%
  summarize(mean_length = mean(Length, na.rm = TRUE))

# View the monthly mean lengths
View(monthly_mean_lengths)

# Export the monthly mean lengths to a CSV file
write.csv(monthly_mean_lengths, "Unuk_Monthly_Mean_Lengths.csv", row.names = FALSE)

# Install and load the necessary packages
library(ggplot2)

# Create a line plot for annual mean lengths
ggplot(annual_mean_lengths, aes(x = as.numeric(Year), y = mean_length)) +
  geom_line(color = "blue") +
  geom_point(color = "red") +
  labs(title = "Annual Mean Length of Chinook Salmon",
       x = "Year",
       y = "Mean Length (mm)") +
  theme_minimal()

# Create a line plot for monthly mean lengths
ggplot(monthly_mean_lengths, aes(x = as.Date(paste(Year, Month, "01", sep = "-")), y = mean_length, group = Year, color = Year)) +
  geom_line() +
  geom_point() +
  labs(title = "Monthly Mean Length of Chinook Salmon",
       x = "Date",
       y = "Mean Length (mm)") +
  scale_x_date(date_labels = "%Y-%m", date_breaks = "1 year") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

## Sex vs length ##

# Standardize the Sex column to ensure consistency
filtered_data <- filtered_data %>%
  mutate(Sex = toupper(Sex)) %>%
  filter(Sex %in% c("M", "F"))

# View the cleaned data
View(filtered_data)

# Group the data by year and sex, and calculate the mean length
annual_mean_lengths_by_sex <- filtered_data %>%
  group_by(Year, Sex) %>%
  summarize(mean_length = mean(Length, na.rm = TRUE))

# View the annual mean lengths by sex
View(annual_mean_lengths_by_sex)

# Create a line plot for annual mean lengths by sex
ggplot(annual_mean_lengths_by_sex, aes(x = as.numeric(Year), y = mean_length, color = Sex, group = Sex)) +
  geom_line() +
  geom_point() +
  labs(title = "Annual Mean Length of Chinook Salmon by Sex",
       x = "Year",
       y = "Mean Length (mm)") +
  theme_minimal()


