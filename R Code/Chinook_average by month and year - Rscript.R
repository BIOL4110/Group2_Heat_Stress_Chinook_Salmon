# Load the necessary library
library(dplyr)

# Read the CSV file into a data frame
data <- read.csv("ASL_formatted_SoutheastKodiak(2010 - 2017) - Chinook and Chum (2).csv")

# Filter the data to include only Chinook salmon and select the desired columns
chinook_data <- data %>%
  filter(Species == "chinook") %>%
  select(c("sampleDate", "Length..mm.", "Species"))

# Convert sampleDate to Date type
chinook_data$sampleDate <- as.Date(chinook_data$sampleDate, format = "%Y-%m-%d")

# Extract the year and month from sampleDate and add them as new columns
chinook_data <- chinook_data %>%
  mutate(year = format(sampleDate, "%Y"),
         month = format(sampleDate, "%m"))

# Group the data by year and month and calculate the average length in mm
average_length_by_year_month <- chinook_data %>%
  group_by(year, month) %>%
  summarize(average_length = mean(Length..mm., na.rm = TRUE))

# View the summarized data
View(average_length_by_year_month)

# Export the average length by year data to a new CSV file
write.csv(average_length_by_year_month, "Average_Length_By_Year_month.csv", row.names = FALSE)

# Install and load the necessary packages
install.packages("ggplot2")
library(ggplot2)

# Create a line plot for the average length by year and month
ggplot(average_length_by_year_month, aes(x = as.Date(paste(year, month, "01", sep = "-")), y = average_length, group = year, color = year)) +
  geom_line() +
  geom_point() +
  labs(title = "Average Length of Chinook Salmon by Year and Month",
       x = "Date",
       y = "Average Length (mm)") +
  scale_x_date(date_labels = "%Y-%m", date_breaks = "1 year") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

