# Load the necessary library
library(dplyr)

# Read the CSV file into a data frame
data <- read.csv("ASL_formatted_SoutheastKodiak(2010 - 2017) - Chinook and Chum (2).csv")

# Print the first few rows of the data
head(data)

# Convert sampleDate to Date type
data$sampleDate <- as.Date(data$sampleDate, format = "%Y-%m-%d")

# Filter the data to include only Chinook salmon
chinook_data <- data %>%
  filter(Species == "chinook")

# Print the first few rows of the filtered data
head(chinook_data)

# Extract the year from sampleDate and add it as a new column
chinook_data <- chinook_data %>%
  mutate(year = format(sampleDate, "%Y"))

# Group the data by year and calculate the average length in mm
average_length_by_year <- chinook_data %>%
  group_by(year) %>%
  summarize(average_length = mean(Length..mm., na.rm = TRUE))

# View the summarized data
View(average_length_by_year)

# Install and load the necessary packages
install.packages("ggplot2")
library(ggplot2)

# Create a line plot for the average length by year
ggplot(average_length_by_year, aes(x = as.numeric(year), y = average_length)) +
  geom_line(color = "blue") +
  geom_point(color = "red") +
  labs(title = "Average Length of Chinook Salmon Over Time",
       x = "Year",
       y = "Average Length (mm)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Export the average length by year data to a new CSV file
write.csv(average_length_by_year, "Average_Length_By_Year.csv", row.names = FALSE)
