library(readr)
library(tidyverse)
library(data.table)
library(dplyr)

##Cleaning Data
Juvenile_Data <- Chinook_Juvenile_Data %>% 
  select(`Juvenile Year`, `Total J/S`) %>% 
  filter(`Juvenile Year` > 2009 & `Juvenile Year` < 2018)

##Making a new csv
  write.csv(Juvenile_Data, "Juvenile_Data.csv")

View(Juvenile_Data)  
  