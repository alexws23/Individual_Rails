library(tidyverse)

### Set Environment and Working Directory ###

Sys.setenv(TZ = "UTC") #Ensure that the system environment's timezone is set to UTC
setwd("C:/Users/awsmilor/Git/Ward Lab/Individual_Rails/Data") #Set your working directory
getwd()

#Read in CSV with stopover duration data
stopover_data <- readxl::read_xlsx(path = "./Stopover_Duration.xlsx")

#Cleaning and prepping the data for statistical analysis
stopover_cleaned <- stopover_data %>% 
  filter(`Departure Data Quality` == "x") %>% #filter out any data where the departure date couldn't be determined
  mutate(Min.Stopover.Duration =as.numeric(Min.Stopover.Duration)) %>%  #convert the Min.Stopover.Duration to be a numeric rather than character class
  mutate(Band_Number = gsub("-","", Band_Number)) %>% 
  select(!`Departure Data Quality` & !Min.Stopover.Duration)


