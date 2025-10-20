library(tidyverse)
library(climate)
library(maps)

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

#Get weather data for each departure date
ns = nearest_stations_ogimet(country = c("United States"),
                             point = c(-90, 40),
                             no_of_stations = 5, 
                             add_map = TRUE,
                             allow_failure = FALSE)
if (is.data.frame(ns)) {
  knitr::kable(head(ns, 5))
}

#nearest station: Peoria, Greater Peoria Regional Airport 72532
df = meteo_noaa_hourly(station = "725320-14842", year = 2021)
df = meteo_noaa_hourly(station = "010080-99999", year = 2016)
