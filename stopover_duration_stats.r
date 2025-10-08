library(tidyverse)
library(ggthemes)
library(flextable)

### Set Environment and Working Directory ###

Sys.setenv(TZ = "UTC") #Ensure that the system environment's timezone is set to UTC
setwd("C:/Users/awsmilor/Git/Ward Lab/Individual_Rails/Data") #Set your working directory
getwd()

#Read in CSV with stopover duration data
stopover_data <- readxl::read_xlsx(path = "./Stopover_Duration.xlsx")

#Cleaning and prepping the data for statistical analysis
stopover_cleaned <- stopover_data %>% 
  filter(`Departure Data Quality` == "x") %>% #filter out any data where the departure date couldn't be determined
  mutate(season = as.character(month(Date_Tagged))) %>% #Create a new column for the season the bird was tagged during
  mutate(season = replace(season, season %in% 3:6, "Spring")) %>% #Set all months between March and June as spring
  mutate(season = replace(season, season %in% 8:11, "Fall")) %>% #Set all months between August and November as Fall
  mutate(SeasonYear = paste(Year, season, sep = " ")) %>% #create a new column joining the season and year columns
  mutate(Min.Stopover.Duration =as.numeric(Min.Stopover.Duration)) #convert the Min.Stopover.Duration to be a numeric rather than character class

#Make a horizontal bar chart with the data grouped by season (Spring, Fall) and species
season_plot <- stopover_cleaned %>% 
  group_by(season, Species) %>% #group by both Season and Species to preserve both variables in summary
  summarise(avg_stop = mean(Min.Stopover.Duration), #Calculate mean stopover duration
            CI_low = 0,#avg_stop - 2 * sd(Min.Stopover.Duration), #set lower bounds of CI as 0
            CI_high = avg_stop + 2 * sd(Min.Stopover.Duration)) %>% #Set upper bounds of CI as the average plus 2 times the standard deviation for a 95% CI
  ungroup() %>% 
  ggplot(aes(x = avg_stop, y = season)) + #Set the universal aesthetics for x & y
    geom_col(aes(fill = Species), #make fill color different based on species
             width = .6, #Narrow the width of the bar
             position = position_dodge(reverse = TRUE)) + #Dodge the position so the bars aren't on top of one another
    geom_errorbar(aes(xmin = CI_low, xmax = CI_high, color = Species), #set aesthetics for the error bars
                  position = position_dodge(reverse = TRUE), #Dodge the position so the lines aren't on top of one another
                  width = .6) + #Narrow the width of the line
    theme_minimal() + #Set the theme
    labs(x = "Average Minimum Stopover Duration (Days)", y = "Season Tagged") + #Add labels
    scale_fill_manual(values = c("SORA" = "gray10", "VIRA" = "gray70")) + #manually set fill colors
    scale_color_manual(values = c("SORA" = "gray10", "VIRA" = "gray70")) + #manually set line colors
    theme(panel.grid = element_blank())

print(season_plot)

ggsave("avg_duration_season.png", plot = season_plot, path = "C:/Users/awsmilor/Git/Ward Lab/Individual_Rails/Imgs")

#Make a horizontal bar chart with the data grouped by Year and species
year_plot <- stopover_cleaned %>% 
  mutate(Year = as.character(Year)) %>% 
  group_by(Year, Species) %>% #group by both Year and Species to preserve both variables in summary
  summarise(avg_stop = mean(Min.Stopover.Duration), #Calculate mean stopover duration
            CI_low = 0,#avg_stop - 2 * sd(Min.Stopover.Duration), #set lower bounds of CI as 0
            CI_high = avg_stop + 2 * sd(Min.Stopover.Duration)) %>% #Set upper bounds of CI as the average plus 2 times the standard deviation for a 95% CI
  ungroup() %>% 
  ggplot(aes(x = avg_stop, y = Year)) + #Set the universal aesthetics for x & y
  geom_col(aes(fill = Species), #make fill color different based on species
           width = .6, #Narrow the width of the bar
           position = position_dodge(reverse = TRUE)) + #Dodge the position so the bars aren't on top of one another
  geom_errorbar(aes(xmin = CI_low, xmax = CI_high, color = Species), #set aesthetics for the error bars
                position = position_dodge(reverse = TRUE), #Dodge the position so the lines aren't on top of one another
                width = .6) + #Narrow the width of the line
  theme_minimal() + #Set the theme
  labs(x = "Average Minimum Stopover Duration (Days)", y = "Year Tagged") + #Add labels
  scale_fill_manual(values = c("SORA" = "gray10", "VIRA" = "gray70")) + #manually set fill colors
  scale_color_manual(values = c("SORA" = "gray10", "VIRA" = "gray70")) + #manually set line colors
  theme(panel.grid = element_blank())

print(year_plot)

ggsave("avg_duration_year.png", plot = year_plot, path = "C:/Users/awsmilor/Git/Ward Lab/Individual_Rails/Imgs")

#Make a horizontal bar chart with the data grouped by season, year and species
season_year_plot <- stopover_cleaned %>% 
  group_by(SeasonYear, Species) %>% #group by both Year and Species to preserve both variables in summary
  summarise(avg_stop = mean(Min.Stopover.Duration), #Calculate mean stopover duration
            CI_low = 0,#avg_stop - 2 * sd(Min.Stopover.Duration), #set lower bounds of CI as 0
            CI_high = avg_stop + 2 * sd(Min.Stopover.Duration)) %>% #Set upper bounds of CI as the average plus 2 times the standard deviation for a 95% CI
  ungroup() %>% 
  ggplot(aes(x = avg_stop, y = SeasonYear)) + #Set the universal aesthetics for x & y
  geom_col(aes(fill = Species), #make fill color different based on species
           width = .6, #Narrow the width of the bar
           position = position_dodge(reverse = TRUE)) + #Dodge the position so the bars aren't on top of one another
  geom_errorbar(aes(xmin = CI_low, xmax = CI_high, color = Species), #set aesthetics for the error bars
                position = position_dodge(reverse = TRUE), #Dodge the position so the lines aren't on top of one another
                width = .6) + #Narrow the width of the line
  theme_minimal() + #Set the theme
  labs(x = "Average Minimum Stopover Duration (Days)", y = "Season Tagged") + #Add labels
  scale_fill_manual(values = c("SORA" = "gray10", "VIRA" = "gray70")) + #manually set fill colors
  scale_color_manual(values = c("SORA" = "gray10", "VIRA" = "gray70")) + #manually set line colors
  theme(panel.grid = element_blank())

print(season_year_plot)

ggsave("avg_duration_season_year.png", plot = season_year_plot, path = "C:/Users/awsmilor/Git/Ward Lab/Individual_Rails/Imgs")


####Summary statistics
#Statistics for each species across all seasons
stats_species <- stopover_cleaned %>% 
  group_by(Species) %>% 
  summarise(avg = mean(Min.Stopover.Duration), 
            min = min(Min.Stopover.Duration),
            max = max(Min.Stopover.Duration),
            n = n()
            )

print(stats_species)

#Statistics for each season regardless of species
stats_season <- stopover_cleaned %>% 
  group_by(season, Year) %>% 
  summarise(avg = mean(Min.Stopover.Duration), 
            min = min(Min.Stopover.Duration),
            max = max(Min.Stopover.Duration),
            n = n()
  )

print(stats_season)

#Statistics for each season broken up by species
stats_all <- stopover_cleaned %>% 
  group_by(season, Year, Species) %>% 
  summarise(avg = mean(Min.Stopover.Duration), 
            sd = sd(Min.Stopover.Duration),
            min = min(Min.Stopover.Duration),
            max = max(Min.Stopover.Duration),
            n = n()
  )

print(stats_all)

write.csv(stats_all, file = "summary_stats_rails.csv")
