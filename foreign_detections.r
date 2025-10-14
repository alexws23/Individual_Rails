library(motus)
library(tidyverse)
library(DBI)
library(RSQLite)
library(sf)
library(rnaturalearth)
library(viridis)

#### This code assumes that you already have downloaded your data from MOTUS and filtered it. 
#### The "df" dataframe is your dataframe that has already been filtered

"%ni%" <- Negate("%in%") #Create the %ni% operator

alltags_ordered <- mutate(df, recvDeployName = reorder(recvDeployName,recvDeployLat))

##set parameters for latitudes and longitudes outside species ranges to remove some false detections
alltags_ordered <- subset(alltags_ordered, recvDeployLat>-5)
alltags_ordered <- subset(alltags_ordered, recvDeployLon< -52)
alltags_cleaned <- subset(alltags_ordered, recvDeployLon> -140)

#Create new DF with new columns that include time and date in CST
alltags_corrected <- alltags_cleaned %>% 
  mutate(tag_time = as_datetime(tagDeployStart)) %>% 
  mutate(date = date(time)) %>% 
  mutate(time_cst = with_tz(time, "US/Central")) %>% 
  mutate(date_cst = date(time_cst))


### Create a plot that shows all foreign detections with receivers ordered by latitude. ####

unique_vals = unique(alltags_corrected$motusTagID)

#For-loop to create plots
for(i in unique_vals){
  #Object for species name
  species <- alltags_corrected %>% 
    filter(motusTagID == i) %>% 
    select(speciesEN) %>% 
    distinct()
  #Create plot
  plot <- ggplot(data = filter(alltags_corrected, 
                               motusTagID == i), 
                 aes(x = time_cst, y = recvDeployName)) +
    theme_bw() + 
    geom_point() + 
    labs(x = "Time of year", y = "Receiver name (ordered by latitude)", subtitle = species)
  
  ggsave(plot, file=paste0("Motus_filter_plot_", i,".png"), width = 14, height = 10, units = "cm",
         path = "C:/Users/awsmilor/Git/Ward Lab/Individual_Rails/Imgs") #Set the directory you want to export to
}

### Create a map for each individual tag to help parse through false positives ###
###################
###load in important objects for map visuals, should only need to do once if you use the same working directory
world <- ne_countries(scale = "medium", returnclass = "sf") 
lakes <- ne_download(scale = "medium", type = 'lakes', category = 'physical',
                     returnclass = "sf")

###code for adding state/country boundaries
usmap <- ne_states(country = c("United States of America","Canada"), returnclass = 'sf')
eastern <- filter(usmap, region %in% c("Midwest","South","Northeast")) 

# set limits to map based on locations of detections, ensuring they include the
# deployment locations
df_cleaned <- alltags_corrected %>%
  mutate(year = as.character(year(time_cst))) %>% 
  mutate(season = as.character(month(time_cst))) %>% #Create a new column for the season the bird was tagged during
  mutate(season = replace(season, season %in% 3:6, "Spring")) %>% #Set all months between March and June as spring
  mutate(season = replace(season, season %in% 8:11, "Fall")) %>% #Set all months between August and November as Fall
  mutate(season = replace(season, season %in% c(1,2,12), "Winter")) %>% #Set all months between December and February as Winter
  mutate(season = replace(season, season %in% 7, "Summer")) %>% #Set all months between July and August as Summer
  mutate(SeasonYear = paste(season, year, sep = " ")) #create a new column joining the season and year columns

# set limits to map based on locations of detections, ensuring they include the
# deployment locations

xmin <- min(df_cleaned$recvDeployLon, na.rm = TRUE) - 2
xmax <- max(df_cleaned$recvDeployLon, na.rm = TRUE) + 2
ymin <- min(df_cleaned$recvDeployLat, na.rm = TRUE) - 1
ymax <- max(df_cleaned$recvDeployLat, na.rm = TRUE) + 1

###for rails with filtered detections
for(i in unique_vals13){
  df_tmp <- df_cleaned %>%
    filter(motusTagID %in% c(i)) %>%
    arrange(time_cst)  %>%
    as.data.frame()
  
  # Build segment data
  df_segments <- df_tmp %>%
    mutate(
      x_start = recvDeployLon,
      y_start = recvDeployLat,
      x_end   = lead(recvDeployLon),
      y_end   = lead(recvDeployLat),
      Year    = as.factor(year(time_cst))
    ) %>%
    filter(!is.na(x_end), !is.na(y_end))
  
  #Object for species name
  species <- df_cleaned %>% 
    filter(motusTagID == i) %>% 
    select(speciesEN) %>% 
    distinct()
  
  #Create Plot
  plot2 <- ggplot(data = world) +
    geom_sf(colour = NA) +
    geom_sf(data = lakes, colour = NA, fill = "white") +
    geom_sf(data = usmap, fill = "gray98") +
    coord_sf(xlim = c(xmin, xmax), ylim = c(ymin, ymax), expand = FALSE) +
    theme_bw() +
    labs(
      x = "",
      y = "",
      subtitle = species,
      colour = "Year"
    ) +
    geom_segment(
      data = df_segments,
      aes(x = x_start, y = y_start,
          xend = x_end, yend = y_end,
          #colour = SeasonYear,
          group = motusTagID)
    ) +
    geom_point(
      data = df_tmp,
      aes(x = recvDeployLon, y = recvDeployLat, colour = SeasonYear),
      shape = 16
    ) +
    geom_point(
      data = df_tmp,
      aes(x = tagDepLon, y = tagDepLat),
      colour = "red", shape = 4
    ) +
    scale_colour_discrete(palette = "viridis")
  
  ggsave(plot2, file=paste0("CleanMap_", i,".png"), 
         width = 14, height = 10, units = "cm", 
         create.dir = TRUE,
         path = "C:/Users/awsmilor/Git/Ward Lab/Individual_Rails/Imgs") #set the directory you want to save to
  
}

#################
### Create a list of bad receivers ###
bad_towers <- c("Bad Tower 1")

#####################################################
#### Pretty map ####
migration <- df_cleaned %>% 
  filter(recvDeployName %ni% bad_towers) %>% 

# Filter data first
filtered_data <- migration %>% 
  filter(SeasonYear %in% c("Fall 2023")) %>% #If you want to filter to only include certain seasons and/or years
  filter(speciesEN == "Sora") %>% #If you want to only include certain species
  group_by(motusTagID) %>% 
  filter(n_distinct(recvDeployID) > 1) %>%
  arrange(motusTagID, time_cst) %>% # ensure points are in time order for path drawing
  filter(motusTagID %ni% c(73240, 73234  )) %>%  #If you want to remove any tags altogether
  mutate(
    motusTagID = as.factor(motusTagID),
    is_first = if_else(row_number() == 1, TRUE, FALSE)) #Create a new column that denotes the starting point of the bird's journey

# set limits to map based on locations of detections, ensuring they include the
# deployment locations

xmin <- min(filtered_data$recvDeployLon, na.rm = TRUE) - 2
xmax <- max(filtered_data$recvDeployLon, na.rm = TRUE) + 2
ymin <- min(filtered_data$recvDeployLat, na.rm = TRUE) - 1
ymax <- max(filtered_data$recvDeployLat, na.rm = TRUE) + 1

# Plot
plot3 <- ggplot() +
  # Base layers
  geom_sf(data = world, fill = "gray95", colour = NA) +
  geom_sf(data = lakes, fill = "white", colour = NA) +
  geom_sf(data = usmap, fill = "gray90", colour = "gray80", linewidth = 0.2) +
  
  # Tracks
  geom_path(
    data = filtered_data,
    aes(x = recvDeployLon, y = recvDeployLat, group = motusTagID, colour = as.factor(motusTagID)),
    linewidth = 0.8
  ) +
  
  geom_point(
    data = filtered_data %>% filter(is_first),
    aes(x = recvDeployLon, y = recvDeployLat, colour = motusTagID),
    shape = 17,   # triangle up
    size = 3)+
  
  # Detection points
  geom_point(
    data = filtered_data,
    aes(x = recvDeployLon, y = recvDeployLat, group = motusTagID, colour = as.factor(motusTagID)),
    size = 1.8
  ) +
  
  # Tag deployment point
  geom_point(
    data = filtered_data,
    aes(x = tagDepLon, y = tagDepLat),
    colour = "red", shape = 4, size = 2
  ) +
  
  # Map settings
  coord_sf(xlim = c(xmin, xmax), ylim = c(ymin, ymax), expand = FALSE) +
  scale_colour_viridis_d(option = "plasma", name = "motusTagID") +
  theme_bw() +
  labs(
    x = "",
    y = "",
    title = "Tracks by Individual motusTagID"
  ) +
  theme(
    legend.position = "right",
    panel.grid.major = element_line(colour = "gray90"),
    panel.grid.minor = element_blank()
  )

print(plot3)

ggsave(plot3, file=paste0("Fall_2023_All.png"), 
       width = 25, height = 20, units = "cm", 
       create.dir = TRUE,
       path = "C:/Users/awsmilor/Git/Ward Lab/Individual_Rails/Imgs")