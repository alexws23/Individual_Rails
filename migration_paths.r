library(motus)
library(tidyverse)
library(DBI)
library(RSQLite)
library(sf)
library(rnaturalearth)
library(paletteer)
library(viridis)

### Set Environment and Working Directory ###

Sys.setenv(TZ = "UTC") #Ensure that the system environment's timezone is set to UTC
setwd("C:/Users/awsmilor/Git/Ward Lab/Individual_Rails/Data") #Set your working directory
getwd()

### Bring the MOTUS data into R and clean it ###

#Bring the MOTUS data into R as an SQLite object
file.name <- dbConnect(SQLite(), "./project-314.motus")

#Create a virtual table from the "alltags" table in the SQLite file
tbl.alltags <- tbl(file.name, "alltags")

#Convert the table into a data frame
df.alltags <- tbl.alltags %>% 
  collect() %>% 
  as.data.frame() %>% 
  mutate(time = as_datetime(ts, 
                            tz = "UTC")) #Make sure your timezone is set to UTC or manually specify it

#Create a virtual table from the "tagDeps" table in the SQLite file
#This will be used to add the band number to the alltags dataframe later on
tbl.tagDeps <- tbl(file.name, "tagDeps")

#convert the table into a data frame
df.tagDeps <- tbl.tagDeps %>% 
  collect() %>% 
  as.data.frame() %>% 
  rename(tagDeployID = deployID) %>% #change the name of the "deployID" to "tagDeployID", which is the syntax used in df.alltags
  select(tagDeployID|tagID #Select only the variables needed to merge 
         # |bandNumber  #If your tagDeps dataframe already has the band numbers of your tagged bird, this is how you select it.
  ) # %>% 
#rename(Band_Number = bandNumber)

######Optional if your tagDeps DF doesn't include band number information######
##If the tagDeps dataframe doesn't have data in the bandNumber column, you will need to get that information from elsewhere.
##For the rail data, I had a separate CSV that had the Motus tag ID (called tagID in the "tagDeps" df) and the band numbers.
##If this is the case for you, follow the next steps.

band_numbers <- read.csv("rails.csv") %>% 
  select(!(X:X.4)) %>% select(!(Code | Species)) %>% # #if your CSV has any columns you want to remove, you can do so with these functions.
  mutate(Band_Number = gsub("-","", Band_Number)) %>% #If the band numbers include a "-" you can remove that with the gsub function
  rename(tagID = Motus.ID) #Change the name of the motus tag ID column to "tagID" if needed

######
#Join the band_numbers data frame to the tagDeps dataframe by the "tagID"
df.tagDeps <- df.tagDeps %>% 
  left_join(band_numbers, by = "tagID")

#Create two new variables, one with the alpha code of your study species and one with the year the tag was deployed
alltags <- df.alltags %>% 
  filter(speciesEN %in% c("Sora", "Virginia Rail")) %>% #If your want to only include certain species from your project you can filter with this.
  mutate(alpha_code = speciesEN) %>% #create a new column for alpha codes that is a duplicate of the species name column
  mutate(alpha_code = replace(alpha_code, alpha_code == "Virginia Rail", "VIRA")) %>% #rename each of the species in the alpha code column to their code
  mutate(alpha_code = replace(alpha_code, alpha_code == "Sora", "SORA")) %>% 
  mutate(start_year = year(tagDeployStart)) #create a column with the year the tag was deployed

#Join the alltags dataframe to the tagDeps dataframe to add band number data
"%ni%" <- Negate("%in%")

alltags_unfiltered <- alltags %>% 
  left_join(df.tagDeps, by = "tagDeployID") %>% 
  filter(tagDepComments %ni% "TEST TAG ACTIVATED 9/7, GIVEN TO MIKE AVARA 9/14") #if your dataset includes test tags, filter it out

### Filtering the Data ###
#filter the data to only include data where the motusFilter equals 1
alltags_motus_filter <- alltags_unfiltered %>% 
  filter(motusFilter == 1)

#remove all rows where the receiver info is missing
alltags_na_receivers <- alltags_motus_filter %>%
  drop_na(recvDeployLat | recvDeployName)

############# Prepping data for visualization and analysis
alltags_ordered <- mutate(alltags_na_receivers, recvDeployName = reorder(recvDeployName,recvDeployLat))

##set parameters for latitude > 0 to remove some false detections
alltags_ordered <- subset(alltags_ordered, recvDeployLat>-5)
alltags_ordered <- subset(alltags_ordered, recvDeployLon< -52)
alltags_cleaned <- subset(alltags_ordered, recvDeployLon> -140)


alltags_corrected <- alltags_cleaned %>% 
  mutate(tag_time = as_datetime(tagDeployStart)) %>% 
  mutate(date = date(time)) %>% 
  mutate(time_cst = with_tz(time, "US/Central")) %>% 
  mutate(date_cst = date(time_cst))

## Remove df and tbls no longer in use
rm(list = c("tbl.tagDeps", "tbl.alltags", "file.name", "df.alltags","df.tagDeps","band_numbers", "alltags"))

###### Towers of Concern #####
bad_towers <- c("Missisquoi Bay NWR", #tower in Vermont
  #lots of towers up in the Nova Scotia area picked up our tags. Seems highly unlikely.
  "Kent Island", "Allison (Johnston Point II)", "Johnstons Point", "Borden-Carleton",
  "Selma2", "East Walton", "Cape Jourimain", "Brule Point", "Big Island", "Baie Verte2", 
  "Beaubassin", "Lookoff", "Blandford", "Tantramar School", "Hopewell2", "Sonora", 
  "Bois de la Roche 2", "Linden2", "West Quoddy", "Taylor Head", "Kingsburg", "GMNP-Berry Hill",
  "Kejimkujik National Park", "Bois de la Roche 1", "Goose Bay", "Pugwash", "Grande-Ile ",
  "Bridgewater 2", "Waterside", "Chebogue Point ", "Cabot Beach Provincial Park", "Shag Harbour","Truro",
  "Grève de Tadoussac", "Cap-Gaspe",
  "Golfo de Santa Clara - RV Park", #Tower on the coast of the Gulf of California
  "Ensenada - Estero Beach Hotel", #Tower on the pacific coast of Baja
  "Stump Lake", #Tower in BC
  #Whole bunch of towers in Jamaica got a couple of our birds. Some plausible, but feels unlikely
  "IRI Departure", "LII K.5-7.5", "LII K-8", "LII J.5-7.5", "LII J.5-8.5",
  "LII J-8", "LII I.5-7.5", "MM Departure", "MM B.5-2.5", "LII I-8",
  "MM B.5-3.5", "MM A.5-3.5", "LII H.5-7.5", "MM A.5-4.5", "Test-14DD",
  "LII I.5-8.5", "MM B.5-4.5", "LI Departure",
  "Finca Cristina", #Tower in Panama
  "Finca El Triunfo", #Tower in Colombia
  #Whole Bunch of towers in New Hampshire
  "Weir6", "Gauge House", "Weather Station", "Mid Plot", "West End", "High Plot", "Kineo ",
  "Alaksen", "Scout Island Nature Centre", #Tower in BC
  "Kent Farm Research Station", #Tower in Indiana
  "Montna Farms", #Tower in Central California
  "Allerton", "Kennekuk 2", "Kennekuk 6",
  "Reserva de la Biosfera Ria Celestun, CONANP field ", #Tower in Yucatan
  "Kent Island", "Goose Bay", "McGill_Bird_Observatory", "Senneville Farm", "Glen Rouge Camprgound Parking Lot Loop", "Beloeil",
  "GA_OSS_DOCK", "Harris Neck NWR, GA", #Towers in GA
  "Parramore Island", #Tower in MD
  "AVNJ", #Tower in NJ
  "Shelburne Farms",
  "Triton2", "Lambs Gap Tower",
  "Sunset Bench-MT", "North Floodplain-MT" #Towers in western Montana
)

###### NEED TO REVIEW 73234!!!! ###########
###### SORA 73270 SHOWS ELLIPTICAL MIGRATION IN FALL. Goes from ND to atlantic coast. Seems legit
###### SORA 77501 SHOWS THE SAME THING!! ARE EASTERN DETECTIONS LEGIT?? ######

#Maybe "McGill_Bird_Observatory", "Beloeil", "Senneville Farm" #Towers in Montreal
#Maybe "Scotch_Bush" #Tower near Ottowa
#Maybe "Monocliff", "Walsingham Super Tower", "Glen Rouge Camprgound Parking Lot Loop" #Tower near Toronto
#Maybe Montna Farms #Tower in Central California
#Maybe "SAVA03" #Tower near the Chesapeake bay
#Maybe "Sessions Woods" #Tower in Connecticut
#Maybe "Reserva de la Biosfera Ria Celestun, CONANP field" #Tower in Yucatan
#Maybe "Birds Canada HQ" #In Ontario
#Maybe "FortWhyte Alive" #Tower near Winnipeg
#Maybe "Triton2" #Tower in Quebec
#Maybe "Sunset Bench-MT", "North Floodplain-MT", #Towers in western Montana
#Maybe  "High Knob Fire Tower", "Summit Fire Tower", "PARC Banding", "Yeany's Maple", "PARC Binkey", "Binkey", "PARC Honey Hut", "Lambs Gap Tower" #Towers in Pennsylvania
#Maybe "Newtowne Neck State Park, Compton, MD", "CHIN", "Patuxent River Park", "Bluestem Farm",
#"Nassawango Fire Tower", "Lamb's Knoll" #Towers in MD
#Maybe "␀CHDE" #Tower in Delaware
#Maybe "Cape Romain NWR, SC (Bulls Island)", "North Intet - Winyah Bay NERR", "Fort Moultrie" #Towers in SC
#Maybe "Shelburne Farms"
#"Parramore Island", #Tower in MD
#"JBNY", #Tower in NY
#"ASRI_Bristol", #Tower in MA
#"AVNJ", #Tower in NJ
#"GA_OSS_DOCK", "Harris Neck NWR, GA", #Towers in GA
#"WELL", #tower in MA
#"MASH", #tower in the Hamptons
#"Maywood Environmental Park" #Tower in WI

#### Creating plots of all detections, minus the bad towers

only_good_tower <- alltags_corrected %>% 
  filter(recvDeployName %ni% bad_towers) %>% 
  mutate(year = as.character(year(time_cst))) %>% 
  mutate(season = as.character(month(time_cst))) %>% #Create a new column for the season the bird was tagged during
  mutate(season = replace(season, season %in% 3:6, "Spring")) %>% #Set all months between March and June as spring
  mutate(season = replace(season, season %in% 8:11, "Fall")) %>% #Set all months between August and November as Fall
  mutate(season = replace(season, season %in% c(1,2,12), "Winter")) %>% #Set all months between December and February as Winter
  mutate(season = replace(season, season %in% 7, "Summer")) %>% #Set all months between July and August as Summer
  mutate(SeasonYear = paste(season, year, sep = " "))    #create a new column joining the season and year columns

# Summarize by bird and receiver site
site_summary <- only_good_tower %>%
  group_by(motusTagID, recvDeployName, recvDeployLon, recvDeployLat) %>%
  summarise(
    first_det = min(time_cst, na.rm = TRUE),
    last_det  = max(time_cst, na.rm = TRUE),
    duration_days = as.numeric(difftime(last_det, first_det, units = "days")),
    .groups = "drop"
  )

unique = unique(only_good_tower$motusTagID)


######## TEST LOOP
for(i in unique) {
  
  df_tmp <- only_good_tower %>%
    filter(motusTagID == i) %>%
    arrange(time_cst)
  
  df_summary <- site_summary %>%
    filter(motusTagID == i)
  
  species <- df_tmp %>%
    distinct(speciesEN) %>%
    pull()
  
  # Optional: remove specific problem tower for this ID
  if (i == 63993) {
    df_tmp <- df_tmp %>%
      filter(recvDeployName != "Cape Romain NWR, SC (Bulls Island)")
    df_summary <- df_summary %>%
      filter(recvDeployName != "Cape Romain NWR, SC (Bulls Island)")
  }
  
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
      color = "Last detection",
      size = "Duration (days)"
    ) +
    
    # Bird path
    geom_path(
      data = df_tmp,
      aes(x = recvDeployLon, y = recvDeployLat, group = SeasonYear),
      linewidth = 0.8
    ) +
    
    # Detection sites: color = recency, size = duration
    geom_point(
      data = df_summary,
      aes(x = recvDeployLon, y = recvDeployLat,
          color = last_det,
          size = duration_days),
      alpha = 0.9
    ) +
    
    # Tag deployment point
    geom_point(
      data = df_tmp,
      aes(x = tagDepLon, y = tagDepLat),
      colour = "red", shape = 4, size = 2
    ) +
    
    scale_color_viridis_c(option = "plasma", direction = -1) +
    scale_size_continuous(range = c(1, 6)) +
    theme(legend.position = "right")
  
  ggsave(plot2, file = paste0("Detection_maps_good_towers/good_towers_map_", i, ".png"), 
         width = 14, height = 10, units = "cm", 
         create.dir = TRUE,
         path = "C:/Users/awsmilor/Git/Ward Lab/Individual_Rails/Imgs")
}


#For-loop to create plots
for(i in unique){
  #Object for species name
  species <- only_good_tower %>% 
    filter(motusTagID == i) %>% 
    select(speciesEN) %>% 
    distinct()
  #Create plot
  plot <- ggplot(data = filter(only_good_tower, 
                               motusTagID == i), 
                 aes(x = time_cst, y = recvDeployName)) +
    theme_bw() + 
    geom_point() + 
    labs(x = "Time of year", y = "Receiver name (ordered by latitude)", subtitle = species)
  
  ggsave(plot, file=paste0("Detection_plots_good_towers/good_towers_plot_", i,".png"), width = 14, height = 10, units = "cm",
         path = "C:/Users/awsmilor/Git/Ward Lab/Individual_Rails/Imgs")
}

#### Creating maps of all detections, minus the bad towers
###load in important objects for map visuals, should only need to do once if you use the same working directory
world <- ne_countries(scale = "medium", returnclass = "sf") 
lakes <- ne_download(scale = "medium", type = 'lakes', category = 'physical',
                     returnclass = "sf")

###code for adding state/country boundaries
usmap <- ne_states(country = c("United States of America","Canada"), returnclass = 'sf')
eastern <- filter(usmap, region %in% c("Midwest","South","Northeast")) 

# set limits to map based on locations of detections, ensuring they include the
# deployment locations

xmin <- min(only_good_tower$recvDeployLon, na.rm = TRUE) - 2
xmax <- max(only_good_tower$recvDeployLon, na.rm = TRUE) + 2
ymin <- min(only_good_tower$recvDeployLat, na.rm = TRUE) - 1
ymax <- max(only_good_tower$recvDeployLat, na.rm = TRUE) + 1

###for rails with filtered detections
for(i in unique){
  df_tmp <- only_good_tower %>%
    filter(motusTagID %in% c(i)) %>%
    arrange(time_cst)  %>%
    as.data.frame()
  
  #Object for species name
  species <- only_good_tower %>% 
    filter(motusTagID == i) %>% 
    select(speciesEN) %>% 
    distinct()
  
  if(i == 63993) {df_tmp <- df_tmp %>% filter(recvDeployName %ni% "Cape Romain NWR, SC (Bulls Island)") #Filter out this detection, which conflicts with more likely detections
  }
  
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
    geom_path(
      data = df_tmp,
      aes(x = recvDeployLon, y = recvDeployLat, 
          group = SeasonYear),
      linewidth = 0.8
    ) +
    
    # Detection points
    geom_point(
      data = df_tmp,
      aes(x = recvDeployLon, y = recvDeployLat, group = motusTagID, colour = as.factor(+date_cst)),
      size = 1.8
    )  +
    
    # Tag deployment point
    geom_point(
      data = df_tmp,
      aes(x = tagDepLon, y = tagDepLat),
      colour = "red", shape = 4, size = 2
    ) +
    
    scale_colour_viridis_d()
  
  ggsave(plot2, file=paste0("Detection_maps_good_towers/good_towers_map_", i,".png"), 
         width = 14, height = 10, units = "cm", 
         create.dir = TRUE,
         path = "C:/Users/awsmilor/Git/Ward Lab/Individual_Rails/Imgs")
  
}

### Making plots without "bad towers" removed. Only applying MOTUS filter and removing NA towers###
############# Prepping data for visualization and analysis
unfilter_ordered <- mutate(alltags_na_receivers, recvDeployName = reorder(recvDeployName,recvDeployLat))

##set parameters for latitude > 0 to remove some false detections
unfilter_ordered <- subset(unfilter_ordered, recvDeployLat>-5)
unfilter_ordered <- subset(unfilter_ordered, recvDeployLon< -52)
unfilter_cleaned <- subset(unfilter_ordered, recvDeployLon> -140)


unfilter_corrected <- unfilter_cleaned %>% 
  mutate(tag_time = as_datetime(tagDeployStart)) %>% 
  mutate(date = date(time)) %>% 
  mutate(time_cst = with_tz(time, "US/Central")) %>% 
  mutate(date_cst = date(time_cst))

#### Creating plots of all detections

all_tower <- unfilter_corrected %>% 
  mutate(year = as.character(year(time_cst))) %>% 
  mutate(season = as.character(month(time_cst))) %>% #Create a new column for the season the bird was tagged during
  mutate(season = replace(season, season %in% 3:6, "Spring")) %>% #Set all months between March and June as spring
  mutate(season = replace(season, season %in% 8:11, "Fall")) %>% #Set all months between August and November as Fall
  mutate(season = replace(season, season %in% c(1,2,12), "Winter")) %>% #Set all months between December and February as Winter
  mutate(season = replace(season, season %in% 7, "Summer")) %>% #Set all months between July and August as Summer
  mutate(SeasonYear = paste(season, year, sep = " "))    #create a new column joining the season and year columns

unique2 = unique(all_tower$motusTagID)

#For-loop to create plots
for(i in unique2){
  #Object for species name
  species <- all_tower %>% 
    filter(motusTagID == i) %>% 
    select(speciesEN) %>% 
    distinct()
  #Create plot
  plot <- ggplot(data = filter(all_tower, 
                               motusTagID == i), 
                 aes(x = time_cst, y = recvDeployName)) +
    theme_bw() + 
    geom_point() + 
    labs(x = "Time of year", y = "Receiver name (ordered by latitude)", subtitle = species)
  
  ggsave(plot, file=paste0("Detection_plots_all_towers/all_towers_plot_", i,".png"), width = 14, height = 10, units = "cm",
         path = "C:/Users/awsmilor/Git/Ward Lab/Individual_Rails/Imgs")
}

#### Creating maps of all detections, minus the bad towers
###load in important objects for map visuals, should only need to do once if you use the same working directory
world <- ne_countries(scale = "medium", returnclass = "sf") 
lakes <- ne_download(scale = "medium", type = 'lakes', category = 'physical',
                     returnclass = "sf")

###code for adding state/country boundaries
usmap <- ne_states(country = c("United States of America","Canada"), returnclass = 'sf')
eastern <- filter(usmap, region %in% c("Midwest","South","Northeast")) 

# set limits to map based on locations of detections, ensuring they include the
# deployment locations

xmin <- min(all_tower$recvDeployLon, na.rm = TRUE) - 2
xmax <- max(all_tower$recvDeployLon, na.rm = TRUE) + 2
ymin <- min(all_tower$recvDeployLat, na.rm = TRUE) - 1
ymax <- max(all_tower$recvDeployLat, na.rm = TRUE) + 1

###for rails with filtered detections
for(i in unique2){
  df_tmp <- all_tower %>%
    filter(motusTagID %in% c(i)) %>%
    arrange(time_cst)  %>%
    as.data.frame()
  
  #Object for species name
  species <- all_tower %>% 
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
    geom_path(
      data = df_tmp,
      aes(x = recvDeployLon, y = recvDeployLat, 
          group = SeasonYear),
      linewidth = 0.8
    ) +
    
    # Detection points
    geom_point(
      data = df_tmp,
      aes(x = recvDeployLon, y = recvDeployLat, group = motusTagID, colour = as.factor(SeasonYear)),
      size = 1.8
    )  +
    
    # Tag deployment point
    geom_point(
      data = df_tmp,
      aes(x = tagDepLon, y = tagDepLat),
      colour = "red", shape = 4, size = 2
    ) +
    
    scale_colour_paletteer_c(`"grDevices::terrain.colors"`)
  
  ggsave(plot2, file=paste0("Detection_maps_all_towers/all_towers_map_", i,".png"), 
         width = 14, height = 10, units = "cm", 
         create.dir = TRUE,
         path = "C:/Users/awsmilor/Git/Ward Lab/Individual_Rails/Imgs")
  
}

#### Making the Map for fall migration ###

mig_prep <- alltags_corrected %>% 
  filter(recvDeployName %ni% bad_towers) %>% 
  mutate(year = as.character(year(time_cst))) %>% 
  mutate(season = as.character(month(time_cst))) %>% #Create a new column for the season the bird was tagged during
  mutate(season = replace(season, season %in% 3:6, "Spring")) %>% #Set all months between March and June as spring
  mutate(season = replace(season, season %in% 8:11, "Fall")) %>% #Set all months between August and November as Fall
  mutate(season = replace(season, season %in% c(1,2,12), "Winter")) %>% #Set all months between December and February as Winter
  mutate(season = replace(season, season %in% 7, "Summer")) %>% #Set all months between July and August as Summer
  mutate(SeasonYear = paste(season, year, sep = " ")) %>%  #create a new column joining the season and year columns
  filter(
    !(motusTagID == 63993 & recvDeployName == "Cape Romain NWR, SC (Bulls Island)"),
    !(motusTagID == 77508 & recvDeployName == "Split Rock")
  )

######################### Spring SORA
# Filter data first
filtered_data <- mig_prep %>% 
  filter(SeasonYear %in% c("Spring 2021",
    "Spring 2022" 
    ,"Spring 2023"
    )) %>% 
  filter(speciesEN == "Sora") %>% 
  group_by(interaction(motusTagID, SeasonYear)) %>% 
  filter(n_distinct(recvDeployID) > 1) %>%
  arrange(motusTagID, time_cst) %>% # ensure points are in time order for path drawing
  filter(motusTagID %ni% c(73240, 73234,57167,57283)) %>% 
  mutate(
    motusTagID = as.factor(motusTagID),
    is_first = if_else(row_number() == 1, TRUE, FALSE))

n_distinct(filtered_data$motusTagID)

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
    aes(x = recvDeployLon, y = recvDeployLat, 
        group = interaction(motusTagID, SeasonYear), 
        colour = SeasonYear),
    linewidth = 0.8
  ) +
  
  geom_point(
    data = filtered_data %>% filter(is_first),
    aes(x = recvDeployLon, y = recvDeployLat, 
        colour = SeasonYear
        ),
    shape = 17,   # triangle up
    size = 3)+
  
  # Detection points
  geom_point(
    data = filtered_data,
    aes(x = recvDeployLon, y = recvDeployLat, group = motusTagID, colour = as.factor(SeasonYear)),
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
  scale_colour_paletteer_d("ltc::trio2",name = "Year") +
  theme_bw() +
  labs(
    x = "",
    y = "",
    title = "SORA Tracks by Individual motusTagID (Spring)"
  ) +
  theme(
    legend.position = "right",
    panel.grid.major = element_line(colour = "gray90"),
    panel.grid.minor = element_blank()
  )

print(plot3)

ggsave(plot3, file=paste0("Spring_Sora.png"), 
       width = 25, height = 20, units = "cm", 
       create.dir = TRUE,
       path = "C:/Users/awsmilor/Git/Ward Lab/Individual_Rails/Imgs")

######################## Fall SORA
# Filter data first
filtered_data <- mig_prep %>% 
  filter(SeasonYear %in% c("Fall 2021",
                           "Fall 2022" 
                           ,"Fall 2023"
  )) %>% 
  filter(speciesEN == "Sora") %>%
  filter(
    !(motusTagID == 53894 & recvDeployName == "Chad"),
    !(motusTagID == 57153 & recvDeployName == "Swan Bay"),
    recvDeployName != "Birds Canada HQ"
  ) %>% 
  group_by(interaction(motusTagID, SeasonYear)) %>% 
  filter(n_distinct(recvDeployID) > 1) %>%
  arrange(motusTagID, time_cst) %>% # ensure points are in time order for path drawing
  filter(motusTagID %ni% c(73240, 73234,57167,57283)) %>% 
  mutate(
    motusTagID = as.factor(motusTagID),
    is_first = if_else(row_number() == 1, TRUE, FALSE))

n_distinct(filtered_data$motusTagID)


# set limits to map based on locations of detections, ensuring they include the
# deployment locations

xmin <- min(filtered_data$recvDeployLon, na.rm = TRUE) - 2
xmax <- max(filtered_data$recvDeployLon, na.rm = TRUE) + 2
ymin <- min(filtered_data$recvDeployLat, na.rm = TRUE) - 1
ymax <- max(filtered_data$recvDeployLat, na.rm = TRUE) + 1

# Plot
plot4 <- ggplot() +
  # Base layers
  geom_sf(data = world, fill = "gray95", colour = NA) +
  geom_sf(data = lakes, fill = "white", colour = NA) +
  geom_sf(data = usmap, fill = "gray90", colour = "gray80", linewidth = 0.2) +
  
  # Tracks
  geom_path(
    data = filtered_data,
    aes(x = recvDeployLon, y = recvDeployLat, 
        group = interaction(motusTagID, SeasonYear), 
        colour = SeasonYear),
    linewidth = 0.8
  ) +
  
  geom_point(
    data = filtered_data %>% filter(is_first),
    aes(x = recvDeployLon, y = recvDeployLat, 
        colour = SeasonYear
    ),
    shape = 17,   # triangle up
    size = 3)+
  
  # Detection points
  geom_point(
    data = filtered_data,
    aes(x = recvDeployLon, y = recvDeployLat, group = motusTagID, colour = as.factor(SeasonYear)),
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
  scale_colour_paletteer_d("ltc::trio2",name = "Year") +
  theme_bw() +
  labs(
    x = "",
    y = "",
    title = "SORA Tracks by Individual motusTagID (Fall)"
  ) +
  theme(
    legend.position = "right",
    panel.grid.major = element_line(colour = "gray90"),
    panel.grid.minor = element_blank()
  )

print(plot4)

ggsave(plot4, file=paste0("Fall_Sora.png"), 
       width = 25, height = 20, units = "cm", 
       create.dir = TRUE,
       path = "C:/Users/awsmilor/Git/Ward Lab/Individual_Rails/Imgs")

######################## Fall VIRA
# Filter data first
filtered_data <- mig_prep %>% 
  filter(SeasonYear %in% c("Fall 2021",
                           "Fall 2022" 
                           ,"Fall 2023"
  )) %>% 
  filter(speciesEN == "Virginia Rail") %>% 
  filter(
    !(motusTagID == 53105 & recvDeployName == "SAVA03"), # Tag was reported going northward. Might have been picked up at SAVA but not then in Ontario
    !(motusTagID == 57174 & recvDeployName == "Florida Panther NWR, FL"), #Didn't make sense given other detections
    !(motusTagID == 63994 & recvDeployName == "Patuxent River Park") #Maybe possible, but seemed improbable
    ) %>% 
  group_by(interaction(motusTagID, SeasonYear)) %>% 
  filter(n_distinct(recvDeployID) > 1) %>%
  arrange(motusTagID, time_cst) %>% # ensure points are in time order for path drawing
  filter(motusTagID %ni% c(73240, 73234,57167,57283)) %>% 
  mutate(
    motusTagID = as.factor(motusTagID),
    is_first = if_else(row_number() == 1, TRUE, FALSE))

n_distinct(filtered_data$motusTagID)


# set limits to map based on locations of detections, ensuring they include the
# deployment locations

xmin <- min(filtered_data$recvDeployLon, na.rm = TRUE) - 2
xmax <- max(filtered_data$recvDeployLon, na.rm = TRUE) + 2
ymin <- min(filtered_data$recvDeployLat, na.rm = TRUE) - 1
ymax <- max(filtered_data$recvDeployLat, na.rm = TRUE) + 1

# Plot
plot5 <- ggplot() +
  # Base layers
  geom_sf(data = world, fill = "gray95", colour = NA) +
  geom_sf(data = lakes, fill = "white", colour = NA) +
  geom_sf(data = usmap, fill = "gray90", colour = "gray80", linewidth = 0.2) +
  
  # Tracks
  geom_path(
    data = filtered_data,
    aes(x = recvDeployLon, y = recvDeployLat, 
        group = interaction(motusTagID, SeasonYear), 
        colour = SeasonYear),
    linewidth = 0.8
  ) +
  
  geom_point(
    data = filtered_data %>% filter(is_first),
    aes(x = recvDeployLon, y = recvDeployLat, 
        colour = SeasonYear
    ),
    shape = 17,   # triangle up
    size = 3)+
  
  # Detection points
  geom_point(
    data = filtered_data,
    aes(x = recvDeployLon, y = recvDeployLat, group = motusTagID, colour = as.factor(SeasonYear)),
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
  scale_colour_paletteer_d("ltc::trio2",name = "Year") +
  theme_bw() +
  labs(
    x = "",
    y = "",
    title = "VIRA Tracks by Individual motusTagID (Fall)"
  ) +
  theme(
    legend.position = "right",
    panel.grid.major = element_line(colour = "gray90"),
    panel.grid.minor = element_blank()
  )

print(plot5)

ggsave(plot5, file=paste0("Fall_Vira.png"), 
       width = 25, height = 20, units = "cm", 
       create.dir = TRUE,
       path = "C:/Users/awsmilor/Git/Ward Lab/Individual_Rails/Imgs")

######################### Spring VIRA
# Filter data first
filtered_data <- mig_prep %>% 
  filter(SeasonYear %in% c("Spring 2021",
                           "Spring 2022" 
                           ,"Spring 2023"
  )) %>% 
  filter(speciesEN == "Virginia Rail") %>% 
  group_by(interaction(motusTagID, SeasonYear)) %>% 
  filter(n_distinct(recvDeployID) > 1) %>%
  arrange(motusTagID, time_cst) %>% # ensure points are in time order for path drawing
  filter(motusTagID %ni% c(73240, 73234,57167,57283)) %>% 
  mutate(
    motusTagID = as.factor(motusTagID),
    is_first = if_else(row_number() == 1, TRUE, FALSE))

n_distinct(filtered_data$motusTagID)

# set limits to map based on locations of detections, ensuring they include the
# deployment locations

xmin <- min(filtered_data$recvDeployLon, na.rm = TRUE) - 2
xmax <- max(filtered_data$recvDeployLon, na.rm = TRUE) + 2
ymin <- min(filtered_data$recvDeployLat, na.rm = TRUE) - 1
ymax <- max(filtered_data$recvDeployLat, na.rm = TRUE) + 1

# Plot
plot6 <- ggplot() +
  # Base layers
  geom_sf(data = world, fill = "gray95", colour = NA) +
  geom_sf(data = lakes, fill = "white", colour = NA) +
  geom_sf(data = usmap, fill = "gray90", colour = "gray80", linewidth = 0.2) +
  
  # Tracks
  geom_path(
    data = filtered_data,
    aes(x = recvDeployLon, y = recvDeployLat, 
        group = interaction(motusTagID, SeasonYear), 
        colour = SeasonYear),
    linewidth = 0.8
  ) +
  
  geom_point(
    data = filtered_data %>% filter(is_first),
    aes(x = recvDeployLon, y = recvDeployLat, 
        colour = SeasonYear
    ),
    shape = 17,   # triangle up
    size = 3)+
  
  # Detection points
  geom_point(
    data = filtered_data,
    aes(x = recvDeployLon, y = recvDeployLat, group = motusTagID, colour = as.factor(SeasonYear)),
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
  scale_colour_paletteer_d("ltc::trio2",name = "Year") +
  theme_bw() +
  labs(
    x = "",
    y = "",
    title = "VIRA Tracks by Individual motusTagID (Spring)"
  ) +
  theme(
    legend.position = "right",
    panel.grid.major = element_line(colour = "gray90"),
    panel.grid.minor = element_blank()
  )

print(plot6)

ggsave(plot6, file=paste0("Spring_Vira.png"), 
       width = 25, height = 20, units = "cm", 
       create.dir = TRUE,
       path = "C:/Users/awsmilor/Git/Ward Lab/Individual_Rails/Imgs")
