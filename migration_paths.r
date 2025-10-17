library(motus)
library(tidyverse)
library(DBI)
library(RSQLite)
library(sf)
library(rnaturalearth)
library(viridis)

### Set Environment and Working Directory ###

Sys.setenv(TZ = "UTC") #Ensure that the system environment's timezone is set to UTC
setwd("C:/Users/awsmilor/Git/Ward Lab/Individual_Rails/Data") #Set your working directory
getwd()

###### Towers of Concern #####
bad_towers <- c(#"Missisquoi Bay NWR", #tower in Vermont
  #lots of towers up in the Nova Scotia area picked up our tags. Seems highly unlikely.
  "Kent Island", "Allison (Johnston Point II)", "Johnstons Point", "Borden-Carleton",
  "Selma2", "East Walton", "Cape Jourimain", "Brule Point", "Big Island", "Baie Verte2", 
  "Beaubassin", "Lookoff", "Blandford", "Tantramar School", "Hopewell2", "Sonora", 
  "Bois de la Roche 1", "Linden2", "West Quoddy", "Taylor Head", "Kingsburg", "GMNP-Berry Hill",
  "Kejimkujik National Park", "Bois de la Roche 2", "Goose Bay", "Pugwash", "Grande-Ile",
  "Bridgewater2", "Waterside", "Chebogue Point", "Cabot Beach Provincial Park", "Shag Harbor","Truro",
  "Grève de Tadoussac",
  "Golfo de Santa Clara - RV Park", #Tower on the coast of the Gulf of California
  "Ensenada - Estero Beach Hotel", #Tower on the pacific coast of Baja
  "Stump Lake", #Tower in BC
  #Whole bunch of towers in Jamaica got a couple of our birds. Some plausible, but feels unlikely
  "IRI Departure", "LII K.5-7.5", "LII K-8", "LII J.5-7.5", "LII J.5-8.5",
  "LII J-8", "LII I.5-7.5", "MM Departure", "MM B.5-2.5", "LII I-8",
  "MM B.5-3.5", "MM A.5-3.5", "LII H.5-7.5", "MM A.5-4.5", "Test-14DD",
  "LII I.5-8.5", "MM B.5-4.5",
  "Finca Cristina", #Tower in Panama
  "Finca El Triunfo", #Tower in Colombia
  #Whole Bunch of towers in New Hampshire
  "Weir6", "Gauge House", "Weather Station", "Mid Plot", "West End", "High Plot", "Kineo",
  "Alaksen", "Scout Island Nature Centre", #Tower in BC
  "Kent Farm Research Station", #Tower in Indiana
  "Montna Farms", #Tower in Central California
  "Allerton", "Kennekuk 2", "Kennekuk 6",
  "Reserva de la Biosfera Ria Celestun, CONANP field ", #Tower in Yucatan
  "Kent Island", "Goose Bay", "McGill_Bird_Observatory", "Senneville Farm", "Glen Rouge Camprgound Parking Lot Loop",
  "Triton2", "High Knob Fire Tower", "Birds Canada HQ", "SAVA03", "Monocliff", "Scotch_Bush"
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

#### Making the Map for fall migration ###

fall_mig_prep <- alltags_corrected %>% 
  filter(recvDeployName %ni% bad_towers) %>% 
  mutate(year = as.character(year(time_cst))) %>% 
  mutate(season = as.character(month(time_cst))) %>% #Create a new column for the season the bird was tagged during
  mutate(season = replace(season, season %in% 3:6, "Spring")) %>% #Set all months between March and June as spring
  mutate(season = replace(season, season %in% 8:11, "Fall")) %>% #Set all months between August and November as Fall
  mutate(season = replace(season, season %in% c(1,2,12), "Winter")) %>% #Set all months between December and February as Winter
  mutate(season = replace(season, season %in% 7, "Summer")) %>% #Set all months between July and August as Summer
  mutate(SeasonYear = paste(season, year, sep = " ")) #create a new column joining the season and year columns

# Filter data first
filtered_data <- fall_mig_prep %>% 
  filter(SeasonYear %in% c("Spring 2021",
    "Spring 2022" 
    ,"Spring 2023"
    )) %>% 
  filter(speciesEN == "Sora") %>% 
  group_by(interaction(motusTagID, SeasonYear)) %>% 
  filter(n_distinct(recvDeployID) > 1) %>%
  arrange(motusTagID, time_cst) %>% # ensure points are in time order for path drawing
  filter(motusTagID %ni% c(73240, 73234, 65888)) %>% 
  mutate(
    motusTagID = as.factor(motusTagID),
    is_first = if_else(row_number() == 1, TRUE, FALSE))

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
  scale_color_manual(values = c("#3b528b", "#21918c", "#5ec962"), name = "Year") +
  theme_bw() +
  labs(
    x = "",
    y = "",
    title = "Tracks by Individual motusTagID (Fall)"
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