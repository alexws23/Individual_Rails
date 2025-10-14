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

#All of the data with a motusFilter of 0
df_block_0 <- tbl(sql_motus, "alltags") %>%
  filter(motusFilter == 0) %>%
  collect()

## Preliminary Data Checks ##
#Check receivers
alltags_motus_filter %>%
  filter(is.na(recvDeployLat) | is.na(recvDeployName))
  select(recvDeployLat, recvDeployLon, recvDeployName, recvDeployID, recv, 
         recvProjID, recvProjName)

#remove all rows where the receiver info is missing
alltags_na_receivers <- alltags_motus_filter %>%
  drop_na(recvDeployLat | recvDeployName)

## Summarizing tag Detections ##
df_summary <- alltags_na_receivers %>%
  filter(tagProjID == 314) %>%
  group_by(motusTagID, runID, recvDeployName, ambigID, 
           tagDepLon, tagDepLat, recvDeployLat, recvDeployLon, tagDeployID) %>%
  #summarizing by runID to get max run length and mean time stamp:
  summarize(max.runLen = max(runLen, na.rm = TRUE), 
            ts = mean(ts, na.rm = TRUE), .groups = "drop") %>% 
  arrange(motusTagID, ts) %>%
  mutate(time = as_datetime(ts))

df_summary %>% 
  slice_sample(n = 10) %>% 
  select(tagDeployID) %>% 
  distinct()

ggplot(data = filter(df_summary,
                     tagDeployID %in% c(45803, 38035, 45847, 28490, 38031, 28491)), 
       aes(x = time, y = recvDeployLat)) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) + 
  geom_point() + 
  geom_path() +
  facet_wrap(~ tagDeployID, scales = "free", ncol = 2) +
  scale_x_datetime(date_labels = "%Y-%m-%d")
 
## Ambiguous Tags ##
clarify(file.name)

ambigTags <- alltags_motus_filter %>%
  select(ambigID, motusTagID) %>%
  filter(!is.na(ambigID)) %>%
  distinct()

df_summary.ambig <- filter(df_summary, motusTagID %in% ambigTags$motusTagID) %>% 
  mutate(ambig = !is.na(ambigID)) # Ambiguous or not? TRUE/FALSE

# to put all ambiguous tags from the same project on the same plot together, we
# need to create a new 'ambig tag' variable we call 'newID' that includes the 
# multiple 'motusTagIDs' for each 'ambigID'

ambigTags2 <- alltags_motus_filter %>%
  select(ambigID, motusTagID) %>%
  filter(!is.na(ambigID)) %>%
  distinct() %>%
  group_by(ambigID) %>%
  summarize(newID = paste(unique(ambigID), toString(motusTagID), sep = ": ")) %>%
  left_join(ambigTags, by = "ambigID")

# and merge that with 'df_summary'

df_summary.ambig <- left_join(df_summary.ambig, ambigTags2, by = "motusTagID") %>% 
  arrange(time)

ggplot(data = df_summary.ambig, 
       aes(x = time, y = recvDeployLat, colour = ambig)) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) + 
  geom_point() + 
  geom_path() + 
  facet_wrap(~ newID, scales = "free", ncol = 2)


### Making departure graphs for fall rails ###
towers <- unique(alltags_na_receivers$recvDeployName)
view(towers)
departures <- alltags_unfiltered %>% 
  filter(recvDeployName %in% c("Chad", "Chautauqua", "Swan Bay", "Pumphouse",
                               "Dixon Waterfowl Refuge-North")) %>% 
  mutate(tag_time = as_datetime(tagDeployStart)) %>% 
  mutate(date = date(time)) %>% 
  mutate(time_cst = with_tz(time, "US/Central")) %>% 
  mutate(date_cst = date(time_cst))

### Creating departure plots for all birds tagged in the fall ###
fall_departures <- departures %>% 
  filter(month(tag_time) %in% 8:12) %>% 
  filter(month(date_cst) %in% 8:12)

setwd("C:/Users/awsmilor/Git/Ward Lab/Individual_Rails/Data")

unique_vals <- unique(fall_departures$motusTagID)

for (i in unique_vals) {
  
  # Filter data for this tag
  df_sub <- fall_departures %>%
    filter(motusTagID == i)
  
  # Identify the 6 most recent dates
  recent_dates <- df_sub %>%
    distinct(date_cst) %>%
    arrange(desc(date_cst)) %>%
    slice(1:6) %>%
    pull(date_cst)
  
  # Filter to only those dates
  df_sub <- df_sub %>%
    filter(date_cst %in% recent_dates)
  
  # Make the plot
  departure_plot <- ggplot(data = df_sub,
                           aes(x = time_cst, y = sig, colour = as.factor(port))) +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) + 
    geom_point() + 
    geom_smooth(method = "loess", se = FALSE) + 
    facet_wrap(date_cst ~ recvDeployName, scales = "free_x") +
    labs(title = paste(i))
  
  # Save the plot
  ggsave(departure_plot,
         filename = paste0("dep_plots_fall/departure_", i, ".png"),
         path = "C:/Users/awsmilor/Git/Ward Lab/Individual_Rails/Imgs",
         scale = 1,
         create.dir = TRUE)
}

### Creating Departure Plots for all birds tagged in the spring ###
spring_departures <- departures %>% 
  filter(month(tag_time) %in% 3:6) %>% 
  filter(month(date_cst) %in% 3:6)

unique_vals2 <- unique(spring_departures$motusTagID)

for (i in unique_vals2) {
  
  # Filter data for this tag
  df_sub <- spring_departures %>%
    filter(motusTagID == i)
  
  # Identify the 6 most recent dates
  recent_dates <- df_sub %>%
    distinct(date_cst) %>%
    arrange(desc(date_cst)) %>%
    slice(1:6) %>%
    pull(date_cst)
  
  # Filter to only those dates
  df_sub <- df_sub %>%
    filter(date_cst %in% recent_dates)
  
  # Make the plot
  departure_plot <- ggplot(data = df_sub,
                           aes(x = time_cst, y = sig, colour = as.factor(port))) +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) + 
    geom_point() + 
    geom_smooth(method = "loess", se = FALSE) + 
    facet_wrap(date_cst ~ recvDeployName, scales = "free_x") +
    labs(title = paste(i))
  
  # Save the plot
  ggsave(departure_plot,
         filename = paste0("dep_plots_spring/departure_", i, ".png"),
         path = "C:/Users/awsmilor/Git/Ward Lab/Individual_Rails/Imgs",
         scale = 1,
         create.dir = TRUE)
}

## Using filtered data
departures_filtered <- alltags_motus_filter %>% 
  filter(recvDeployName %in% c("Chad", "Chautauqua", "Swan Bay", "Pumphouse",
                               "Dixon Waterfowl Refuge-North")) %>% 
  mutate(tag_time = as_datetime(tagDeployStart)) %>% 
  mutate(date = date(time)) %>% 
  mutate(time_cst = with_tz(time, "US/Central")) %>% 
  mutate(date_cst = date(time_cst))

spring_departures_filtered <- departures_filtered %>% 
  filter(month(tag_time) %in% 3:6) %>% 
  filter(month(date_cst) %in% 3:6)

unique_vals3 <- unique(spring_departures_filtered$motusTagID)

for (i in unique_vals3) {
  
  # Filter data for this tag
  df_sub <- spring_departures_filtered %>%
    filter(motusTagID == i)
  
  # Identify the 6 most recent dates
  recent_dates <- df_sub %>%
    distinct(date_cst) %>%
    arrange(desc(date_cst)) %>%
    slice(1:6) %>%
    pull(date_cst)
  
  # Filter to only those dates
  df_sub <- df_sub %>%
    filter(date_cst %in% recent_dates)
  
  # Make the plot
  departure_plot <- ggplot(data = df_sub,
                           aes(x = time_cst, y = sig, colour = as.factor(port))) +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) + 
    geom_point() + 
    geom_smooth(method = "loess", se = FALSE) + 
    facet_wrap(date_cst ~ recvDeployName, scales = "free_x") +
    labs(title = paste(i))
  
  # Save the plot
  ggsave(departure_plot,
         filename = paste0("dep_plots_spring_filtered/departure_", i, ".png"),
         path = "C:/Users/awsmilor/Git/Ward Lab/Individual_Rails/Imgs",
         scale = 1,
         create.dir = TRUE)
}

###### For-loop for departure CSVs
######################
setwd("C:/Users/awsmilor/Git/Ward Lab/Individual_Rails/Data")
##Fall Departures
for (i in unique_vals) {
  
  # Filter data for this tag
  df_sub <- fall_departures %>%
    filter(motusTagID == i)  
  
  # Save the plot
  write.csv(df_sub,
         file = paste0("dep_data_fall_unfiltered/departure_", i, ".csv"))
}

##Spring Departures
for (i in unique_vals2) {
  
  # Filter data for this tag
  df_sub <- spring_departures %>%
    filter(motusTagID == i)  
  
  # Save the plot
  write.csv(df_sub,
            file = paste0("dep_data_spring_unfiltered/departure_", i, ".csv"))
}


tag_65919 <- departures %>% 
  filter(motusTagID == 65919) %>% 
  arrange(desc(time_cst))

############# for-loop for movement data
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

### For Spring 2021
spring_2021 <- alltags_corrected %>% 
  filter(year(tag_time) == 2021) %>% 
  filter(month(tag_time) %in% 3:6)

unique_vals4 = unique(spring_2021$motusTagID)

#For-loop to create plots
for(i in unique_vals4){
  #Object for species name
  species <- spring_2021 %>% 
    filter(motusTagID == i) %>% 
    select(speciesEN) %>% 
    distinct()
  #Create plot
  plot <- ggplot(data = filter(spring_2021, 
                               motusTagID == i), 
                 aes(x = time_cst, y = recvDeployName)) +
    theme_bw() + 
    geom_point() + 
    labs(x = "Time of year", y = "Receiver name (ordered by latitude)", subtitle = species)
  
  ggsave(plot, file=paste0("Spring_2021/Motus_filter_plot_", i,".png"), width = 14, height = 10, units = "cm",
         path = "C:/Users/awsmilor/Git/Ward Lab/Individual_Rails/Imgs")
}

#For Fall 2021
fall_2021 <- alltags_corrected %>% 
  filter(year(tag_time) == 2021) %>% 
  filter(month(tag_time) %in% 8:11)

unique_vals5 = unique(fall_2021$motusTagID)

#For-loop to create plots
for(i in unique_vals5){
  #Object for species name
  species <- fall_2021 %>% 
    filter(motusTagID == i) %>% 
    select(speciesEN) %>% 
    distinct()
  #Create plot
  plot <- ggplot(data = filter(fall_2021, 
                               motusTagID == i), 
                 aes(x = time_cst, y = recvDeployName)) +
    theme_bw() + 
    geom_point() + 
    labs(x = "Time of year", y = "Receiver name (ordered by latitude)", subtitle = species)
  
  ggsave(plot, file=paste0("Fall_2021/Motus_filter_plot_", i, ".png"), width = 14, height = 10, units = "cm",
         path = "C:/Users/awsmilor/Git/Ward Lab/Individual_Rails/Imgs")
}

#For Spring 2022
spring_2022 <- alltags_corrected %>% 
  filter(year(tag_time) == 2022) %>% 
  filter(month(tag_time) %in% 3:6)

unique_vals8 = unique(spring_2022$motusTagID)

#For-loop to create plots
for(i in unique_vals8){
  #Object for species name
  species <- spring_2022 %>% 
    filter(motusTagID == i) %>% 
    select(speciesEN) %>% 
    distinct()
  #Create plot
  plot <- ggplot(data = filter(spring_2022, 
                               motusTagID == i), 
                 aes(x = time_cst, y = recvDeployName)) +
    theme_bw() + 
    geom_point() + 
    labs(x = "Time of year", y = "Receiver name (ordered by latitude)", subtitle = species)
  
  ggsave(plot, file=paste0("Spring_2022/Motus_filter_plot_", i, ".png"), width = 14, height = 10, units = "cm",
         path = "C:/Users/awsmilor/Git/Ward Lab/Individual_Rails/Imgs")
}

#For Fall 2022
fall_2022 <- alltags_corrected %>% 
  filter(year(tag_time) == 2022) %>% 
  filter(month(tag_time) %in% 8:11)

unique_vals10 = unique(fall_2022$motusTagID)

#For-loop to create plots
for(i in unique_vals10){
  #Object for species name
  species <- fall_2022 %>% 
    filter(motusTagID == i) %>% 
    select(speciesEN) %>% 
    distinct()
  #Create plot
  plot <- ggplot(data = filter(fall_2022, 
                               motusTagID == i), 
                 aes(x = time_cst, y = recvDeployName)) +
    theme_bw() + 
    geom_point() + 
    labs(x = "Time of year", y = "Receiver name (ordered by latitude)", subtitle = species)
  
  ggsave(plot, file=paste0("Fall_2022/Motus_filter_plot_", i, ".png"), width = 14, height = 10, units = "cm",
         path = "C:/Users/awsmilor/Git/Ward Lab/Individual_Rails/Imgs")
}

#For Spring 2023
spring_2023 <- alltags_corrected %>% 
  filter(year(tag_time) == 2023) %>% 
  filter(month(tag_time) %in% 3:6)

unique_vals12 = unique(spring_2023$motusTagID)

#For-loop to create plots
for(i in unique_vals12){
  #Object for species name
  species <- spring_2023 %>% 
    filter(motusTagID == i) %>% 
    select(speciesEN) %>% 
    distinct()
  #Create plot
  plot <- ggplot(data = filter(spring_2023, 
                               motusTagID == i), 
                 aes(x = time_cst, y = recvDeployName)) +
    theme_bw() + 
    geom_point() + 
    labs(x = "Time of year", y = "Receiver name (ordered by latitude)", subtitle = species)
  
  ggsave(plot, file=paste0("Spring_2023/Motus_filter_plot_", i, ".png"), width = 14, height = 10, units = "cm",
         path = "C:/Users/awsmilor/Git/Ward Lab/Individual_Rails/Imgs")
}
###### Towers of Concern #####
bad_towers <- c(#"Missisquoi Bay NWR", #tower in Vermont
                #lots of towers up in the Nova Scotia area picked up our tags. Seems highly unlikely.
                "Kent Island", "Allison (Johnston Point II)", "Johnstons Point", "Borden-Carleton",
                "Selma2", "East Walton", "Cape Jourimain", "Brule Point", "Big Island", "Baie Verte2", 
                "Beaubassin", "Lookoff", "Blandford", "Tantramar School", "Hopewell2", "Sonora", 
                "Bois de la Roche 2", "Linden2", "West Quoddy", "Taylor Head", "Kingsburg", "GMNP-Berry Hill",
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
                "Kent Island", "Goose Bay", "McGill_Bird_Observatory", "Senneville Farm", "Glen Rouge Camprgound Parking Lot Loop"
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

####### Make for-loop for spatial map for spring 2021
###################
###load in important objects for map visuals, should only need to do once if you use the same working directory
world <- ne_countries(scale = "medium", returnclass = "sf") 
lakes <- ne_download(scale = "medium", type = 'lakes', category = 'physical',
                     returnclass = "sf")

###code for adding state/country boundaries
usmap <- ne_states(country = c("United States of America","Canada"), returnclass = 'sf')
eastern <- filter(usmap, region %in% c("Midwest","South","Northeast")) 

###determine what values you want to be pulled out for each run of the for-loop
unique_vals6 = unique(spring_2021$motusTagID)

# set limits to map based on locations of detections, ensuring they include the
# deployment locations

xmin <- min(spring_2021$recvDeployLon, na.rm = TRUE) - 2
xmax <- max(spring_2021$recvDeployLon, na.rm = TRUE) + 2
ymin <- min(spring_2021$recvDeployLat, na.rm = TRUE) - 1
ymax <- max(spring_2021$recvDeployLat, na.rm = TRUE) + 1

###for rails with filtered detections
for(i in unique_vals6){
  df_tmp <- spring_2021 %>%
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
  species <- spring_2021 %>% 
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
          colour = Year,
          group = motusTagID),
      arrow = arrow(type = "open", length = unit(0.25, "cm"))
    ) +
    geom_point(
      data = df_tmp,
      aes(x = recvDeployLon, y = recvDeployLat),
      shape = 16,
      colour = "black"
    ) +
    geom_point(
      data = df_tmp,
      aes(x = tagDepLon, y = tagDepLat),
      colour = "red", shape = 4
    ) +
    scale_colour_discrete()
  
    ggsave(plot2, file=paste0("Spring_2021_Filtered_Maps/FilterMap_", i,".png"), 
           width = 14, height = 10, units = "cm", 
           create.dir = TRUE,
           path = "C:/Users/awsmilor/Git/Ward Lab/Individual_Rails/Imgs")
    
}

####### Make for-loop for spatial map for Fall 2021
###################
###load in important objects for map visuals, should only need to do once if you use the same working directory
world <- ne_countries(scale = "medium", returnclass = "sf") 
lakes <- ne_download(scale = "medium", type = 'lakes', category = 'physical',
                     returnclass = "sf")

###code for adding state/country boundaries
usmap <- ne_states(country = c("United States of America","Canada"), returnclass = 'sf')
eastern <- filter(usmap, region %in% c("Midwest","South","Northeast")) 

###determine what values you want to be pulled out for each run of the for-loop
unique_vals7 = unique(fall_2021$motusTagID)

# set limits to map based on locations of detections, ensuring they include the
# deployment locations

xmin <- min(fall_2021$recvDeployLon, na.rm = TRUE) - 2
xmax <- max(fall_2021$recvDeployLon, na.rm = TRUE) + 2
ymin <- min(fall_2021$recvDeployLat, na.rm = TRUE) - 1
ymax <- max(fall_2021$recvDeployLat, na.rm = TRUE) + 1

###for rails with filtered detections
for(i in unique_vals7){
  df_tmp <- fall_2021 %>%
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
  species <- fall_2021 %>% 
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
          colour = Year,
          group = motusTagID),
      arrow = arrow(type = "open", length = unit(0.25, "cm"))
    ) +
    geom_point(
      data = df_tmp,
      aes(x = recvDeployLon, y = recvDeployLat),
      shape = 16,
      colour = "black"
    ) +
    geom_point(
      data = df_tmp,
      aes(x = tagDepLon, y = tagDepLat),
      colour = "red", shape = 4
    ) +
    scale_colour_discrete()
  
  ggsave(plot2, file=paste0("Fall_2021_Filtered_Maps/FilterMap_", i,".png"), 
         width = 14, height = 10, units = "cm", 
         create.dir = TRUE,
         path = "C:/Users/awsmilor/Git/Ward Lab/Individual_Rails/Imgs")
  
}

####### Make for-loop for spatial map for spring 2022
###################
###load in important objects for map visuals, should only need to do once if you use the same working directory
world <- ne_countries(scale = "medium", returnclass = "sf") 
lakes <- ne_download(scale = "medium", type = 'lakes', category = 'physical',
                     returnclass = "sf")

###code for adding state/country boundaries
usmap <- ne_states(country = c("United States of America","Canada"), returnclass = 'sf')
eastern <- filter(usmap, region %in% c("Midwest","South","Northeast")) 

###determine what values you want to be pulled out for each run of the for-loop
unique_vals9 = unique(spring_2022$motusTagID)

# set limits to map based on locations of detections, ensuring they include the
# deployment locations

xmin <- min(spring_2022$recvDeployLon, na.rm = TRUE) - 2
xmax <- max(spring_2022$recvDeployLon, na.rm = TRUE) + 2
ymin <- min(spring_2022$recvDeployLat, na.rm = TRUE) - 1
ymax <- max(spring_2022$recvDeployLat, na.rm = TRUE) + 1

###for rails with filtered detections
for(i in unique_vals9){
  df_tmp <- spring_2022 %>%
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
  species <- spring_2022 %>% 
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
          colour = Year,
          group = motusTagID),
      arrow = arrow(type = "open", length = unit(0.25, "cm"))
    ) +
    geom_point(
      data = df_tmp,
      aes(x = recvDeployLon, y = recvDeployLat),
      shape = 16,
      colour = "black"
    ) +
    geom_point(
      data = df_tmp,
      aes(x = tagDepLon, y = tagDepLat),
      colour = "red", shape = 4
    ) +
    scale_colour_discrete()
  
  ggsave(plot2, file=paste0("Spring_2022_Filtered_Maps/FilterMap_", i,".png"), 
         width = 14, height = 10, units = "cm", 
         create.dir = TRUE,
         path = "C:/Users/awsmilor/Git/Ward Lab/Individual_Rails/Imgs")
  
}

### Create maps for Fall 2022
unique_vals11 = unique(fall_2022$motusTagID)

# set limits to map based on locations of detections, ensuring they include the
# deployment locations

xmin <- min(fall_2022$recvDeployLon, na.rm = TRUE) - 2
xmax <- max(fall_2022$recvDeployLon, na.rm = TRUE) + 2
ymin <- min(fall_2022$recvDeployLat, na.rm = TRUE) - 1
ymax <- max(fall_2022$recvDeployLat, na.rm = TRUE) + 1

###for rails with filtered detections
for(i in unique_vals11){
  df_tmp <- fall_2022 %>%
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
  species <- fall_2022 %>% 
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
          colour = Year,
          group = motusTagID),
      arrow = arrow(type = "open", length = unit(0.25, "cm"))
    ) +
    geom_point(
      data = df_tmp,
      aes(x = recvDeployLon, y = recvDeployLat),
      shape = 16,
      colour = "black"
    ) +
    geom_point(
      data = df_tmp,
      aes(x = tagDepLon, y = tagDepLat),
      colour = "red", shape = 4
    ) +
    scale_colour_discrete()
  
  ggsave(plot2, file=paste0("Fall_2022_Filtered_Maps/FilterMap_", i,".png"), 
         width = 14, height = 10, units = "cm", 
         create.dir = TRUE,
         path = "C:/Users/awsmilor/Git/Ward Lab/Individual_Rails/Imgs")
  
}

unique_vals12 = unique(spring_2023$motusTagID)

# set limits to map based on locations of detections, ensuring they include the
# deployment locations

xmin <- min(spring_2023$recvDeployLon, na.rm = TRUE) - 2
xmax <- max(spring_2023$recvDeployLon, na.rm = TRUE) + 2
ymin <- min(spring_2023$recvDeployLat, na.rm = TRUE) - 1
ymax <- max(spring_2023$recvDeployLat, na.rm = TRUE) + 1

###for rails with filtered detections
for(i in unique_vals12){
  df_tmp <- spring_2023 %>%
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
  species <- spring_2023 %>% 
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
          colour = Year,
          group = motusTagID),
      arrow = arrow(type = "open", length = unit(0.25, "cm"))
    ) +
    geom_point(
      data = df_tmp,
      aes(x = recvDeployLon, y = recvDeployLat),
      shape = 16,
      colour = "black"
    ) +
    geom_point(
      data = df_tmp,
      aes(x = tagDepLon, y = tagDepLat),
      colour = "red", shape = 4
    ) +
    scale_colour_discrete()
  
  ggsave(plot2, file=paste0("Spring_2023_Filtered_Maps/FilterMap_", i,".png"), 
         width = 14, height = 10, units = "cm", 
         create.dir = TRUE,
         path = "C:/Users/awsmilor/Git/Ward Lab/Individual_Rails/Imgs")
  
}


###############################################################################
#### Create plots that have potentially bad towers removed ####
spring_2023_cleaned <- spring_2023 %>% 
  filter(recvDeployName %ni% bad_towers) %>% 
  mutate(year = as.character(year(time_cst))) %>% 
  mutate(season = as.character(month(time_cst))) %>% #Create a new column for the season the bird was tagged during
  mutate(season = replace(season, season %in% 3:6, "Spring")) %>% #Set all months between March and June as spring
  mutate(season = replace(season, season %in% 8:11, "Fall")) %>% #Set all months between August and November as Fall
  mutate(season = replace(season, season %in% c(1,2,12), "Winter")) %>% #Set all months between December and February as Winter
  mutate(season = replace(season, season %in% 7, "Summer")) %>% #Set all months between July and August as Summer
  mutate(SeasonYear = paste(season, year, sep = " ")) #create a new column joining the season and year columns

unique_vals13 = unique(spring_2023_cleaned$motusTagID)

# set limits to map based on locations of detections, ensuring they include the
# deployment locations

xmin <- min(spring_2023_cleaned$recvDeployLon, na.rm = TRUE) - 2
xmax <- max(spring_2023_cleaned$recvDeployLon, na.rm = TRUE) + 2
ymin <- min(spring_2023_cleaned$recvDeployLat, na.rm = TRUE) - 1
ymax <- max(spring_2023_cleaned$recvDeployLat, na.rm = TRUE) + 1

###for rails with filtered detections
for(i in unique_vals13){
  df_tmp <- spring_2023_cleaned %>%
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
  species <- spring_2023_cleaned %>% 
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
      shape = 16,
      #colour = "black"
    ) +
    geom_point(
      data = df_tmp,
      aes(x = tagDepLon, y = tagDepLat),
      colour = "red", shape = 4
    ) +
    scale_colour_discrete(palette = "viridis")
  
  ggsave(plot2, file=paste0("Spring_2023_Cleaned_Maps/CleanMap_", i,".png"), 
         width = 14, height = 10, units = "cm", 
         create.dir = TRUE,
         path = "C:/Users/awsmilor/Git/Ward Lab/Individual_Rails/Imgs")
  
}

### For Spring 2022 ###
spring_2022_cleaned <- spring_2022 %>% 
  filter(recvDeployName %ni% bad_towers) %>% 
  mutate(year = as.character(year(time_cst))) %>% 
  mutate(season = as.character(month(time_cst))) %>% #Create a new column for the season the bird was tagged during
  mutate(season = replace(season, season %in% 3:6, "Spring")) %>% #Set all months between March and June as spring
  mutate(season = replace(season, season %in% 8:11, "Fall")) %>% #Set all months between August and November as Fall
  mutate(season = replace(season, season %in% c(1,2,12), "Winter")) %>% #Set all months between December and February as Winter
  mutate(season = replace(season, season %in% 7, "Summer")) %>% #Set all months between July and August as Summer
  mutate(SeasonYear = paste(season, year, sep = " ")) #create a new column joining the season and year columns

unique_vals14 = unique(spring_2022_cleaned$motusTagID)

# set limits to map based on locations of detections, ensuring they include the
# deployment locations

xmin <- min(spring_2022_cleaned$recvDeployLon, na.rm = TRUE) - 2
xmax <- max(spring_2022_cleaned$recvDeployLon, na.rm = TRUE) + 2
ymin <- min(spring_2022_cleaned$recvDeployLat, na.rm = TRUE) - 1
ymax <- max(spring_2022_cleaned$recvDeployLat, na.rm = TRUE) + 1

###for rails with filtered detections
for(i in unique_vals14){
  df_tmp <- spring_2022_cleaned %>%
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
  species <- spring_2022_cleaned %>% 
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
      shape = 16,
      #colour = "black"
    ) +
    geom_point(
      data = df_tmp,
      aes(x = tagDepLon, y = tagDepLat),
      colour = "red", shape = 4
    ) +
    scale_colour_discrete(palette = "viridis")
  
  ggsave(plot2, file=paste0("Spring_2022_Cleaned_Maps/CleanMap_", i,".png"), 
         width = 14, height = 10, units = "cm", 
         create.dir = TRUE,
         path = "C:/Users/awsmilor/Git/Ward Lab/Individual_Rails/Imgs")
  
}

##### List of good birds to map ######
good_tags <- c("73260", #Has a really nice spring track up to Saskatchewan, 
               #then doubles back south to Northern MT, where it is picked up in summer, then has an okay fall track
               "73262", #Interesting track. Moves through MI & Ontario in summer. Also has spring track. Might need to remove Kennekuk
               "73270", #Best track we have. You can follow it's migration really well from Spring 2023 to Spring 2024
               "77501", #Few points in spring. Tons of interesting data in fall 2021. Heads to PA
               "77508", #Need to remove one point in Ontario but otherwise good fall & spring
               "73256" #Good tracks from Spring 2023 to Spring 2024
               )


#### Pretty map ####
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
  filter(SeasonYear %in% c("Fall 2022")) %>% 
  filter(speciesEN == "Sora") %>% 
  group_by(motusTagID) %>% 
  filter(n_distinct(recvDeployID) > 1) %>%
  arrange(motusTagID, time_cst) %>% # ensure points are in time order for path drawing
  filter(motusTagID %ni% c(73240, 73234
                           #,57121,57122,57146,57167,57171
                           )) %>% 
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
    title = "Tracks by Individual motusTagID (Fall 2023)"
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

#######Export individual data and create new folders to store them in########
setwd("R:/Rails_NEW") #if you want to export the data to somewhere other than your original directory (like the samba cluster) you can set that here
output_base <- "band_exports" #set the name of the folder you want all of your data to be stored in
if (!dir.exists(output_base)) dir.create(output_base) #create output folder if it doesn't already exist

#This process can take several hours. If you just want to test that it's outputting what you want, I suggest creating a sample of the data using slice_sample

# Loop over each unique Band_Number
unique_bands <- unique(alltags_unfiltered$Band_Number)

for (band in unique_bands) {
  
  # Subset data for this Band_Number
  subset_data <- alltags_rails %>% filter(Band_Number == band)
  
  # Get alpha_code and start_year from the first row (assuming consistent values per Band_Number)
  alpha_code <- subset_data$alpha_code[1]
  start_year <- subset_data$start_year[1]
  
  # Create folder name
  folder_name <- paste(alpha_code, band, start_year, sep = "_")
  folder_path <- file.path(output_base, folder_name)
  
  # Create folder if it doesn't exist
  if (!dir.exists(folder_path)) dir.create(folder_path)
  
  # Create CSV file name
  file_name <- paste(alpha_code, band, start_year, "unfiltered.csv", sep = "_")
  file_path <- file.path(folder_path, file_name)
  
  # Write CSV
  write.csv(subset_data, file_path)
}

spring_departures <- alltags_motus_filter %>% 
  mutate(tag_time = as_datetime(tagDeployStart)) %>% 
  mutate(date = date(time)) %>% 
  filter(month(tag_time) == 3:6)

fall_departures <- alltags_motus_filter %>% 
  mutate(tag_time = as_datetime(tagDeployStart)) %>% 
  mutate(date = date(time)) %>% 
  filter(month(tag_time) == 8:11)

fall_departures %>% 
  distinct(motusTagID) %>% 
  count()

spring_departures %>% 
  distinct(motusTagID) %>% 
  count()

alltags_motus_filter %>% 
  distinct(motusTagID) %>% 
  count()

departures %>% 
  distinct(motusTagID) %>% 
  count()
