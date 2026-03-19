library(motus)
library(tidyverse)
library(DBI)
library(RSQLite)
library(sf)
library(rnaturalearth)
library(paletteer)
library(viridis)
library(ggrepel)

### Set Environment and Working Directory ###

Sys.setenv(TZ = "UTC") #Ensure that the system environment's timezone is set to UTC
setwd("C:/Users/awsmilor/Git/Ward Lab/Individual_Rails/Data") #Set your working directory
getwd()

### Bring the MOTUS data into R and clean it ###

#Bring the MOTUS data into R as an SQLite object
file.name <- dbConnect(SQLite(), "./project-314.motus") #replace 314 with your project ID

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


###############################################################################
##### OPTIONAL!!! if your tagDeps DF doesn't include band number information #####
##If the tagDeps dataframe doesn't have data in the bandNumber column, you will need to get that information from elsewhere.
##For the rail data, I had a separate CSV that had the Motus tag ID (called tagID in the "tagDeps" df) and the band numbers.
##If this is the case for you, follow the next steps.

band_numbers <- read.csv("rails.csv") %>% 
  select(!(X:X.4)) %>% select(!(Code | Species)) %>% # #if your CSV has any columns you want to remove, you can do so with these functions.
  mutate(Band_Number = gsub("-","", Band_Number)) %>% #If the band numbers include a "-" you can remove that with the gsub function
  rename(tagID = Motus.ID) #Change the name of the motus tag ID column to "tagID" if needed

#Join the band_numbers data frame to the tagDeps dataframe by the "tagID"
df.tagDeps <- df.tagDeps %>% 
  left_join(band_numbers, by = "tagID")

########## OPTIONAL ###########
########## Creates some new columns if you want to include them ##########
#Create two new variables, one with the alpha code of your study species and one with the year the tag was deployed
alltags <- df.alltags %>% 
  filter(speciesEN %in% c("Sora", "Virginia Rail")) %>% #If your want to only include certain species from your project you can filter with this.
  mutate(alpha_code = speciesEN) %>% #create a new column for alpha codes that is a duplicate of the species name column
  mutate(alpha_code = replace(alpha_code, alpha_code == "Virginia Rail", "VIRA")) %>% #rename each of the species in the alpha code column to their code
  mutate(alpha_code = replace(alpha_code, alpha_code == "Sora", "SORA")) %>% 
  mutate(start_year = year(tagDeployStart)) #create a column with the year the tag was deployed

##### Join the tag information with the alltags dataframe. This gives you more helpful information about the tags.

#Join the alltags dataframe to the tagDeps dataframe to add band number data
"%ni%" <- Negate("%in%")

alltags_unfiltered <- alltags %>% 
  left_join(df.tagDeps, by = "tagDeployID") %>% 
  filter(tagDepComments %ni% "TEST TAG ACTIVATED 9/7, GIVEN TO MIKE AVARA 9/14") #if your dataset includes test tags, filter it out

###############################################################################
#### Filtering the Data ####
#filter the data to only include data where the motusFilter equals 1
alltags_motus_filter <- alltags_unfiltered %>% 
  filter(motusFilter == 1)

#remove all rows where the receiver info is missing
alltags_na_receivers <- alltags_motus_filter %>%
  drop_na(recvDeployLat | recvDeployName)


###### This is the ambiguous tag code from MOTUS. It isn't complete since it's highly dependent on the project, so follow the MOTUS R handbook
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

############# Getting rid of towers outside of species range and  adding a few new columns #########
alltags_ordered <- mutate(alltags_na_receivers, recvDeployName = reorder(recvDeployName,recvDeployLat))

##set parameters for latitude > 0 to remove some false detections
alltags_ordered <- subset(alltags_ordered, recvDeployLat>-5)
alltags_ordered <- subset(alltags_ordered, recvDeployLon< -52)
alltags_cleaned <- subset(alltags_ordered, recvDeployLon> -140)

### Add a few convenient human-readable parameters and divide the year into seasons. Feel free to adjust the seasonal definitions depending on your species or remove it altogether if you don't find it helpful
alltags_corrected <- alltags_cleaned %>% 
  mutate(tag_time = as_datetime(tagDeployStart)) %>% 
  mutate(date = date(time)) %>% 
  mutate(time_cst = with_tz(time, "US/Central")) %>% 
  mutate(date_cst = date(time_cst)) %>% 
  mutate(year = as.character(year(time_cst))) %>% 
  mutate(season = as.character(month(time_cst))) %>% #Create a new column for the season the bird was tagged during
  mutate(season = replace(season, season %in% 3:6, "Spring")) %>% #Set all months between March and June as spring
  mutate(season = replace(season, season %in% 8:11, "Fall")) %>% #Set all months between August and November as Fall
  mutate(season = replace(season, season %in% c(1,2,12), "Winter")) %>% #Set all months between December and February as Winter
  mutate(season = replace(season, season %in% 7, "Summer")) %>% #Set all months between July and August as Summer
  mutate(SeasonYear = paste(season, year, sep = " "))    #create a new column joining the season and year columns

## Remove df and tbls no longer in use
rm(list = c("tbl.tagDeps", "tbl.alltags", "file.name", "df.alltags","df.tagDeps","band_numbers", "alltags"))

## So that you don't need to read in the .MOTUS file every time, I recommend saving a copy of the alltags_corrected dataframe
write.csv(alltags_corrected, "data.csv") # change the filename as needed


################################################################################
#### Creating Plots and Maps of Detections ####
#### I found these helpful, so hopefully you do too ####

# Summarize by bird and receiver site
site_summary <- alltags_corrected %>%
  group_by(motusTagID, recvDeployName, recvDeployLon, recvDeployLat, SeasonYear) %>%
  summarise(
    first_det = min(date_cst, na.rm = TRUE),
    last_det  = max(date_cst, na.rm = TRUE),
    duration_days = as.numeric(difftime(last_det, first_det, units = "days")),
    .groups = "drop"
  )

unique = unique(alltags_corrected$motusTagID)

unique = c(53894, 73226)

### Setting parameters for maps
world <- ne_countries(scale = "medium", returnclass = "sf") 
lakes <- ne_download(scale = "medium", type = 'lakes', category = 'physical',
                     returnclass = "sf")

###code for adding state/country boundaries
usmap <- ne_states(country = c("United States of America","Canada"), returnclass = 'sf')
eastern <- filter(usmap, region %in% c("Midwest","South","Northeast")) 

### For-loop to create maps for every tag
for(i in unique) {
  
  df_tmp <- alltags_corrected %>%
    filter(motusTagID == i) %>%
    arrange(time_cst) %>% 
    mutate(recvDeployName = as.character(recvDeployName))
  
  # set limits to map based on locations of detections, ensuring they include the
  # deployment locations
  
  xmin <- min(df_tmp$recvDeployLon, na.rm = TRUE) - 2
  xmax <- max(df_tmp$recvDeployLon, na.rm = TRUE) + 2
  ymin <- min(df_tmp$recvDeployLat, na.rm = TRUE) - 1
  ymax <- max(df_tmp$recvDeployLat, na.rm = TRUE) + 1
  
  df_recv <- df_tmp %>% 
    distinct(recvDeployName, recvDeployLat, recvDeployLon)
  
  df_summary <- site_summary %>%
    filter(motusTagID == i)
  
  species <- df_tmp %>%
    distinct(speciesEN) %>%
    pull()
  
  plot <- ggplot(data = world) +
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
    
    # Detection sites: color = most recent detection, size = duration
    geom_point(
      data = df_summary,
      aes(x = recvDeployLon, y = recvDeployLat,
          color = as.factor(last_det),
          size = duration_days),
      alpha = 0.9
    ) +
    
    #Create labels that include only the first 6 letters of each receiver name 
    geom_text_repel(data = df_recv, aes(x = recvDeployLon, y = recvDeployLat, label = substr(recvDeployName, 1,6)),
               size = 1.75,
               color = "black") +
    
    # Tag deployment point
    geom_point(
      data = df_tmp,
      aes(x = tagDepLon, y = tagDepLat),
      colour = "red", shape = 4, size = 2
    ) +
    
    scale_color_viridis_d(option = "plasma", direction = -1) +
    scale_size_continuous(range = c(1, 6)) +
    theme(legend.position = "right")
  
  #Export image file
  ggsave(plot, file = paste0("Detection_maps/all_towers_map_", i, ".png"), 
         units = "cm", 
         create.dir = TRUE,
         #path = "C:/Users/awsmilor/Git/Ward Lab/Individual_Rails/Imgs" OPTIONAL. If you want to specify a folder for your images different than your working directory.
  )
}

##### Create Plots that show detections ordered by latitude
for(i in unique){
  #Object for species name
  species <- alltags_corrected %>% 
    filter(motusTagID == i) %>% 
    select(speciesEN) %>% 
    distinct()
  
  #Create plot
  plot <- ggplot(data = filter(alltags_corrected, 
                               motusTagID == i), 
                 aes(x = time_cst, y = recvDeployName, color = SeasonYear)) +
    theme_bw() + 
    geom_point() + 
    labs(x = "Time of year", y = "Receiver name (ordered by latitude)", subtitle = species)
  
  ggsave(plot, file=paste0("Detection_plots/all  _towers_plot_", i,".png"), units = "cm", create.dir = TRUE,
         #, path = "C:/Users/awsmilor/Git/Ward Lab/Individual_Rails/Imgs" #If you want to specify a specific path to save to
         )
}
