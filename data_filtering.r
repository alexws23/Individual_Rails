library(motus)
library(tidyverse)
library(DBI)
library(RSQLite)
library(sf)
library(rnaturalearth)

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
departures <- alltags_motus_filter %>% 
  filter(recvDeployName %in% c("Chad", "Chautauqua", "Swan Bay", "Pumphouse",
                               "Dixon Waterfowl Refuge-North")) %>% 
  mutate(tag_time = as_datetime(tagDeployStart)) %>% 
  mutate(date = date(time))

fall_departures <- departures %>% 
  mutate(tag_time = as_datetime(tagDeployStart)) %>% 
  mutate(date = date(time)) %>% 
  filter(month(tag_time) == 8:11)

departures %>% 
  filter(motusTagID == 62571) %>% 
  mutate(time_cst = with_tz(time, "US/Central")) %>% 
  mutate(date_cst = date(time_cst)) %>% 
  ggplot(aes(x = time_cst, y = sig, colour = as.factor(port))) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) + 
  geom_point() + 
  geom_smooth(method = "loess", se = FALSE) + 
  facet_wrap(date_cst ~ recvDeployName, scales = "free_x")

tag_65915 <- departures %>% 
  filter(motusTagID == 65915) %>% 
  mutate(time_cst = with_tz(time, "US/Central")) %>% 
  mutate(date_cst = date(time_cst)) %>%
  arrange(desc(time_cst))



####### Make for-loop for spatial map
###################
###load in important objects for map visuals, should only need to do once if you use the same working directory
world <- ne_countries(scale = "medium", returnclass = "sf") 
lakes <- ne_download(scale = "medium", type = 'lakes', category = 'physical',
                     returnclass = "sf")

###code for adding state/country boundaries
usmap <- ne_states(country = c("United States of America","Canada"), returnclass = 'sf')
eastern <- filter(usmap, region %in% c("Midwest","South","Northeast"))

xmin <- min(tag_65915$recvDeployLon, na.rm = TRUE) - 2
xmax <- max(tag_65915$recvDeployLon, na.rm = TRUE) + 2
ymin <- min(tag_65915$recvDeployLat, na.rm = TRUE) - 1
ymax <- max(tag_65915$recvDeployLat, na.rm = TRUE) + 1

ggplot(data = world) +
  geom_sf(colour = NA) +
  geom_sf(data = lakes, colour = NA, fill = "white") +
  geom_sf(data = usmap, fill = "gray98") +
  coord_sf(xlim = c(xmin, xmax), ylim = c(ymin, ymax), expand = FALSE) +
  theme_bw() + 
  labs(x = "", y = "") +
  geom_path(data = tag_65915, 
            aes(x = recvDeployLon, y = recvDeployLat, 
                group = as.factor(mfgID), colour = as.factor(mfgID))) +
  geom_point(data = tag_65915, aes(x = recvDeployLon, y = recvDeployLat), 
             shape = 16, colour = "black") +
  geom_point(data = tag_65915, 
             aes(x = tagDepLon, y = tagDepLat), colour = "red", shape = 4) +
  scale_colour_discrete("Motus Tag ID")

ggsave(plot2, file=paste0("DubMap_", i,".png"), width = 14, height = 10, units = "cm")

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
