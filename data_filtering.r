library(motus)
library(tidyverse)
library(DBI)
library(RSQLite)

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

