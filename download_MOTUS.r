library(motus)
library(lubridate)
library(dplyr)
library(DBI)
library(RSQLite)

#Download data for each individual rail from MOTUS

Sys.setenv(TZ = "UTC")
setwd("C:/Users/awsmilor/Git/Ward Lab/Individual_Rails/Data")
getwd()

#Bring the MOTUS data into R as an SQLite object
sql_motus_rail <- tagme(projRecv = 314, new = TRUE)

file.name <- dbConnect(SQLite(), "./project-314.motus")

# get a list of tables in the .motus file specified above.
dbListTables(file.name)

# get a list of variables in the "species" table in the .motus file.
dbListFields(file.name, "species") 

tbl.alltags <- tbl(sql_motus_rail, "alltags")

str(tbl.alltags)

tbl.alltags %>% 
  collect() %>%
  names() # list the variable names in the table

df.alltags <- tbl.alltags %>% 
  collect() %>% 
  as.data.frame()

tbl.tagprops <- tbl(sql_motus_rail, "tagDeps")

df.tagprops <- tbl.tagprops %>% 
  collect() %>% 
  as.data.frame() #%>% 
  #rename(tagDeployID = deployID) %>% 
  #select(tagDeployID|tagID)

names(df.alltags)     # field names
str(df.alltags)       # structure of your data fields
head(df.alltags)      # prints the first 6 rows of your df to the console
summary(df.alltags)   # summary of each column in your df

df.alltags <- tbl.alltags %>% 
  collect() %>% 
  as.data.frame() %>%     # for all fields in the df (data frame)
  mutate(time = as_datetime(ts))

 # the tz = "UTC" is not necessary here, provided you have set your system time to UTC
# ... but it serves as a useful reminder!

rails <- read.csv("rails.csv") %>% 
  select(!(X:X.4)) %>% 
  mutate(Band_Number = gsub("-","", Band_Number)) %>% 
  select(!(Code | Species)) %>% 
  rename(tagID = Motus.ID)

df.alltags.rails <- df.alltags %>% 
  filter(speciesEN %in% c("Sora", "Virginia Rail")) %>% 
  mutate(alpha_code = speciesEN) %>% 
  mutate(alpha_code = replace(alpha_code, alpha_code == "Virginia Rail", "VIRA")) %>% 
  mutate(alpha_code = replace(alpha_code, alpha_code == "Sora", "SORA")) %>% 
  mutate(start_year = year(tagDeployStart))


df.alltags.rails <- df.alltags.rails %>% 
  left_join(df.tagprops, by = "tagDeployID")

"%ni%" <- Negate("%in%")

alltags_rails <- df.alltags.rails %>% 
  left_join(rails, by = "tagID") %>% 
  filter(tagDepComments %ni% "TEST TAG ACTIVATED 9/7, GIVEN TO MIKE AVARA 9/14")

sample <- slice_sample(alltags_rails, n = 100)
write.csv(sample, "data_sample.csv")

# Create output directory (optional)
setwd("R:/Rails_NEW")
output_base <- "band_exports"
if (!dir.exists(output_base)) dir.create(output_base)

# Loop over each unique Band_Number
unique_bands <- unique(alltags_rails$Band_Number)

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

cat("✅ Export complete. Files saved in:", output_base, "\n")


########################################################
tbl(sql_motus_rail, "alltags") %>%
  select(hitID, runID, batchID, ts, motusFilter)

df.alltags.rails %>% 
  group_by(motusFilter) %>% 
  count()

tbl_alltags_sub <- tbl(sql_motus_rail, "alltags") %>%
  filter(motusFilter == 1)

tbl(sql_motus_rail, "alltags") %>%
  select(hitID, runID, batchID, motusTagID, runLen) %>%
  filter(runLen <= 3)

tbl_alltags_sub %>%
  select(hitID, runID, batchID, motusTagID, runLen) %>%
  filter(runLen <= 3)

df_block_0 <- tbl(sql_motus_rail, "alltags") %>%
  filter(motusFilter == 0) %>%
  collect()

