##This code will allow you to download MOTUS data from MOTUS central using the "motus" package, clean the data, and easily export the data
##to multiple CSV files that correspond to individual birds, with each file stored in its own folder. This script only exports unfiltered data,
##which may be useful for parsing through very short runs or relatively short runs at noisy stations to determine whether or not they are true
##detections.

#If you haven't installed the "motus" package, install it here.
install.packages(c("motus", "motusData"), 
                 repos = c(birdscanada = 'https://birdscanada.r-universe.dev',
                           CRAN = 'https://cloud.r-project.org'))

#Attach necessary packages for receiving, cleaning, and exporting MOTUS data
library(motus)
library(tidyverse)

#Set environment
Sys.setenv(TZ = "UTC") #Ensure that the system environment's timezone is set to UTC
setwd("C:/Users/awsmilor/Git/Ward Lab/Individual_Rails/Data") #Set your working directory
getwd()

#Bring the MOTUS data into R as an SQLite object
sql_motus <- tagme(projRecv = 314, #Add your project number. You will need your username and password to make this work
                        new = TRUE) #if you already have the .motus file in your working directory, you can change this to false.

file.name <- dbConnect(SQLite(), "./project-314.motus")

# get a list of tables in the .motus file specified above.
dbListTables(file.name)

#Create a virtual table from the "alltags" table in the SQLite file
tbl.alltags <- tbl(sql_motus, "alltags")

#Convert the table into a data frame
df.alltags <- tbl.alltags %>% 
  collect() %>% 
  as.data.frame()

#Create a virtual table from the "tagDeps" table in the SQLite file
tbl.tagDeps <- tbl(sql_motus, "tagDeps")

#convert the table into a data frame
df.tagDeps <- tbl.tagDeps %>% 
  collect() %>% 
  as.data.frame() %>% 
  rename(tagDeployID = deployID) %>% 
  select(tagDeployID|tagID)
