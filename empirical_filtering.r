# set system environment time zone to GMT
Sys.setenv(tz="GMT")
getwd()
# load required packages
library(motus)
library(rms) # For modelling
library(ggplot2)
library(dplyr)
library(lubridate)

# set base theme for plots
theme_set(theme_bw(base_size = 18))

### download sample data from James Bay Shorebird Project #176
#   note: if you haven't downloaded sample dataset before, use 'new = T'
#   login name and password are both 'motus.sample'
#   specify 'dir =' to save elsewhere than working directory
db <- tagme(314, update = FALSE)

t <- tbl(db, "alltags")

df <- t %>% 
  filter(!is.na(tagDeployID)) %>%                                              # remove tags with no deployment or burst interval
  select(hitID, runID, ts, sig, sigsd, noise, freq, freqsd, slop, burstSlop,   # select variables
         motusTagID, port, runLen, tagModel, tagBI, tagDeployID,
         deviceID, recvDeployID, recvDeployLat, recvDeployLon, recv, recvSiteName) %>%
  arrange(motusTagID, ts) %>%                                                  # order data frame
  collect() %>%                                               # convert into flat data frame
  mutate(ts = as_datetime(ts)) %>%      # convert ts into date-time format
  distinct()

rm(t) # Remove this object now that we're done with it

# find duplicated runIDs (duplicates in ts, motusTagID, port, recv)
runsDup <- df %>%
  filter(duplicated(cbind(ts, motusTagID, port, recv)) == TRUE) %>%
  select(runID)

# check for deprecated batches
tbl(db, "deprecated")

# update SQL database by removing deprecated batches
# select 1 (Yes)
db <- deprecateBatches(db)

# update flat dataframe so it doesn't have duplicated runs due to deprecated batches
t <- tbl(db, "alltags")

df <- t %>% 
  filter(!is.na(tagDeployID)) %>%                                              # remove tags with no deployment or burst interval
  select(hitID, runID, ts, sig, sigsd, noise, freq, freqsd, slop, burstSlop,   # select variables
         motusTagID, port, runLen, tagModel, tagBI, tagDeployID,
         deviceID, recvDeployID, recvDeployLat, recvDeployLon, recv, recvSiteName) %>%
  arrange(motusTagID, ts) %>%                                                  # order data frame
  collect() %>%                                               # convert into flat data frame
  mutate(ts = as_datetime(ts)) %>%      # convert ts into date-time format
  distinct()

rm(t)  # Remove this object

# confirm there are no longer duplicated runIDs
runsDup <- df %>%
  filter(duplicated(cbind(ts, motusTagID, port, recv)) == TRUE) %>%
  select(runID)

rm(runsDup)



##### Step 1 - Classification of detections

### calculate features 2 and 3 for each run
runFeat <- df %>%
  group_by(runID) %>%
  mutate(duration = as.numeric(difftime(max(ts), min(ts), units = "secs")),
         nPred = ceiling(duration/tagBI) + 1,           # expected number of bursts in run if none are missed
         nMiss = nPred - runLen,                        # number of bursts missed in run
         propMiss = nMiss/nPred,                        # feature 2 - proportion of bursts missed in run
         meanBI = mean(as.numeric(diff(ts))),           # mean observed burst interval of all intervals between sequential bursts in a run
         deltaMeanBI = meanBI - tagBI,                  # feature 3 - difference between observed mean burst interval and expected burst interval recorded at tag registration
         log10runLen = log10(runLen)) %>%               # log10 of runLen for better visualization
  select(runID, motusTagID, port, runLen, tagModel, tagBI,
         recv, recvSiteName, recvDeployLat, recvDeployLon,
         duration, nPred, nMiss, propMiss, meanBI, deltaMeanBI, log10runLen) %>%
  filter(duplicated(runID) == FALSE) %>% 
  ungroup()

### assign category for goodness of detection (run)
#   filter by propMiss >= 0.75, deltaMeanBI >= 3*tagBI, runLen < 4
runFeat$category <- ifelse(abs(runFeat$propMiss) > 0.75 |
                             runFeat$deltaMeanBI > 3 * runFeat$tagBI |
                             runFeat$runLen < 4, "bad",
                           ifelse(abs(runFeat$propMiss) < 0.25 |
                                    runFeat$deltaMeanBI < runFeat$tagBI,
                                  "good", "unassigned"))

# some diagnostic plots
plotdata <- data.frame(group = rep(c("log10runLen", "propMiss", "deltaMeanBI"),
                                   each = nrow(runFeat)),
                       value = c(runFeat$log10runLen, runFeat$propMiss, runFeat$deltaMeanBI),
                       category = c(rep(runFeat$category, 3))) #%>%
#filter(category != "unassigned") # uncomment the %>% filter() statement to ignore the 'unassigned'


p1 <- ggplot(data = plotdata, 
             aes(x = abs(value), group = category, colour = category)) +
  geom_density() +
  facet_wrap(~group, scales = "free")

rm(plotdata)

p1


##### Step 2 - Identification of potential diagnostic parameters

### calculate run mean values for
#   'sigsd', 'noise', 'freqsd', 'slop', 'burstSlop'
runTemp <- df %>%
  group_by(runID) %>%
  mutate(meanTs = mean(ts),
         meanSigSD = mean(sigsd),
         meanNoise = mean(noise),
         meanFreqSD = mean(freqsd),
         meanSlop = mean(slop),
         meanBurstSlop = mean(burstSlop)) %>%
  select(runID, meanTs, meanSigSD, meanNoise, meanFreqSD, meanSlop, meanBurstSlop) %>%
  filter(duplicated(runID) == F) %>% 
  ungroup()

runFeat <- left_join(runFeat, runTemp, by = "runID")

rm(runTemp)

# plot variables (visual inspection of informative content)
plotdata <- data.frame(group = rep(c("meanSigSD", "meanNoise", "meanFreqSD",
                                     "meanSlop", "meanBurstSlop"),
                                   each = nrow(runFeat)),
                       value = c(runFeat$meanSigSD, runFeat$meanNoise,
                                 runFeat$meanFreqSD, runFeat$meanSlop,
                                 runFeat$meanBurstSlop),
                       category = c(rep(runFeat$category, 5))) #%>%
#filter(category != "unassigned") # uncomment the %>% filter() statement to ignore the 'unassigned'

p2 <- ggplot(data = plotdata, 
             aes(x = abs(value), group = category, colour = category)) +
  geom_density() +
  facet_wrap(~group, scales = "free")

rm(plotdata)

p2

# get activity table
a <- tbl(db, "activity")

adf <- collect(a)                                              # convert into flat data frame

rm(a)

# get runs table
r <- tbl(db, "runs")

rdf <- r %>% 
  collect() %>%
  mutate(hourBin = floor(tsBegin/3600))                                # calculate hourBin for each run

rm(r)

# join runs and activity tables to get hourly values on runs/recv for each runID
radf <- left_join(rdf, adf, by = c("batchIDbegin" = "batchID", "ant", "hourBin"))

# join with detection data
df <- left_join(df, select(radf, runID, numRuns, numHits, run2, run3), by = "runID")

### calculate 'noise' variables with values from activity table
runTemp <- df %>% 
  mutate(propRS = (run2 + run3)/numRuns, # proportion of short runs per receiver hour
         log10numRuns = log10(numRuns)) %>% # log transform for better visibility 
  group_by(motusTagID) %>% 
  mutate(tagPropRS = length(unique(runID[runLen<4]))/length(unique(runID))) %>% # tags’ proportion of short runs
  group_by(runID) %>%
  select(runID, numRuns, propRS, tagPropRS, log10numRuns) %>%
  filter(duplicated(runID) == FALSE) %>% 
  ungroup()

runFeat <- merge(runFeat, runTemp, by = "runID")

rm(runTemp)

# plot variables
plotdata <- data.frame(group = rep(c("log10numRuns", "propRS", "tagPropRS"),
                                   each = nrow(runFeat)),
                       value = c(runFeat$log10numRuns, runFeat$propRS, runFeat$tagPropRS),
                       category = c(rep(runFeat$category, 3))) #%>%
#filter(category != "unassigned") # uncomment the %>% filter() statement to ignore the ‘unassigned’

p3 <- ggplot(data = plotdata, aes(x = abs(value), group = category, colour = category)) +
  geom_density() +
  facet_wrap(~group, scales = "free")

rm(plotdata)

p3

### calculate 'continuity' variables
runTemp <- df %>%
  group_by(runID) %>%
  mutate(meanTs = mean(ts)) %>%
  group_by(motusTagID, recv) %>%
  mutate(nContRuns = length(unique(runID[ts <= meanTs + 25*60 |
                                           ts >= meanTs - 25*60])),
         nPorts = length(unique(port[ts <= meanTs + 25*60 |
                                       ts >= meanTs - 25*60]))) %>%
  group_by(runID) %>%
  select(runID, nContRuns, nPorts) %>%
  filter(duplicated(runID) == FALSE) %>% 
  ungroup()

runFeat <- left_join(runFeat, runTemp, by = "runID")

rm(runTemp)

# plot variables
plotdata <- data.frame(group = rep(c("nContRuns", "nPorts"),
                                   each = nrow(runFeat)),
                       value = c(runFeat$nContRuns, runFeat$nPorts),
                       category = c(rep(runFeat$category, 2))) #%>%
#filter(category != "unassigned") # uncomment the %>% filter() statement to ignore the 'unassigned'


p4 <- ggplot(data = plotdata, 
             aes(x = abs(value), group = category, colour = category)) +
  geom_density() +
  facet_wrap(~group, scales = "free")

rm(plotdata)

p4
