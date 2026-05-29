library(tidyverse)
library(sf)
library(rnaturalearth)

getwd()

alltags_filtered <- read.csv("alltags_filtered.csv")

alltags_ordered <- mutate(alltags_filtered, recvDeployName = reorder(recvDeployName,recvDeployLat))

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

df <- alltags_corrected %>%
  mutate(year = as.character(year(time_cst))) %>% 
  mutate(season = as.character(month(time_cst))) %>% #Create a new column for the season the bird was tagged during
  mutate(season = replace(season, season %in% 4:5, "Spring")) %>% #Set all months between March and June as spring
  mutate(season = replace(season, season %in% 8:11, "Fall")) %>% #Set all months between August and November as Fall
  mutate(season = replace(season, season %in% c(1,2,3,12), "Winter")) %>% #Set all months between December and February as Winter
  mutate(season = replace(season, season %in% 6:7, "Summer")) %>% #Set all months between July and August as Summer
  mutate(SeasonYear = paste(season, year, sep = " ")) %>%  #create a new column joining the season and year columns
  filter(motusTagID %ni% c(53163, 57153,57136,57178,57281,57344,62560,73241,73257,73258,73261,77498,77510)) #remove birds where the transmitter dropped from dataset

bad_dates <- ymd(c("2024-04-18", "2024-04-19", "2024-04-22", "2024-05-18"))

tag <- d %>% 
  filter(alpha_code == "VIRA") %>% 
  mutate(time = as_datetime(time)) %>% 
  mutate(s2n = sig-noise) %>% 
  filter(
    !(motusTagID == 53158 & recvDeployName == "Birds Canada HQ"), #Contradicting detections here and at Chad. Not 100% sure if either are true, but this one had an extremely low s2n
    !(motusTagID == 53894 & recvDeployName == "SAVA03"), #Detected at a tower on the Chesapeake in August then multiple times at Chad a few weeks later. Not sure which is true, but I strongly suspect one isn't
    !(motusTagID == 57122 & recvDeployName == "Birds Canada HQ"), #Detections at Chad on 9/7 and then here at 9/27. Relatively short runs at both. I'd guess both are false
    !(motusTagID %in% c(57123, 57128) & recvDeployName == "Kent Island"), #Detected in Kent Island the following fall and summer. Maybe legit but probably now. No other detections that fall
    !(motusTagID == 57125 & recvDeployName == "Birds Canada HQ"), #Detected at Bird Canada HQ after leaving tagging site. Guessing the bird didn't migrate north
    !(motusTagID == 57122 & recvDeployName %in% c("Kent Island", "Bois de la Roche 2", "Bois de la Roche 1")), #Sporadic Detections in NE canada the following fall. All relatively short runs
    !(motusTagID %in% c(57121) & recvDeployName %in% c("Kent Island", "Bois de la Roche 2")), #Not a lot of fetections in fall 2022 and none have very strong runs. Most likely detection is probably one in southern Manitoba
    !(motusTagID == 57124 & recvDeployName %in% c("Kent Island", "Triton2")), #Detections at both site in 2022. Also a detection at triton in 2021 shortly after tagging, though this is probably unlikely given more verifiable detections in Missouri 4 days later.
    !(motusTagID == 57126 & recvDeployName %in% c("Kent Island", "Triton2")), #Detected at both sites in 2022. Short runs (4 & 5)
    !(motusTagID == 57144 & recvDeployName == "Birds Canada HQ"), #Bird had a short run at this site in Ontario 1 day after departing from tagging site.
    !(motusTagID == 57139 & recvDeployName %in% c("Kent Island", "Triton2")), #Short runs at both sites in 2022. Only 4 detections at each with northwestward movement between.
    !(motusTagID == 57167 & recvDeployName %in% c("Golfo de Santa Clara - RV Park", "Scotch_Bush", "GMNP-Berry Hill", "Beloeil")), #Lots of weird detections from this bird. Feels improbably that the bird ended up on the gulf on california. Also a short detection in E Ontario that I think was untrue. Detected sporadically from 6/06 to 10/27 in 2022 in FL but not sure I trust those fully. There are a few summertime records on eBird from nearby sites though. Relatively short run at GMNP Berry hill in september is probably false. In spring detected in Quebec 3 days after tagging for short run (4). Probably false
    !(motusTagID == 57172 & recvDeployName == "Senneville Farm"), #Short run way to the north after a good track south along the eastern seaboard
    !(motusTagID == 57173 & recvDeployName == "Senneville Farm"), #Brief detections at Senneville farm. Could be legit. Only other fall detection was in FL before the Senneville detection, though that also seems potentially suspect. All detections have low s2n
    !(motusTagID == 65915 & recvDeployName %in% c("Selma2","Truro")), #Late fall (11/01-11/24) and early winter (12/05) detections that are probably false. Mostly low S2N values and short runs
    !(motusTagID == 65917 & recvDeployName == "Grève de Tadoussac"), #Short run (4) in mid Nov. after bird was detected in Mexico.
    !(motusTagID == 65920 & recvDeployName %in% c("Allison (Johnston Point II)","Cape Jourimain")), #Short runs (4) at two stations in Nova Scotia in late October
    !(motusTagID == 67763 & recvDeployName %in% c("Weir6","West End")), #A few sporadic detections in NH over a few days. Maybe okay? No other detections to verify
    !(motusTagID == 62539 & recvDeployName == "Kejimkujik National Park"), # Short run (4) in spring after tagging. Detected south of this point near lake Ontario 2 weeks later
    !(motusTagID == 73226 & recvDeployName %in% c("Kejimkujik National Park", "Kennekuk 2")), # Short run at KNP probably not real. Weird detections in April at Kennekuk are suspect given good track later in may
    !(motusTagID == 73238 & recvDeployName == "Stump Lake"), # Short run (4) that contradicts more probable detections 
    !(motusTagID == 53105 & recvDeployName %in% c("Golfo de Santa Clara - RV Park","Birds Canada HQ")), #Dubious detections that don't make sense in space or time.
    !(motusTagID == 57174 & recvDeployName %in% c("Florida Panther NWR, FL")), #some extremely unlikely detections in august (no records of VIRA in south FL in August on eBird). Detections in October plausible, but probably unlikely given stronger detections. Only a few detections actually are possible. All relatively low s2n.
    !(motusTagID == 63993 & recvDeployName %in% c("AVNJ", "Cape Romain NWR, SC (Bulls Island)", "GA_OSS_DOCK", "Parramore Island")), #All short records that conflict with other much better detections.
    !(motusTagID == 73207 & recvDeployName %in% c("Pumphouse")), #Short run (4) at Pumphouse in late november. While it's plausible that the bird was present I feel that the short run + very low S2N make it unlikely
    !(motusTagID == 73234 & recvDeployName %in% c("Bluestem Farm", "RPBO-Donnecke")), #Two short (4) runs. One on the east coast on 9-05, one on the west coast on 9-15. I doubt the bird flew west across the continent in ten days. Also low s2n
    !(motusTagID == 73240 & recvDeployName %in% c("Bluestem Farm", "Santa Ana NWR", "Shag Harbour")), #All low S2N and short runs (4-5). Plus, don't make a lot of contextual sense.Supposedly bird went from MD to south TX, then to Nova Scotia
    !(motusTagID == 73244 & recvDeployName %in% c("Pumphouse")), #Short run (4) at Pumphouse in late november with low S2N. Doesn't make contextual sense
    !(motusTagID == 73201 & recvDeployName %in% c("Swan Bay")), #Short run (4) almost 2 weeks after better detections of the bird further to the north east.
    !(motusTagID == 73222 & recvDeployName %in% c("Alaksen")), # Quite a few detections, but given  the context of other detections, it seems likely that these detections in BC are spurious.
    !(motusTagID == 73234 & recvDeployName %in% c("Shawanaga Old Gas Station", "Scout Island Nature Centre", "Stump Lake", "Kennekuk 2", "Kennekuk 6", "Allerton")), #Mix of short runs (4-5) and extremely dubious detections in western Canada. Also some suspect detections around kennekuk.
    !(motusTagID == 73240 & recvDeployName %in% c("Stump Lake")), #BC detections don't make sense given more concrete detections a week later as the bird migrated northeast through Iowa
    !(motusTagID == 64001 & recvDeployName %in% c("Chad")), #Only two detection and both a relatively short runs (5). Southbound movement between detections. Feels unlikely. Not sure which, if either, are true.
    !(motusTagID == 63996 & recvDeployName %in% c("Chad")), # Quite a few detections at Chad but I think the fact that it was almost simultaneously detected at two more northerly towers a few weeks prior rules these out as correct.
    !(motusTagID == 77508 & recvDeployName %in% c("Split Rock")), #Summer detection that doesn't make sense. Short run too.
    !(motusTagID == 57190 & recvDeployName %in% c("Johnstons Point", "Selma2", "Truro")), #Plausible detections in Nova Scotia, but they look really weird. Short (4-6) runs with fairly low S2N. Probably aliasing
    !(motusTagID == 57340 & recvDeployName %in% c("Johnstons Point", "Allison (Johnston Point II)", "Linden2")), #Plausible detections in Nova Scotia, but they look really weird. Short (4-6) runs with fairly low S2N. Probably aliasing
    !(motusTagID == 73240 & recvDeployName %in% c("Stump Lake")), #Highly unlikely BC detections. Multiple short (4-6) runs over 30 minutes at low S2N. Feels more like aliasing than true detections.
    !(motusTagID == 73256 & recvDeployName %in% c("Pumphouse")), #Conflicting short (4) run
    !(motusTagID == 73225 & recvDeployName %in% c("Stump Lake")) #Conflicts with summer detection
  ) %>% 
  filter(time_cst < tag_time + 38880000) %>% 
  filter(recvDeployName %ni% c("Kent Island", "Triton2","Bois de la Roche 2", "Bois de la Roche 1", "McGill_Bird_Observatory", "Kent Farm Research Station")) %>%  #these towers are clearly bad. Keep coming up in nonsensical ways
  filter(!(recvDeployName %in% c("Kennekuk 2", "Allerton", "Kennekuk 6") & date(time_cst) %in% bad_dates))

world <- ne_countries(scale = "medium", returnclass = "sf") 
lakes <- ne_download(scale = "medium", type = 'lakes', category = 'physical',
                     returnclass = "sf")
states <- ne_states(returnclass = "sf")

###code for adding state/country boundaries
usmap <- ne_states(country = c("United States of America","Canada"), returnclass = 'sf')
eastern <- filter(usmap, region %in% c("Midwest","South","Northeast")) 

#Downloaded 3/18/2026
sora_range <- st_read("Data/sora_range_2023/sora_range_2023.gpkg") %>% 
  filter(season == c("breeding", "nonbreeding"))
vira_range <- st_read("Data/virrai_range_2023/virrai_range_2023.gpkg") %>% 
  filter(season == c("breeding", "nonbreeding"))

tag_map <- tag %>% 
  #filter(motusTagID == 77509) %>% 
  mutate(recvDeployID = if_else(recvDeployID %in% c(7895, 7893, 7891, 7889),
                                99999999999,
                                recvDeployID)) %>% 
  group_by(interaction(motusTagID, SeasonYear)) %>% 
  filter(n_distinct(recvDeployID) > 1) %>%
  arrange(motusTagID, time_cst) %>% 
  mutate(
    motusTagID = as.factor(motusTagID),
    is_first = if_else(row_number() == 1, TRUE, FALSE)) %>% 
  filter(season == "Fall") 


points_sf <- st_as_sf(tag_map, coords = c("recvDeployLon", "recvDeployLat"), crs = 4326, remove = F) #

bbox_points <- st_bbox(points_sf)

map <- ggplot() +
  geom_sf(data = states,fill="gray98",color = NA)+
  geom_sf(data = lakes, colour = NA, fill = "gray80")+
  #geom_sf(data = usmap, fill = NA)+
  geom_sf(data = states,fill=NA)+
  geom_sf(data = vira_range, aes(fill = season), alpha = 0.5, color = NA) +
  #geom_sf(data = points_sf, aes(color = s2n))+
  geom_path(
    data = points_sf,
    aes(x = recvDeployLon, y = recvDeployLat, 
        group = interaction(motusTagID, SeasonYear)
        #,color = motusTagID
    ),
    linewidth = .5,linetype = 5
  ) +
  
  # Detection points
  geom_point(
    data = points_sf,
    aes(x = recvDeployLon, y = recvDeployLat,
        group = motusTagID, #colour = as.factor(SeasonYear)
    ),
    shape = 21,
    size = 2,
    fill = "gray95"
  ) +
  
  geom_point(
    data = points_sf %>% filter(is_first),
    aes(x = recvDeployLon, y = recvDeployLat
    ),
    shape = 24,   # triangle up
    size = 3,
    fill = "gray95")+
  
  # Tag deployment point
  geom_point(
    aes(x = -90.071194, y = 40.347444),
    colour = "gray15", shape = 8, size = 4
  ) +
  coord_sf(
    xlim = c(bbox_points["xmin"], bbox_points["xmax"]),
    ylim = c(bbox_points["ymin"], bbox_points["ymax"]))+
  theme_bw() +
  labs(
    x = "",
    y = "",
    title = "VIRA"
  ) +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "gray80"),
    legend.position = "none"
  )

map

ggsave(filename = paste("tracks_VIRA_fall.png", sep = "_"), plot = map)

