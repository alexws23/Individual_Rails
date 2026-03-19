library(motus)
library(tidyverse)
library(DBI)
library(RSQLite)
library(sf)
library(rnaturalearth)
library(paletteer)
library(viridis)

tags <- alltags %>% 
  select(tagDepLat,tagDepLon) %>% 
  round(digits = 5) %>% 
  unique()

write.csv(tags, file = "tag_dep_location.csv")

world <- ne_countries(scale = "medium", returnclass = "sf") 
lakes <- ne_download(scale = "medium", type = 'lakes', category = 'physical',
                     returnclass = "sf")

plot2 <- ggplot(data = world) +
  geom_sf(colour = NA)+
  geom_sf(data = lakes, colour = NA, fill = "white")+
  coord_sf(xlim = c(-89, -91), ylim = c(40, 41), expand = FALSE)+
  geom_point(data = tags,
             aes(x = tagDepLon, y = tagDepLat))
plot2
