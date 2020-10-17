library(tidyverse)
library(lubridate)
library(magrittr)
library(STRAD)
library(sf)
library(geojsonsf)

#read in geojson - also saved as oct_2020_hotspots.geojson
#link to the geojson via https://qri.cloud/chriswhong-demo/new_york_city_covid_19_hot_spot_zones
hotspots <- geojson_sf("https://services1.arcgis.com/oOUgp466Coyjcu6V/arcgis/rest/services/HotSpots_Union_Dissolve/FeatureServer/0/query?f=pgeojson&where=OBJECTID>0")

hotspots %<>% mutate(n=Name) %>%  separate(n,"_",into=c("place","color"))


hotspots %>% 
  ggplot() +
  geom_sf(aes(fill=color)) +
  scale_fill_manual(values = c(orange="orange",red="red",yellow="yellow"))

saveRDS(hotspots,"oct_2020_hotspots_sf.Rds")

#intersect with zip codes

