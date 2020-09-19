library(tidyverse)
library(RSocrata)




threeoneone <- read.socrata("https://data.cityofnewyork.us/resource/erm2-nwe9.csv?$order=unique_key&agency=DEP&$where=created_date>'2019-01-01'")

saveRDS(threeoneone,"DEP_311_from2019.Rds")


library(sf)
library(STRAD)

manhattan <- STRAD::branch_geoms %>% mutate(borough=STRAD::getborough(branch_code)) %>% 
  filter(borough=="Manhattan")

lower_manhattan <- STRAD::branch_geoms %>% mutate(borough=STRAD::getborough(branch_code)) %>% 
  filter(borough=="Manhattan",branch_code  %in% c("jm","ml","hp","ts","ch","se","ot","lm","bp","hf"))
lm_limits <- st_bbox(lower_manhattan)

lm_hvac_complaints <- threeoneone %>% 
  mutate(month=month(created_date,label = T,abbr = T),
         summer=month  %in% c("May","Jun","Jul","Aug","Sep")) %>% 
  filter(complaint_type=="Noise",borough=="MANHATTAN",grepl("NV1",descriptor),
         between(longitude,lm_limits$xmin,lm_limits$xmax),
         between(latitude,lm_limits$ymin,lm_limits$ymax),
         #grepl("previous",resolution_description)
  ) %>% 
  mutate(label =  paste0(date(created_date),
                         "<br>",
                         incident_address,
                         "<br>",
                         resolution_action_updated_date,
                         "<br>",
                         resolution_description))

lm_hvac_complaints %>% 
  ggplot() +
  geom_sf(data = lower_manhattan) +
  geom_point(aes(x=longitude,y=latitude,color=summer)) 


library(leaflet)
leaflet(data = lm_hvac_complaints %>% filter(summer,year(created_date)==2020)) %>%
  setView(lng = as.numeric(lm_limits$xmin), lat = as.numeric(lm_limits$ymax),zoom = 12) %>% 
  addProviderTiles(provider = "CartoDB") %>% 
  addMarkers(lng = ~longitude,
             lat = ~latitude, 
             #color = ~summer,
             popup = ~label)
