library(tidyverse)
library(lubridate)
library(rvest)
library(magrittr)
library(STRAD)


# #initial data pull####
# #get all the commits - just manually filling in the strings initially
# commits_zip1 <-read_html("https://github.com/nychealth/coronavirus-data/commits/master/data-by-modzcta.csv")
# 
# commits_zip2 <- read_html("https://github.com/nychealth/coronavirus-data/commits/master?after=ce3f3a6952914b8222b3f495ccd69a7f3b881e45+34&branch=master&path%5B%5D=data-by-modzcta.csv")
# 
# commits_zip3 <- read_html("https://github.com/nychealth/coronavirus-data/commits/master?after=ce3f3a6952914b8222b3f495ccd69a7f3b881e45+69&branch=master&path%5B%5D=data-by-modzcta.csv")
# 
# #look for nodes with dates, except 6/24 upload doesn't have a date
# data_upload_strings_zip <- commits_zip1 %>% html_children() %>% extract2(2) %>% html_nodes("a") %>% extract(str_detect(.,"[:digit:]{1,2}/[:digit:]{1,2}")&str_detect(.,"coronavirus-data")) %>% as.character() %>% 
#   c(commits_zip2 %>% html_children() %>% extract2(2) %>% html_nodes("a") %>% extract(str_detect(.,"[:digit:]{1,2}/[:digit:]{1,2}|Add files via upload")&str_detect(.,"coronavirus-data")) %>% as.character()) %>% 
#   c(commits_zip3 %>% html_children() %>% extract2(2) %>% html_nodes("a") %>% extract(str_detect(.,"[:digit:]{1,2}/[:digit:]{1,2}")&str_detect(.,"coronavirus-data")) %>% as.character())
# 
# 
# 
# commit_df_zip <- data_upload_strings_zip %>% map_dfr(~tibble(date_string=str_extract(.x,"[:digit:]{1,2}/[:digit:]{1,2}"),commit_string=str_extract(.x,"nychealth/coronavirus-data/commit/[:alnum:]{40}") %>% str_remove("nychealth/coronavirus-data/commit/"))) %>%
#   mutate(date=mdy(paste0(str_extract(date_string,"[:digit:]{1,2}/[:digit:]{1,2}"),"/20")))
# 
# commit_df_zip %<>% mutate(date=case_when(commit_string=="be46b4e48653c29fab890473672cd7d7e0c7e59c"~ymd("2020-06-24"),T~date))
# 
# 
# c_v_zip_stacked <- commit_df_zip  %>% mutate(data=map(commit_string,~read_csv(paste0("https://raw.githubusercontent.com/nychealth/coronavirus-data/",.x,"/data-by-modzcta.csv"))))
# 
# c_v_zip_stacked %<>% unnest(cols = c(data)) 
# 
# c_v_zip_stacked %<>% mutate(MODIFIED_ZCTA=as.character(MODIFIED_ZCTA))
# 
# saveRDS(c_v_zip_stacked,"c_v_zip_stacked.Rds")


#add new####
c_v_zip_stacked <- readRDS("c_v_zip_stacked.Rds")
c_v_zip_stacked %>% count(date) %>% tail()

commits_zip <-read_html("https://github.com/nychealth/coronavirus-data/commits/master/data-by-modzcta.csv")

data_upload_strings_zip <- commits_zip %>% html_children() %>% extract2(2) %>% html_nodes("a") %>% extract(str_detect(.,"[:digit:]{1,2}/[:digit:]{1,2}")&str_detect(.,"coronavirus-data")) %>% as.character()

commit_df_zip_new <- data_upload_strings_zip %>% map_dfr(~tibble(date_string=str_extract(.x,"[:digit:]{1,2}/[:digit:]{1,2}"),commit_string=str_extract(.x,"nychealth/coronavirus-data/commit/[:alnum:]{40}") %>% str_remove("nychealth/coronavirus-data/commit/"))) %>%
  mutate(date=mdy(paste0(str_extract(date_string,"[:digit:]{1,2}/[:digit:]{1,2}"),"/20"))) %>% 
  filter(!date %in% c_v_zip_stacked$date)

c_v_zip_stacked_new <- commit_df_zip_new  %>% mutate(data=map(commit_string,~read_csv(paste0("https://raw.githubusercontent.com/nychealth/coronavirus-data/",.x,"/data-by-modzcta.csv"))))

c_v_zip_stacked_new %<>% unnest(cols = c(data)) 

c_v_zip_stacked_new %<>% mutate(MODIFIED_ZCTA=as.character(MODIFIED_ZCTA))

c_v_zip_stacked_new %>% count(date)

#c_v_zip_stacked_new %<>% filter(!commit_string=="7138e6906c9ee1117e2dac06656f0e1c03190370")
#ad to old####
c_v_zip_stacked %<>% bind_rows(c_v_zip_stacked_new) %>% arrange(desc(date))


#standardize neighborhood names - use most recent
zips_neighborhoods <- c_v_zip_stacked %>% 
  group_by(MODIFIED_ZCTA) %>% 
  filter(date==max(date)) %>% 
  ungroup() %>% 
  select(MODIFIED_ZCTA,borough=BOROUGH_GROUP,neighborhood=NEIGHBORHOOD_NAME) %>% 
  left_join(STRAD::zcta_demographics_all_acs_2018 %>% select(ZIPCODE,median_income,perc_pov,median_income_bin,perc_pov_bin,pop_18),by=c(MODIFIED_ZCTA="ZIPCODE"))


c_v_zip_stacked %<>% 
  select(-neighborhood) %>% 
  left_join(zips_neighborhoods %>% select(MODIFIED_ZCTA,neighborhood))

saveRDS(c_v_zip_stacked,"c_v_zip_stacked.Rds")



#plot/analyze####


c_v_zip_stacked %>% filter(MODIFIED_ZCTA==10012) %>% select(date,neighborhood,COVID_CASE_COUNT,MODIFIED_ZCTA) %>% head()

#plot 1 zip
c_v_zip_stacked %>% filter(MODIFIED_ZCTA==10012) %>% select(date,NEIGHBORHOOD_NAME,COVID_CASE_COUNT,MODIFIED_ZCTA) %>% ggplot(aes(x=date,y=COVID_CASE_COUNT)) + geom_line()

c_v_zip_stacked %>% filter(grepl("Village",neighborhood)|grepl("SoHo",neighborhood)) %>% group_by(date) %>% summarize(COVID_CASE_COUNT=sum(COVID_CASE_COUNT)) %>% ggplot(aes(x=date,y=COVID_CASE_COUNT)) + geom_line()


#plot borough neighborhoods (names change in june)
c_v_zip_stacked %>% 
  filter(BOROUGH_GROUP=="Bronx",date>=ymd("2020-08-01")) %>% 
  group_by(BOROUGH_GROUP,neighborhood,date) %>% 
  summarize(case_count=sum(COVID_CASE_COUNT)) %>% 
  ggplot(aes(x=date,y=case_count,color=neighborhood)) + 
  geom_line() + scale_color_discrete(guide = F)


#new cases week over week
zip_weekly_neighborhoods <- c_v_zip_stacked  %>% 
  select(-neighborhood) %>% 
  left_join(zips_neighborhoods) %>% 
  group_by(date,borough,neighborhood) %>% 
  summarize(COVID_CASE_COUNT=sum(COVID_CASE_COUNT),zips=n(),population=sum(POP_DENOMINATOR),
            pop2=sum(pop_18),perc_pov=sum(perc_pov*pop_18)/sum(pop_18)) %>% 
  ungroup() %>% 
  mutate(case_rate=COVID_CASE_COUNT/population,
         perc_pov_bin=cut(perc_pov,breaks = c(-1,.05,.1,.15,.2,.3,.4,.6),
                          labels = c('0-5%', '5-10%', '10-15%', '15-20%', '20-30%', '30-40%', '40+%'))) %>% 
  mutate(week=week(date)) %>% 
  filter(max(date)-date>=7) %>% 
  arrange(neighborhood,date) %>% 
  group_by(week,neighborhood) %>% 
  mutate(day_count=row_number()) %>% 
  filter(day_count==1) %>% 
  #filter(week  %in% 32:33) %>% 
  group_by(neighborhood) %>% 
  mutate(increase=COVID_CASE_COUNT-lag(COVID_CASE_COUNT)) %>% 
  ungroup() %>% 
  filter(!is.na(increase)) %>% 
  arrange(desc(increase)) %>% 
  select(date,borough,neighborhood,COVID_CASE_COUNT,increase,population,perc_pov,perc_pov_bin,case_rate) %>% 
  mutate(increase_per_thous_pop=1000*increase/population) %>% 
  arrange(desc(increase_per_thous_pop))


zip_weekly_neighborhoods %>% filter(borough=="Bronx",date>=ymd("2020-06-04")) %>% 
  ggplot(aes(x=date,y=increase_per_thous_pop,group = neighborhood,color=perc_pov_bin)) + 
  geom_line() +
  scale_color_discrete(guide=F)


zip_weekly_neighborhoods %>% filter(grepl("West Village",neighborhood)|grepl("East Village",neighborhood)|grepl("SoHo",neighborhood)) %>% 
  ggplot(aes(x=date,y=increase_per_thous_pop,color=neighborhood)) + geom_line()


zip_weekly_neighborhoods %>% 
  ggplot(aes(x=date,y=increase_per_thous_pop,group=neighborhood,color=perc_pov_bin)) + geom_line() +facet_grid(cols = vars(perc_pov_bin),rows = vars(borough),scales = "free") + scale_color_discrete(guide=F)


zip_weekly_neighborhoods %>% 
  filter(date>ymd("2020-06-03")) %>% 
  ggplot(aes(x=date,y=increase_per_thous_pop,group=neighborhood,color=perc_pov_bin)) + 
  geom_line() +
  facet_grid(cols = vars(perc_pov_bin),rows = vars(borough)) +
  scale_color_discrete(guide=F) +
  ylim(0,1.2)


zip_weekly_neighborhoods %>% 
  filter(date>ymd("2020-06-03")) %>% 
  ggplot(aes(x=date,y=case_rate,group=neighborhood,color=perc_pov_bin)) + 
  geom_line() +
  facet_grid(cols = vars(perc_pov_bin),rows = vars(borough)) +
  scale_color_discrete(guide=F) 


zip_ch_from_aug_1 <- c_v_zip_stacked %>% 
  filter(date %in% c(ymd("2020-08-01"),max(date))) %>% 
  group_by(MODIFIED_ZCTA,neighborhood,borough=BOROUGH_GROUP) %>% 
  arrange(MODIFIED_ZCTA,date) %>% 
  summarize(begin_cases=first(COVID_CASE_COUNT),end_cases=last(COVID_CASE_COUNT)) %>% 
  mutate(perc_ch=end_cases/begin_cases-1,
         perc_ch=ifelse(perc_ch<0,NA,perc_ch),
         ch=end_cases-begin_cases,
         label=paste0(MODIFIED_ZCTA,"<br>",
                      neighborhood,"<br>",
                      begin_cases,"/",end_cases,"<br>",
                      ch,"<br>",
                      format_perc(perc_ch,1))) %>% 
  ungroup()


STRAD::zcta_nyc_metro_geom %>% 
  filter(nyc) %>% 
  left_join(zip_ch_from_aug_1,by=c(ZIPCODE="MODIFIED_ZCTA")) %>% 
  ggplot(aes(fill=perc_ch)) +
  geom_sf()

fill_palette <- colorBin(palette = RColorBrewer::brewer.pal(5,"YlOrRd"),domain = zip_ch_from_aug_1$perc_ch)

fill_palette_ch <- colorBin(palette = RColorBrewer::brewer.pal(5,"YlOrRd"),domain = zip_ch_from_aug_1$ch)


leaflet(data = STRAD::zcta_nyc_metro_geom %>% 
          filter(nyc) %>% 
          left_join(zip_ch_from_aug_1,by=c(ZIPCODE="MODIFIED_ZCTA")) %>% 
          st_transform(4326)) %>% 
  setView(lng = as.numeric(lm_limits$xmin), lat = as.numeric(lm_limits$ymax),zoom = 12) %>% 
  addProviderTiles(provider = "CartoDB") %>% 
  addPolygons(
    fillColor = ~fill_palette_ch(ch),
    #fillColor = ~fill_palette(perc_ch),
              popup = ~label,
              opacity = .8,
              color = "grey",
              weight = 1)
  


#alt version - city's four week files####
#data released on thursday, updated through previous saturday
# so far: 10/1 file, updated through 9/26; 10/8 file, updated through 10/3

#initial pull####
# # #get all the commits
# commits_recent_zip <-read_html("https://github.com/nychealth/coronavirus-data/commits/master/recent")
# 
# data_upload_strings_recent_zip <- commits_recent_zip %>% html_children() %>% extract2(2) %>% html_nodes("a") %>% extract(str_detect(.,"data upload|data update")) %>% as.character()
# 
# commit_df_recent_zip <- data_upload_strings_recent_zip %>%
#   map_dfr(~tibble(date_string=str_extract(.x,"[:digit:]{1,2}/[:digit:]{1,2}"),commit_string=str_extract(.x,"nychealth/coronavirus-data/commit/[:alnum:]{40}") %>% str_remove("nychealth/coronavirus-data/commit/"))) %>%
#   mutate(date=mdy(paste0(str_extract(date_string,"[:digit:]{1,2}/[:digit:]{1,2}"),"/20"))) %>%
#   filter(date!=ymd(20200818))
# 
# recent_zip_history <- commit_df_recent_zip  %>% mutate(data=map(commit_string,~read_csv(paste0("https://raw.githubusercontent.com/nychealth/coronavirus-data/",.x,"/recent/recent-4-week-by-modzcta.csv")))) 
# 
# weekly_zcta_weeks <- tibble(week_end=min(recent_zip_history$date)-5-weeks(0:3),
#                             week_start=week_end-6) %>% 
#   bind_rows(tibble(week_end = recent_zip_history$date-5,
#                    week_start=week_end-6)) %>% 
#   distinct() %>% 
#   arrange(week_start) %>% 
#   mutate(week_num_abs=row_number()) %>% 
#   select(week_num_abs,week_start,week_end)
# 
# 
# make_recent_zcta_tall <- function(recent_zcta_df,upload_date){
#   weeks_ends <- (upload_date-5)-weeks(0:4)
#   recent_zcta_df %>%
#     select(-contains("4WEEK"),-PCT_CHANGE_2WEEK) %>%
#     pivot_longer(cols = -c(MODIFIED_ZCTA,NEIGHBORHOOD_NAME)) %>%
#     mutate(week_num_rel=str_extract(name,"WEEK[:digit:]{1}") %>% str_remove("WEEK") %>% as.numeric(),
#            week_end=weeks_ends[week_num_rel],
#            measure=str_remove(name,"_WEEK[:digit:]{1}") %>% str_remove("COVID_CASE_") %>% str_to_lower(),
#            iteration=recent_modzcta_iteration) %>%
#     left_join(weekly_zcta_weeks,by="week_end") 
#   
# }
# 
# recent_modzcta_tall <- recent_zip_history %>% 
#   mutate(data_tall=map2(data,date,~make_recent_zcta_tall(.x,.y))) %>% select(commit_string,data_tall) %>% unnest(data_tall) %>% 
#     left_join(commit_df_recent_zip %>% select(commit_string,upload_date=date))
# 
# saveRDS(recent_modzcta_tall,"recent_modzcta_tall.Rds")
# saveRDS(weekly_zcta_weeks,"weekly_zcta_weeks.Rds")

#add a week####
weekly_zcta_weeks <- readRDS("weekly_zcta_weeks.Rds")
recent_modzcta_tall <- readRDS("recent_modzcta_tall.Rds")

commits_recent_zip <-read_html("https://github.com/nychealth/coronavirus-data/commits/master/recent")

data_upload_strings_recent_zip <- commits_recent_zip %>% html_children() %>% extract2(2) %>% html_nodes("a") %>% extract(str_detect(.,"data upload|data update")) %>% as.character()

commit_df_recent_zip_new <- data_upload_strings_recent_zip %>%
  map_dfr(~tibble(date_string=str_extract(.x,"[:digit:]{1,2}/[:digit:]{1,2}"),commit_string=str_extract(.x,"nychealth/coronavirus-data/commit/[:alnum:]{40}") %>% str_remove("nychealth/coronavirus-data/commit/"))) %>% 
  mutate(date=mdy(paste0(str_extract(date_string,"[:digit:]{1,2}/[:digit:]{1,2}"),"/20"))) %>% 
  filter(date!=ymd(20200818),!commit_string %in% recent_modzcta_tall$commit_string)


weekly_zcta_weeks  %<>% 
  bind_rows(tibble(week_end = commit_df_recent_zip_new$date-5,
                   week_start=week_end-6,
                   week_num_abs=max(weekly_zcta_weeks$week_num_abs)+1)) 

make_recent_zcta_tall <- function(recent_zcta_df,upload_date){
  weeks_ends <- (upload_date-5)-weeks(0:4)
  recent_zcta_df %>%
    select(-contains("4WEEK"),-PCT_CHANGE_2WEEK) %>%
    pivot_longer(cols = -c(MODIFIED_ZCTA,NEIGHBORHOOD_NAME)) %>%
    mutate(week_num_rel=str_extract(name,"WEEK[:digit:]{1}") %>% str_remove("WEEK") %>% as.numeric(),
           week_end=weeks_ends[week_num_rel],
           measure=str_remove(name,"_WEEK[:digit:]{1}") %>% str_remove("COVID_CASE_") %>% str_to_lower()) %>%
    left_join(weekly_zcta_weeks,by="week_end") 
  
}

recent_modzcta_tall_new <- commit_df_recent_zip_new  %>% 
  mutate(data=map(commit_string,~read_csv(paste0("https://raw.githubusercontent.com/nychealth/coronavirus-data/",.x,"/recent/recent-4-week-by-modzcta.csv"))))

recent_modzcta_tall_new %<>% 
         mutate(data_tall=map2(data,date,~make_recent_zcta_tall(.x,.y))) %>% 
  select(commit_string,data_tall) %>% unnest(data_tall) %>% 
  left_join(commit_df_recent_zip %>% select(commit_string,upload_date=date))

recent_modzcta_tall_new

recent_modzcta_tall %<>% bind_rows(recent_modzcta_tall_new)

saveRDS(weekly_zcta_weeks,"weekly_zcta_weeks.Rds")

#analyze####
weekly_modzcta_latest <- recent_modzcta_tall %>% group_by(measure,MODIFIED_ZCTA,week_num_abs) %>% slice(which.max(upload_date)) %>% ungroup()

#these are the initially proposed shutdown zips, superseded by hotspots defined as polygons
zips_shutdown <- tibble(ZCTA= c(11691, 11219, 11223, 11230, 11204, 11210, 11229, 11415, 11367),
                        action = "full") %>% 
  bind_rows(tibble(ZCTA= c(11205, 11211, 11249, 11235, 11234, 11213, 11218, 11374, 11366, 11432 , 11365),
                   action = "limited"))

weekly_by_target <- weekly_modzcta_latest %>% left_join(zips_shutdown,by=c(MODIFIED_ZCTA="ZCTA")) %>% 
  mutate(shutdown=factor(replace_na(action,"none"))) %>% 
  group_by(shutdown,week_num_abs) %>% 
  filter(measure=="count") %>% 
  summarize(cases=sum(value)) %>%
  spread(shutdown,cases)

weekly_by_target %>% 
  mutate_at(vars(full,limited,none),~.x/lag(.x)-1)


#cases
weekly_modzcta_latest %>% left_join(zips_shutdown,by=c(MODIFIED_ZCTA="ZCTA")) %>% 
  group_by(action,week_num_abs,week_start) %>% 
  filter(measure=="count") %>% 
  summarize(cases=sum(value)) %>% 
  mutate(shutdown=factor(replace_na(action,"none"))) %>% 
  ggplot(aes(x=week_start,y=cases,color=shutdown)) +
  geom_line(size=3)
  
#rates
weekly_modzcta_latest %>% left_join(zips_shutdown,by=c(MODIFIED_ZCTA="ZCTA")) %>% 
  group_by(action,week_num_abs,week_start) %>% 
  filter(measure=="rate") %>% 
  summarize(mean_rate=mean(value)) %>% 
  mutate(shutdown=factor(replace_na(action,"none"))) %>% 
  ggplot(aes(x=week_start,y=mean_rate,color=shutdown)) +
  geom_line(size=3)


weekly_modzcta_latest %>% left_join(zips_shutdown,by=c(MODIFIED_ZCTA="ZCTA")) %>% 
  group_by(action,week_num_abs,week_start) %>% 
  filter(measure=="rate",week_num_abs==max(week_num_abs)) %>% 
  mutate(shutdown=factor(replace_na(action,"none"))) %>% 
  ggplot(aes(x=value,group=shutdown,fill=shutdown)) +
  geom_density(color="transparent",alpha = .5)
  

weekly_modzcta_latest %>% left_join(zips_shutdown,by=c(MODIFIED_ZCTA="ZCTA")) %>% 
  filter(week_num_abs==max(week_num_abs),measure=="rate") %>% 
  arrange(desc(value)) %>% 
  group_by(action) %>% 
  top_n(10,value) %>% 
  mutate(rank=row_number()) %>% 
  select(rank,value,action) %>% 
  spread(action,value)
  
library(mapboxapi)
library(leaflet)


leaflet() %>% 
  addMapboxTiles(username = "srhrnkn",style_id = "light-v9") %>% 
  setView(lng = addresses$Longitude[1],lat = addresses$Latitude[1],zoom = 13)


library(sf)
zcta_nyc_metro_geom %>% 
  filter(nyc) %>% 
  left_join(weekly_modzcta_latest %>% filter(measure=="rate") %>% mutate(ZIPCODE=as.character(MODIFIED_ZCTA))) %>% 
  ggplot(aes(fill=value)) +
  geom_sf()
  

zcta_nyc_metro_geom %>% 
  filter(nyc) %>% 
  left_join(zips_shutdown  %>% mutate(ZIPCODE=as.character(ZCTA))) %>% 
  mutate(shutdown=factor(replace_na(action,"none"))) %>% 
  ggplot(aes(fill=shutdown)) +
  geom_sf()
