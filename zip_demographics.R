library(tidyverse)
library(magrittr)
library(STRAD)
library(tidycensus)

# 
# v_18 <- load_variables(2018,"acs5",cache=T) %>% mutate(table=str_sub(name,1,6))
# zcta_pop <- get_acs(geography = "zcta",variables = "B01003_001",year = 2018,geometry = F)   
# zcta_med_income = get_acs(geography = "zcta",variables = c( "B06011_001"),year = 2018)
# zcta_pov = get_acs(geography = "zcta",variables = c( "B17020_001","B17020_002"),year = 2018)

zcta_nyc_geom <- readRDS("zcta_nyc_geom.Rds")
zcta_med_income <- readRDS("zcta_med_income.Rds")
zcta_pop <- readRDS("zcta_pop.Rds")
zcta_pov <- readRDS("zcta_pov.Rds")

zcta_cv_nyc <- read_csv("https://raw.githubusercontent.com/nychealth/coronavirus-data/master/tests-by-zcta.csv",col_types = cols(
  MODZCTA = col_character(),
  Positive = col_double(),
  Total = col_double()
))



#first cut of vars is the low response predictor vars from census = check to see if these are the same variable names in 2018 ACS
#make varname table####
low_response_predictor_vars <- tribble(~indicator,~var_name,~var_denom_name,~note,~universe,
                                       "% black","B02001_003","B02001_001",NA,"population",
                                       "% Asian","B02001_005","B02001_001",NA,"population",
                                       "% Hispanic","B03003_003","B03003_001",NA,"population",
                                       "% foreign born","B05001_004","B05001_001","multi","population",
                                       "% foreign born","B05001_005","B05001_001","multi","population",
                                       "% foreign born","B05001_006","B05001_001","multi","population",
                                       "% Yiddish speakers","B16001_021" ,"B16001_001",NA,"??",
                                       "% renter occupied housing","B25003_003","B25003_001",NA,"households",
                                       "% non-family","B11016_009","B11016_001",NA,"households",
                                       "% poverty","B17020_002","B17020_001",NA,"population",
                                       "% above age 25 with no high school degree","B15003_002", "B15003_001","multi","pop 25 and over",
                                       "% above age 25 with no high school degree","B15003_003", "B15003_001","multi","pop 25 and over",
                                       "% above age 25 with no high school degree","B15003_004", "B15003_001","multi","pop 25 and over",
                                       "% above age 25 with no high school degree","B15003_005", "B15003_001","multi","pop 25 and over",
                                       "% above age 25 with no high school degree","B15003_006", "B15003_001","multi","pop 25 and over",
                                       "% above age 25 with no high school degree","B15003_007", "B15003_001","multi","pop 25 and over",
                                       "% above age 25 with no high school degree","B15003_008", "B15003_001","multi","pop 25 and over",
                                       "% above age 25 with no high school degree","B15003_009", "B15003_001","multi","pop 25 and over",
                                       "% above age 25 with no high school degree","B15003_010", "B15003_001","multi","pop 25 and over",
                                       "% above age 25 with no high school degree","B15003_011", "B15003_001","multi","pop 25 and over",
                                       "% above age 25 with no high school degree","B15003_012", "B15003_001","multi","pop 25 and over",
                                       "% above age 25 with no high school degree","B15003_013", "B15003_001","multi","pop 25 and over",
                                       "% above age 25 with no high school degree","B15003_014", "B15003_001","multi","pop 25 and over",
                                       "% above age 25 with no high school degree","B15003_015", "B15003_001","multi","pop 25 and over",
                                       "% above age 25 with no high school degree","B15003_016", "B15003_001","multi","pop 25 and over",
                                       "% less than ten years tenure in unit", "B25038_005","B25038_001","multi","households",
                                       "% less than ten years tenure in unit", "B25038_006","B25038_001","multi","households",
                                       "% less than ten years tenure in unit", "B25038_007","B25038_001","multi","households",
                                       "% less than ten years tenure in unit", "B25038_008","B25038_001","multi","households",
                                       "% less than ten years tenure in unit", "B25038_012","B25038_001","multi","households",
                                       "% less than ten years tenure in unit", "B25038_013","B25038_001","multi","households",
                                       "% less than ten years tenure in unit", "B25038_014","B25038_001","multi","households",
                                       "% less than ten years tenure in unit", "B25038_015","B25038_001","multi","households",
                                       "% single family headed","B11003_008","B11003_001",NA,"family households") %>% 
  left_join(v_18,by=c("var_name"="name")) %>% rename(var_label=label,var_concept=concept) %>% 
  left_join(v_18,by=c("var_denom_name"="name")) %>% rename(denom_label=label,denom_concept=concept)


# low_response_acs_17 <- get_acs(geography = "tract",variables = c(low_response_predictor_vars$var_name,unique(low_response_predictor_vars$var_denom_name)),state = "NY",county = c(061,005,085,047,081),year = 2017,geometry = F)     


#spread poverty and get perc
zcta_pov_wide <- zcta_pov %>% 
  mutate(part = ifelse(variable=="B17020_001","denom","num")) %>% 
  select(-variable) %>% 
  pivot_wider(names_from = part,values_from = c(estimate,moe)) %>% 
  mutate(perc_pov = estimate_num/estimate_denom,
         moe_perc_pov = moe_prop(estimate_num,estimate_denom,moe_num,moe_denom))



#start with median income, poverty
#readers
#convert zip code to zcta zip code
#get median income
#bin

tests <- zcta_nyc_geom %>% select(ZIPCODE,COUNTY) %>% 
  left_join(zcta_pop %>% select(ZIPCODE=GEOID,pop_18 = estimate, moe_pop_18=moe)) %>% 
  left_join(zcta_med_income %>% select(ZIPCODE=GEOID,median_income = estimate, moe_median_income=moe)) %>% 
  left_join(zcta_cv_nyc %>% rename(ZIPCODE=MODZCTA)) %>% 
  mutate(test_per_pop=Total/pop_18,pos_per_pop=Positive/pop_18,pos_per_test=Positive/Total) %>%   mutate(median_income_quartile=ntile(median_income,n = 4),
         median_income_bin = cut(median_income,breaks = c(0,25000,35000,45000,85000,250000),
                                 labels = c("0-25K","25-35K","35-45K","45-85K","85K+")),
         # perc_pov_quartile = ntile(perc_pov,n = 4),
         # perc_pov_bin = cut(perc_pov,breaks = c(0,.05,.1,.15,.2,.3,.4,1),
         #                    labels = c("0-5%","5-10%","10-15%","15-20%","20-30%","30-40%","40+%")),
         median_income_bin_alt1=cut(median_income,breaks = c(0,12500,25000,35000,45000,65000,85000,250000),
                                    labels = c("0-12.5K","12.5-25K","25-35K","35-45K","45-65K","65-85K","85K+")),
         median_income_bin_alt2=cut(median_income,breaks = c(15000,20000,25000,35000,45000,65000,85000,250000),
                                    labels = c("15-20K","20-25K","25-35K","35-45K","45-65K","65-85K","85K+")),
         pop_bin=cut(median_income,breaks = c(0,10000,30000,50000,70000,113000),
                                    labels = c("0-10K","10-30K","30-50K","50-70K","70-113K")))



#maps####
#function to map value
palette_maps <- "OrRd"
OrRd_cols <- RColorBrewer::brewer.pal(n = 5,palette_maps)

pos_per_test_map <- tests %>% 
  ggplot(aes(fill = pos_per_test)) + 
  geom_sf() +
  scale_fill_gradient(low = OrRd_cols[1],high = OrRd_cols[5],labels = scales::label_percent(),name="% of Tests Positive") + 
  theme_void() + 
  theme(legend.position = c(.25,.75),plot.background = element_rect(color = "black"),legend.key.size = unit(5,"mm"),
        plot.margin = margin(3,3,3,3)) 

ggsave("pos_per_test_map.png",pos_per_test_map,dpi = "retina")

pos_per_pop_map <- tests %>% 
  ggplot(aes(fill = pos_per_pop)) + 
  geom_sf() +
  scale_fill_gradient(low = OrRd_cols[1],high = OrRd_cols[5],
                      #labels = scales::label_percent(),
                      name="Positive Tests per capita") + 
  theme_void() + 
  theme(legend.position = c(.25,.75),plot.background = element_rect(color = "black"),legend.key.size = unit(5,"mm"),
        plot.margin = margin(3,3,3,3)) 

ggsave("pos_per_pop_map.png",pos_per_pop_map,dpi = "retina")

test_per_pop_map <- tests %>% 
  ggplot(aes(fill = test_per_pop)) + 
  geom_sf() +
  scale_fill_gradient(low = OrRd_cols[1],high = OrRd_cols[5],
                      #labels = scales::label_percent(),
                      name="Tests per capita") + 
  theme_void() + 
  theme(legend.position = c(.25,.75),plot.background = element_rect(color = "black"),legend.key.size = unit(5,"mm"),
        plot.margin = margin(3,3,3,3)) 

ggsave("test_per_pop_map.png",test_per_pop_map,dpi = "retina")

med_income_map <- tests %>% 
  ggplot(aes(fill = median_income_bin)) + 
  geom_sf() +
  scale_fill_manual(values = rev(OrRd_cols),
                      #labels = scales::label_percent(),
                      name="Zip median income ($)",na.translate = F) + 
  theme_void() + 
  theme(legend.position = c(.25,.75),plot.background = element_rect(color = "black"),legend.key.size = unit(5,"mm"),
        plot.margin = margin(3,3,3,3)) 

ggsave("med_income_map.png",med_income_map,dpi = "retina")


pop_map <- tests %>% 
  ggplot(aes(fill = pop_bin)) + 
  geom_sf() +
  scale_fill_brewer(palette = "Blues",
                    #labels = scales::label_percent(),
                    name="Zip population",na.translate = F) + 
  theme_void() + 
  theme(legend.position = c(.25,.75),plot.background = element_rect(color = "black"),legend.key.size = unit(5,"mm"),
        plot.margin = margin(3,3,3,3)) 

ggsave("pop_map.png",pop_map,dpi = "retina")

