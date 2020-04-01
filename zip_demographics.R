library(tidyverse)
library(magrittr)
library(STRAD)
library(tidycensus)

# library(extrafont)
# 
# #load fonts
# font_import(paths = "../KievitforNYPL2016")
# loadfonts()

reader_geog <- readRDS("reader_geog.Rds")
nyc_zip_correspondence_use <- readRDS("nyc_zip_correspondence_use.Rds")
print_checkouts_2020 <- readRDS("print_checkouts_2020.Rds")



v_18 <- load_variables(2018,"acs5",cache=T) %>% mutate(table=str_sub(name,1,6))
zcta_pop <- get_acs(geography = "zcta",variables = "B01003_001",year = 2018,geometry = F)   
zcta_med_income = get_acs(geography = "zcta",variables = c( "B06011_001"),year = 2018)
zcta_pov = get_acs(geography = "zcta",variables = c( "B17020_001","B17020_002"),year = 2018)

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

#kids####
#figure out who is reading kids books
juv_checkouts <- print_checkouts_2020 %<>% 
  mutate(LocationAge = get_itype_info(itype_code_num,return_value = "LocationAge"),
         item_coll = get_item_coll(item_location_code)) %>% 
  filter(LocationAge=="Juvenile")

patrons_juv_checkouts <- juv_checkouts %>% 
  group_by(patron_id) %>% 
  summarize(juv_checkout_count = n(), 
            juv_checkout_any = T,
            picture_checkout_count = sum(item_coll=="Picture Book",na.rm=T),
            picture_checkout_any = picture_checkout_count>0) %>% 
  ungroup()

#start with median income, poverty
#readers
#convert zip code to zcta zip code
#get median income
#bin
#summarize by e, p, both
reader_geog_demo <- reader_geog %>% 
  mutate(zip_use = coalesce(nyc_zip_correspondence_use$zcta_nyc_geom_ZIPCODE[match(postal_code,nyc_zip_correspondence_use$nyc_zip_geom_ZIPCODE)],postal_code)) %>%
  left_join(patrons_juv_checkouts %>% select(patron_id,juv_checkout_any,picture_checkout_any)) %>% 
  mutate(juv_checkout_any=case_when(total.p>0&juv_checkout_any~juv_checkout_any,
                                    total.p>0~F,
                                    T~NA),
         picture_checkout_any=case_when(total.p>0&picture_checkout_any~picture_checkout_any,
                                    total.p>0~F,
                                    T~NA)) %>% 
  left_join(zcta_med_income %>% select(zip_use = GEOID,median_income = estimate, moe_median_income=moe)) %>% 
  left_join(zcta_pov_wide %>% select(zip_use = GEOID,perc_pov,moe_perc_pov)) %>% 
  left_join(zcta_pop %>% select(zip_use = GEOID,pop_18 = estimate, moe_pop_18=moe)) %>% 
  left_join(zcta_nyc_geom %>% st_drop_geometry() %>% select(zip_use=ZIPCODE,county=COUNTY,NYPL) %>% 
              left_join(county_borough %>% add_row(county=c("Queens","Kings"),borough=c("Queens","Brooklyn"))))


  mutate(perc_e=(`e only`+`p+e`)/total_readers,
         median_income_quartile=ntile(median_income,n = 4),
         median_income_bin = cut(median_income,breaks = c(0,25000,35000,45000,85000,250000),
                                 labels = c("0-25K","25-35K","35-45K","45-85K","85K+")),
         perc_pov_quartile = ntile(perc_pov,n = 4),
         perc_pov_bin = cut(perc_pov,breaks = c(0,.05,.1,.15,.2,.3,.4,1),
                            labels = c("0-5%","5-10%","10-15%","15-20%","20-30%","30-40%","40+%")),
         median_income_bin_alt1=cut(median_income,breaks = c(0,12500,25000,35000,45000,65000,85000,250000),
                                    labels = c("0-12.5K","12.5-25K","25-35K","35-45K","45-65K","65-85K","85K+")),
         median_income_bin_alt2=cut(median_income,breaks = c(15000,20000,25000,35000,45000,65000,85000,250000),
                                    labels = c("15-20K","20-25K","25-35K","35-45K","45-65K","65-85K","85K+")))



#maps####
#function to map value
palette_maps <- "OrRd"

med_income_map <- zcta_nyc_geom %>% 
  filter(NYPL) %>% 
  left_join(reader_e_p_demo_byzip %>% select(ZIPCODE = zip_use,median_income_bin)) %>% 
  ggplot(aes(fill = median_income_bin)) + 
  geom_sf() +
  scale_fill_brewer(palette = palette_maps,direction = -1,name = "",na.translate=FALSE) + 
  theme_void() + 
  theme(legend.position = c(.25,.75),plot.background = element_rect(color = "black"),legend.key.size = unit(5,"mm"),
        plot.margin = margin(3,3,3,3),title  = element_text(family = "KievitforNYPL")) +
  labs(title = "Median income by zip")

perc_ebook_map <- zcta_nyc_geom %>% 
  filter(NYPL) %>% 
  left_join(reader_e_p_demo_byzip %>% 
              mutate(perc_ebook_bin = cut(perc_e,breaks = c(0,.2,.4,.6,.8,1),
                                          labels = c("0-20%","20-40%","40-60%","60-80%","80%+")),
                     perc_ebook_bin2 = cut(perc_e,breaks = c(0,.15,.3,.45,.6,1),
                                           labels = c("0-15%","15-30%","30-45%","45-60%","60%+"))
                     ) %>% 
              select(ZIPCODE = zip_use,perc_ebook_bin2)) %>% 
  ggplot(aes(fill = perc_ebook_bin2)) + 
  geom_sf() +
  scale_fill_brewer(palette = palette_maps,direction = -1,name = "",na.translate=FALSE) + 
  theme_void() + 
  theme(legend.position = c(.25,.75),plot.background = element_rect(color = "black"),legend.key.size = unit(5,"mm"),
        plot.margin = margin(3,3,3,3),title = element_text(family = "KievitforNYPL")) +
  labs(title = "% of readers\nchecking out ebooks",name = "")


readers_map <- zcta_nyc_geom %>% 
  filter(NYPL) %>% 
  left_join(reader_e_p_demo_byzip %>% 
              mutate(readers_perc_pop=total_readers/pop_18,
                     readers_perc_pop_bin = cut(readers_perc_pop,breaks = c(0,.02,.04,.06,.8,1),
                                          labels = c("0-2%","2-4%","4-6%","6-8%","8%+"))
              ) %>% 
              select(ZIPCODE = zip_use,readers_perc_pop_bin)) %>% 
  ggplot(aes(fill = readers_perc_pop_bin)) + 
  geom_sf() +
  scale_fill_brewer(palette = palette_maps,direction = -1,name = "",na.translate=FALSE) + 
  theme_void() + 
  theme(legend.position = c(.25,.75),plot.background = element_rect(color = "black"),legend.key.size = unit(5,"mm"),
        plot.margin = margin(3,3,3,3),title = element_text(family = "KievitforNYPL")) +
  labs(title = "% of population\nchecking out books")

juv_usage_map <- zcta_nyc_geom %>% 
  filter(NYPL) %>% 
  left_join(reader_p_juv_demo_byzip_juv %>% 
              mutate(perc_w_juv_checkout = cut(perc_w_juv_checkout,breaks = c(.2,.3,.4,.5,.6,1),
                                          labels = c("20-30%","30-40%","40-50%","50-60%","60%+"))
              ) %>% 
              select(ZIPCODE = zip_use,perc_w_juv_checkout)) %>% 
  ggplot(aes(fill = perc_w_juv_checkout)) + 
  geom_sf() +
  scale_fill_brewer(palette = palette_maps,direction = -1,name = "",na.translate=FALSE) + 
  theme_void() + 
  theme(legend.position = c(.25,.75),plot.background = element_rect(color = "black"),legend.key.size = unit(5,"mm"),
        plot.margin = margin(3,3,3,3),title = element_text(family = "KievitforNYPL")) +
  labs(title = "% of print readers\nchecking out children's books")

#library(patchwork)
zip_demo_maps <- med_income_map + perc_ebook_map + readers_map + juv_usage_map  

ggsave("zip_demo_maps.png",zip_demo_maps,dpi = "retina",height = 10,width = 7)

#scatterplots####



juv_medincome_scatter <- reader_p_juv_demo_byzip_juv %>% 
  filter(residency_cat=="NYPL boroughs") %>% 
  ggplot(aes(x=median_income,y = perc_w_juv_checkout,color = borough,size = total_print_readers)) + 
  geom_point(alpha = .8) + 
  scale_color_manual(values = borough_cols,name = "Borough",guide = guide_legend(order=1,override.aes = list(size=3))) + 
  scale_size_continuous(name = "Print Readers\n in Zip Code",breaks = c(100,1000,4000)) +
  scale_x_continuous(labels = scales::label_dollar(), name= "\nZip Code Median Income") +
  scale_y_continuous(breaks = c(.2,.3,.4,.5,.6,.7),labels = scales::label_percent(), name = "",limits = c(.15,.75)) +
  theme_minimal() +
  theme(text = element_text(family = "Helvetica"),plot.subtitle = element_text(hjust = .5)) +
  labs(title = "Percent of print readers checking out\n  children's books",subtitle = "NYPL Zip Codes")

ggsave("juv_medincome_scatter.png",juv_medincome_scatter,dpi = "retina",height = 4,width = 4)

e_p_medincome_scatter <- reader_e_p_demo_byzip %>% 
  filter(residency_cat=="NYPL boroughs") %>% 
  ggplot(aes(x=median_income,y = perc_e,color = borough,size = total_readers)) + 
  geom_point(alpha = .8) + 
  scale_color_manual(values = borough_cols,name = "Borough",guide = guide_legend(order=1,override.aes = list(size=3))) + 
  scale_size_continuous(name = "Readers\n in Zip Code",
                        breaks = c(500,1500,6000)
                        ) +
  scale_x_continuous(labels = scales::label_dollar(), name= "\nZip Code Median Income") +
  scale_y_continuous(breaks = c(.2,.3,.4,.5,.6,.7),labels = scales::label_percent(), name = "",limits = c(.15,.75)) +
  theme_minimal() +
  theme(text = element_text(family = "Helvetica"),plot.subtitle = element_text(hjust = .5)) +
  labs(title = "Percent of readers checking out\n  ebooks",subtitle = "NYPL Zip Codes")

ggsave("e_p_medincome_scatter.png",e_p_medincome_scatter,dpi = "retina",height = 4,width = 4)




