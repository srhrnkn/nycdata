library(tidyverse)
library(lubridate)
library(rvest)
library(magrittr)

#initial data pull####
# #get all the commits
# commits <-read_html("https://github.com/nychealth/coronavirus-data/commits/master/case-hosp-death.csv")
# 
# data_upload_strings <- commits %>% html_children() %>% extract2(2) %>% html_nodes("a") %>% extract(str_detect(.,"data upload")&str_detect(.,"coronavirus-data")) %>% as.character()
# 
# commit_df <- data_upload_strings %>% map_dfr(~tibble(date_string=str_extract(.x,"[:digit:]{1,2}/[:digit:]{1,2} data upload"),commit_string=str_extract(.x,"nychealth/coronavirus-data/commit/[:alnum:]{40}") %>% str_remove("nychealth/coronavirus-data/commit/"))) %>% 
#   mutate(date=mdy(paste0(str_extract(date_string,"[:digit:]{1,2}/[:digit:]{1,2}"),"/20")))
# #add june 24
# commit_df %<>% mutate(date=case_when(is.na(date)&lag(date)==ymd("2020-06-25")&lead(date)==ymd("2020-06-23")~ymd("2020-06-24"),T~date))
# 
# c_v_stacked <- commit_df  %>% mutate(data=map(commit_string,~read_csv(paste0("https://raw.githubusercontent.com/nychealth/coronavirus-data/",.x,"/case-hosp-death.csv")))) 
# 
# c_v_stacked %<>% unnest(cols = c(data)) %>% mutate(obs_date=mdy(DATE_OF_INTEREST)) %>% 
#   select(-date_string) %>% add_count(obs_date,name = "count_obs") %>% 
#   add_count(obs_date,CASE_COUNT,name = "count_obs_case")
# 
# c_v_stacked %<>% rename(file_date=date) %>% 
#   mutate(file_date=case_when(commit_string=="be46b4e48653c29fab890473672cd7d7e0c7e59c"~ymd("2020-06-24"),T~file_date))
# 
# saveRDS(c_v_stacked,"c_v_stacked.Rds")

#add days####
c_v_stacked <- readRDS("c_v_stacked.Rds")

commits <-read_html("https://github.com/nychealth/coronavirus-data/commits/master/case-hosp-death.csv")

data_upload_strings <- commits %>% html_children() %>% extract2(2) %>% html_nodes("a") %>% extract(str_detect(.,"[:digit:]{1,2}/[:digit:]{1,2}")&str_detect(.,"coronavirus-data")) %>% as.character()

new_commit_df <- data_upload_strings %>% map_dfr(~tibble(date_string=str_extract(.x,"[:digit:]{1,2}/[:digit:]{1,2}"),commit_string=str_extract(.x,"nychealth/coronavirus-data/commit/[:alnum:]{40}") %>% str_remove("nychealth/coronavirus-data/commit/"))) %>% 
  mutate(date=mdy(paste0(str_extract(date_string,"[:digit:]{1,2}/[:digit:]{1,2}"),"/20")),
         date=case_when(is.na(date)&lag(date)==ymd("2020-06-25")&lead(date)==ymd("2020-06-23")~ymd("2020-06-24"),T~date)) %>% 
  filter(!date  %in% c_v_stacked$file_date)


c_v_stacked_new <- new_commit_df  %>% mutate(data=map(commit_string,~read_csv(paste0("https://raw.githubusercontent.com/nychealth/coronavirus-data/",.x,"/case-hosp-death.csv")))) 

c_v_stacked_new %<>% unnest(cols = c(data)) %>% mutate(obs_date=mdy(DATE_OF_INTEREST)) %>% 
  select(-date_string) %>% add_count(obs_date,name = "count_obs") %>% 
  add_count(obs_date,CASE_COUNT,name = "count_obs_case") %>%  rename(file_date=date)

c_v_stacked_new %>% count(file_date)


#add to old####
c_v_stacked %<>% bind_rows(c_v_stacked_new) %>% arrange(desc(file_date),desc(obs_date))

#two commits on 9/8, remove earlier
#c_v_stacked %<>% filter(!commit_string=="7138e6906c9ee1117e2dac06656f0e1c03190370")


saveRDS(c_v_stacked,"c_v_stacked.Rds")

#just get the latest file and count by week
c_v_stacked %>% 
  filter(file_date==max(file_date)) %>% 
  mutate(week=week(obs_date)) %>% 
  group_by(week) %>% 
  summarize(days=n(),cases=sum(CASE_COUNT),week_start=min(obs_date),week_end=max(obs_date)) %>% 
  tail(10)

#same but also count by file date
cases_weeks_file_dates <- c_v_stacked %>% 
  mutate(week=week(obs_date)) %>% 
  group_by(file_date,week) %>% 
  summarize(days=n(),cases=sum(CASE_COUNT),week_start=min(obs_date),week_end=max(obs_date)) %>% 
  filter(days==7) %>% 
  group_by(week) %>% 
  arrange(week,file_date) %>% 
  mutate(obs_number=row_number()) %>% 
  ungroup()

cases_weeks_file_dates %>% 
  filter(week>30) %>% 
  ggplot(aes(x=obs_number,y=cases,group=as.character(week_start),color=as.character(week_start))) +
  geom_line(size=5,alpha=.9) + 
  #geom_text(data = )
  scale_x_continuous(breaks = c(7,14,21),name = "days since first complete week of data uploaded") +
  scale_color_manual(values = RColorBrewer::brewer.pal(7,"BuPu"),name="week of") +
  theme_minimal() +
  labs(subtitle = "NYC weekly cases by data upload date")
ggsave("cases_by_upload.png")


#all full weeks 
cases_weeks_file_dates %>% 
  filter(week>24) %>% 
  ggplot(aes(x=obs_number,y=cases,group=week_start,color=week_start)) +
  geom_line(size=5,alpha=.9) + 
  #geom_text(data = )
  scale_x_continuous(breaks = c(7,14,21)) +
  scale_color_date(breaks = cases_weeks_file_dates %>% filter(week>24) %>% distinct(week,week_start) %>% pull(week_start)) +
  #scale_color_manual(values = RColorBrewer::brewer.pal(5,"BuPu"),name="week") +
  theme_minimal()


#by day, facet by weekday
c_v_stacked %>% 
  group_by(obs_date) %>% 
  arrange(file_date) %>% 
  mutate(obs_number=row_number()) %>%
  ungroup() %>% 
  filter(obs_date > (max(obs_date,na.rm=T)-months(1))) %>% 
  mutate(wday=wday(obs_date,label = T),
         week = ceiling((as.numeric(max(obs_date,na.rm = T)-obs_date)+1)/7)) %>% 
  group_by(week) %>% 
  mutate(week_start=as.character(min(obs_date))) %>% 
  ggplot(aes(x=obs_number,y=CASE_COUNT,group=obs_date,color=week_start)) +
  geom_line(size=5,alpha=.8) + 
  #geom_text(data = )
  scale_x_continuous(breaks = c(7,14,21)) +
  scale_color_manual(values = RColorBrewer::brewer.pal(5,"BuPu")) +
  facet_wrap(~wday) + 
  theme_minimal()

#Show weekly totals by obs date
c_v_stacked %>% 
  group_by(obs_date) %>% 
  arrange(file_date) %>% 
  mutate(obs_number=row_number()) %>%
  ungroup() %>% 
  filter(obs_date > (max(obs_date,na.rm=T)-days(28)),obs_number>=7) %>% 
  mutate(wday=wday(obs_date,label = T),
         week = ceiling((as.numeric(max(obs_date,na.rm = T)-obs_date)+1)/7)) %>% 
  group_by(week) %>% 
  mutate(week_start=as.character(min(obs_date))) %>% 
  group_by(obs_number,week_start) %>% 
  summarize(weekly_cases=sum(CASE_COUNT),days=length(unique(obs_date))) %>%   
  ggplot(aes(x=obs_number,y=weekly_cases,color=week_start)) +
  geom_line(size=5,alpha=.8) + 
  #geom_text(data = )
  scale_x_continuous(breaks = c(7,14,21)) +
  scale_color_manual(values = RColorBrewer::brewer.pal(5,"BuPu")) + 
  theme_minimal()




c_v_stacked %>% 
  group_by(obs_date) %>% 
  arrange(file_date) %>% 
  mutate(obs_number=row_number())

#start over. get weekly pulls, limit to full weeks, compare weekly totals

max_date <- max(c_v_stacked$obs_date,na.rm = T)

c_v_stacked %>% 
  filter((interval(max(file_date), file_date) %>% divide_by(weeks(1)) %>% `%%`(1))==0) %>% 
  filter(obs_date>=min(file_date)-days(7)) %>% 
  mutate(week=ceiling(interval(min(obs_date)-days(1),obs_date) %>% divide_by(weeks(1)))) %>%   
  group_by(file_date,week) %>% 
  summarize(first=min(obs_date),last=max(obs_date),days=n(),cases=sum(CASE_COUNT))


