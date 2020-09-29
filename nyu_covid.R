#nyu

library(tidyverse)
library(lubridate)
library(rvest)
library(magrittr)

#dashboard page is https://www.nyu.edu/life/safety-health-wellness/coronavirus-information/nyc-covid-19-testing-data.html


#use download function on barchart, select daily test result crosstab
#need to copy paste it into another text file, weird characters
#tests <- read_lines("../../Downloads/Daily Test Results_crosstab (1).csv")
nyu_tests <- read_delim("testresults3.txt",delim = "\t")
nyu_tests %<>% janitor::clean_names()

nyu_tests %<>% distinct()

nyu_tests %<>% mutate(date=mdy(selected_time))
nyu_tests %>% tail()

nyu_tests %>% mutate(perc_pos=total_positive_cases_identified_via_nyu_testing_centers/total_tests_at_an_nyu_testing_center) %>%  select(date,total_tests_at_an_nyu_testing_center,total_positive_cases_identified_via_nyu_testing_centers,perc_pos) %>% tail(14) %>% summarize(start=min(date),end=max(date),positives=sum(total_positive_cases_identified_via_nyu_testing_centers))

tibble(date=seq.Date(min(nyu_tests$date),max(nyu_tests$date),by = 1))  %>% 
  left_join(nyu_tests) %>% 
  select(date,daily_positive=total_positive_cases_identified_via_nyu_testing_centers) %>% 
  mutate(cumul_positive=cumsum(daily_positive),
         rolling_14=map_dbl(date,~sum(daily_positive[between(date,.x-days(13),.x)],na.rm=T))) %>% 
  pivot_longer(cols = c(daily_positive,cumul_positive,rolling_14)) %>% ggplot(aes(x=date,y=value,color=name)) + geom_line()



tibble(date=seq.Date(min(nyu_tests$date),max(nyu_tests$date),by = 1))  %>% 
  left_join(nyu_tests) %>% 
  mutate(perc_pos=total_positive_cases_identified_via_nyu_testing_centers/total_tests_at_an_nyu_testing_center) %>%  
  select(date,perc_pos) %>% 
  ggplot(aes(x=date,y=perc_pos)) + 
  geom_point(size=3) + 
  scale_y_continuous(labels = scales::percent_format())
