####
# Hospital admissions forcare home residents (with MPI flag or based on ADMISORC/DISDEST)
# Process data split by care home type and covid primary diagnosis, or admission tyoe, generated by RW
####

library(tidyverse)
library(tidylog)
library(janitor)
library(lubridate)
library(readxl)

library(zoo)
library(data.table)

source("file_paths.R")
source("functions.R")

# COVID admissions -------------------------------------------------------------

admissions_covid <- read_xlsx(str_c(raw_data_path, "April 2020/01c Care Home Admissions Aggregates.xlsx"),
                               sheet = "2019-20 CH Adm by Type Covid")

admissions_covid <- admissions_covid %>% 
  mutate(spellstartdate = as.Date(spellstartdate, format = "%y-%m-%d"),
         day_dummy = `year<-`(spellstartdate, 0004),
         week = lubridate::isoweek(spellstartdate))


# COVID admissions from all care homes - weekly 
admissions_covid %>% 
  filter(spellstartdate >= ymd("2020-03-01")) %>% 
  group_by(admyr, week, covid_prim) %>% 
  summarise(ch_adm = sum(freq),
            week_start = min(spellstartdate),
            week_end = max(spellstartdate)) %>% 
  group_by(admyr, week) %>% 
  mutate(percent = round(100*ch_adm/ sum(ch_adm), 2)) %>% 
  write_csv(str_c(results_path_sprint3, "CH_admissions_covid_weekly.csv"))

# COVID admissions from all care homes - beginning of March to end of April
admissions_covid %>% 
  filter(spellstartdate >= ymd("2020-03-01")) %>% 
  group_by(admyr, covid_prim) %>% 
  summarise(ch_adm = sum(freq)) %>% 
  group_by(admyr) %>% 
  mutate(percent = round(100*ch_adm/ sum(ch_adm), 2)) %>% 
  write_csv(str_c(results_path_sprint3, "CH_admissions_covid_March-April.csv"))

# COVID admissions by care home type - weekly 
admissions_covid %>% 
  filter(spellstartdate >= ymd("2020-03-01")) %>% 
  group_by(admyr, week, ch_nursing_adm, covid_prim) %>% 
  summarise(ch_adm = sum(freq),
            week_start = min(spellstartdate),
            week_end = max(spellstartdate)) %>% 
  group_by(admyr, week, ch_nursing_adm) %>% 
  mutate(percent = round(100*ch_adm/ sum(ch_adm), 2)) %>% 
  write_csv(str_c(results_path_sprint3, "CH_admissions_covid_nursing_weekly.csv"))

# COVID admissions from all care homes - beginning of March to end of April
admissions_covid %>% 
  filter(spellstartdate >= ymd("2020-03-01")) %>% 
  group_by(admyr, ch_nursing_adm, covid_prim) %>% 
  summarise(ch_adm = sum(freq)) %>% 
  group_by(admyr, ch_nursing_adm) %>% 
  mutate(percent = round(100*ch_adm/ sum(ch_adm), 2)) %>% 
  write_csv(str_c(results_path_sprint3, "CH_admissions_covid_nursing_March-April.csv"))


# Elective vs. emergency ---------------------------------------------------


admissions_type <- read_csv(str_c(raw_data_path, "April 2020/01d Care Home Admissions Elective Emergency 1420.csv"))


admissions_type <- admissions_type %>% 
  select(-chadm) %>% 
  mutate(spellstartdate = as.Date(spellstartdate, format = "%d/%m/%Y"),
         day_dummy = `year<-`(spellstartdate, 0004),
         week = lubridate::isoweek(spellstartdate),
         adm_type = case_when(emergency == 1 ~ "emergency",
                              elective == 1 ~ "elective",
                              emergency == 0 & elective == 0 ~ "other"))

# Elective vs. emergency admissions from all care homes - weekly 
admissions_type %>% 
  filter(day_dummy >= ymd("0004-03-01") & day_dummy <= ymd("0004-04-30")) %>% 
  group_by(admyr, week, adm_type) %>% 
  summarise(ch_adm = sum(freq),
            week_start = min(spellstartdate),
            week_end = max(spellstartdate)) %>% 
  group_by(admyr, week) %>% 
  mutate(percent = round(100*ch_adm/ sum(ch_adm), 2)) %>% 
  write_csv(str_c(results_path_sprint3, "CH_admissions_type_weekly.csv"))

# Elective vs. emergency admissions from all care homes - beginning of March to end of April
admissions_type %>% 
  filter(day_dummy >= ymd("0004-03-01")& day_dummy <= ymd("0004-04-30")) %>% 
  group_by(admyr, adm_type) %>% 
  summarise(ch_adm = sum(freq)) %>% 
  group_by(admyr) %>% 
  mutate(percent = round(100*ch_adm/ sum(ch_adm), 2)) %>% 
  write_csv(str_c(results_path_sprint3, "CH_admissions_type_March-April.csv"))

# Elective vs. emergency admissions by care home type - weekly 
admissions_type %>% 
  filter(day_dummy >= ymd("0004-03-01") & day_dummy <= ymd("0004-04-30")) %>% 
  group_by(admyr, week, ch_nursing_adm, adm_type) %>% 
  summarise(ch_adm = sum(freq),
            week_start = min(spellstartdate),
            week_end = max(spellstartdate)) %>% 
  group_by(admyr, week, ch_nursing_adm) %>% 
  mutate(percent = round(100*ch_adm/ sum(ch_adm), 2)) %>% 
  write_csv(str_c(results_path_sprint3, "CH_admissions_type_nursing_weekly.csv"))

# Elective vs. emergency admissions from all care homes - beginning of March to end of April
admissions_type %>% 
  filter(day_dummy >= ymd("0004-03-01") & day_dummy <= ymd("0004-04-30")) %>% 
  group_by(admyr, ch_nursing_adm, adm_type) %>% 
  summarise(ch_adm = sum(freq)) %>% 
  group_by(admyr, ch_nursing_adm) %>% 
  mutate(percent = round(100*ch_adm/ sum(ch_adm), 2)) %>% 
  write_csv(str_c(results_path_sprint3, "CH_admissions_type_nursing_March-April.csv"))


# Elective vs. emergency from nursing vs. residential care homes - beginning of March to end of April
# relative to mean 2015 to 2019
admissions_type %>% 
  filter(day_dummy >= ymd("0004-03-01") & day_dummy <= ymd("0004-04-30") & admyr != "2014") %>% 
  group_by(admyr, ch_nursing_adm, adm_type) %>% 
  summarise(ch_adm = sum(freq),
            spellstartdate_start = min(spellstartdate),
            spellstartdate_end = max(spellstartdate)) %>% 
  group_by(ch_nursing_adm, adm_type) %>% 
  mutate(mean_ch_adm_2015_to_2019 = mean(ch_adm[admyr %in% c(2015:2019)]),
         pct_change_2015_to_2019 = round(100 * ch_adm / mean_ch_adm_2015_to_2019, 1)) %>% 
  write_csv(str_c(results_path_sprint3, "CH_admissions_type_nursing_March-April_rel2015-2019.csv"))


# Elective vs. emergency from nall care homes - beginning of March to end of April
# relative to mean 2015 to 2019
admissions_type %>% 
  filter(day_dummy >= ymd("0004-03-01") & day_dummy <= ymd("0004-04-30") & admyr != "2014") %>% 
  group_by(admyr, adm_type) %>% 
  summarise(ch_adm = sum(freq),
            spellstartdate_start = min(spellstartdate),
            spellstartdate_end = max(spellstartdate)) %>% 
  group_by(adm_type) %>% 
  mutate(mean_ch_adm_2015_to_2019 = mean(ch_adm[admyr %in% c(2015:2019)]),
         pct_change_2015_to_2019 = round(100 * ch_adm / mean_ch_adm_2015_to_2019, 1)) %>% 
  write_csv(str_c(results_path_sprint3, "CH_admissions_type_March-April_rel2015-2019.csv"))
