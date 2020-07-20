####
# Number of SUS spells in 2019/2020
####

library(tidyverse)
library(tidylog)
library(janitor)
library(lubridate)
library(readxl)


source("file_paths.R")
source("functions.R")

# Import data -------------------------------------------------------------

admissions <- read_xlsx(str_c(raw_data_path, "April 2020/01a Hospital Admission and Discharge to Care Homes Exc Deaths - Summary.xlsx"),
                               sheet = "Adm 1420 All Exclusions", col_types = "numeric")

admissions <- admissions %>% 
  select(-admch_2014,- admother_2014, - admall_2014) %>% 
  pivot_longer(c(-admmnth, -admday), names_to = "type", values_to = "admissions") %>% 
  separate("type", into = c("source", "year"), sep = "_") %>% 
  mutate(source = gsub("adm", "", source),
         date = ymd(str_c(year, admmnth, admday, sep = "-")))

# Admissions in Apr 2019 - Mar 2020

admissions %>% 
  filter(date %within% interval(ymd("2019-04-01"), ymd("2020-03-31"))) %>%
  group_by(source) %>% 
  summarise(date_start = min(date),
            date_end = max(date),
            admissions = sum(admissions)) %>% 
  write_csv(str_c(results_path_sprint3, "Admissions_2018-2019.csv"))


discharges <- read_xlsx(str_c(raw_data_path, "April 2020/01a Hospital Admission and Discharge to Care Homes Exc Deaths - Summary.xlsx"),
                        sheet = "Disch 1420 All Exclusions", col_types = "numeric")

discharges <- discharges %>% 
  select(-dischch_2014,- dischother_2014, - dischall_2014) %>% 
  pivot_longer(c(-dischmnth, -dischday), names_to = "type", values_to = "discharges") %>% 
  separate("type", into = c("dest", "year"), sep = "_") %>% 
  mutate(dest = gsub("disch", "", dest),
         date = ymd(str_c(year, dischmnth, dischday, sep = "-")))

# discharges in Apr 2019 - Mar 2020

discharges %>% 
  filter(date %within% interval(ymd("2019-04-01"), ymd("2020-03-31"))) %>%
  group_by(dest) %>% 
  summarise(date_start = min(date),
            date_end = max(date),
            discharges = sum(discharges)) %>% 
  write_csv(str_c(results_path_sprint3, "Discharges_2018-2019.csv"))
