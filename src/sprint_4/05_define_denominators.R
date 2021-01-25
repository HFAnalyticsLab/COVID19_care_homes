####
# Define denominator (days spent in care home per resident) by week and overall for March-May
# this will be used to calculate hospital admission rates per resident per year
####

library(tidyverse)
library(tidylog)
library(janitor)
library(lubridate)
library(data.table)
library(ISOweek)

source("file_paths.R")
source("functions.R")

# Import data -------------------------------------------------------------

# cohort characteristics 
chres_cohorts <- readRDS(str_c(Rds_data_path, 'sprint_4/chres_cohorts_Jan.Rds')) 

# hospital spells
chres_apcs <- readRDS(str_c(Rds_data_path, 'sprint_4/apcs_chr_cohorts.Rds'))


# Create daily time series for residents in care homes ----
# between the extractstart in January and the end of follow up
# this will then be used to aggregate the number of days spent in a care home either by week
# or within March-May
chres_cohorts <- chres_cohorts %>% 
  select(pid, extractstart, followup_end) %>% 
  mutate(cohort_year = year(extractstart))

# create time series vector 
chres_denom <- chres_cohorts %>% 
  mutate(date = map2(extractstart, followup_end, 
                     ~as.character(seq.Date(from = .x, to = .y, by = "day"))))

# unnest by day and create corresponding week variables
# now we have one row per resident per day
chres_denom <- chres_denom %>%
  unnest_dt(col = date)  %>% 
  as_tibble() %>% 
  mutate(date = as.Date(date),
         week = format(date, format = "%V"),
         weekstart = ISOweek2date(paste0(year(date), "-W", str_pad(week, width = 2,side = "left", pad = "0"), "-1")))

saveRDS(chres_denom,  str_c(Rds_data_path, 'sprint_4/chres_carehomedays.Rds'))

# Create daily time series for hospital stays ----
# this is so that we can substract the number of days spent in hospital
# ie when residents are not at risk of admission
# Day of discharge will not be counted as a day in hospital
# This also means we will ignore spells where admission and discharge happen on the same day

chres_apcs <- chres_apcs %>% 
  select(pid, spellstartdate, spellenddate) %>% 
  mutate(cohort_year = year(spellstartdate),
         spellenddate_minus1d = spellenddate - 1,
         los = as.numeric(spellenddate - spellstartdate, 'days')) %>% 
  filter(!(spellenddate_minus1d < spellstartdate))

# Resolve overlapping spells (otherwise we will double-count days in hospital)
chres_apcs <- chres_apcs %>% 
  group_by(pid) %>% 
  arrange(spellstartdate, desc(los)) %>% 
  mutate(days_overlap_with_previous = pmax(count_days_overlap(lag(spellenddate), spellstartdate),
                                           count_days_overlap(lag(spellenddate, 2), spellstartdate),
                                           count_days_overlap(lag(spellenddate, 3), spellstartdate),
                                           count_days_overlap(lag(spellenddate, 4), spellstartdate),
                                           count_days_overlap(lag(spellenddate, 5), spellstartdate),
                                           count_days_overlap(lag(spellenddate, 6), spellstartdate),
                                           count_days_overlap(lag(spellenddate, 7), spellstartdate)))

chres_apcs <- chres_apcs %>% 
  mutate(spellstartdate_adj = if_else(days_overlap_with_previous > 0,
                                      spellstartdate + (days_overlap_with_previous-1), 
                                      spellstartdate))

# Remove spells that were completely within another spell
chres_apcs <- chres_apcs %>% 
  filter(spellenddate > spellstartdate_adj)

# convert to daiy time series  -> one row per day spent in hospital
chres_apcs_denom <- chres_apcs %>% 
  mutate(date = map2(spellstartdate_adj, spellenddate_minus1d, 
                     ~as.character(seq.Date(from = .x, to = .y, by = "day"))))

chres_apcs_denom <- chres_apcs_denom %>%
  unnest_dt(col = date)  %>% 
  as_tibble() %>% 
  mutate(date = as.Date(date),
         week = format(date, format = "%V"),
         weekstart = ISOweek2date(paste0(year(date), "-W", str_pad(week, width = 2,side = "left", pad = "0"), "-1")))

saveRDS(chres_apcs_denom,  str_c(Rds_data_path, 'sprint_4/chres_hospitaldays.Rds'))


# calculate weekly denominator ----
# 1. Count number of resident days across weeks 

# summarise by year and week
chres_denom_week <- chres_denom %>% 
  group_by(pid, cohort_year, week, weekstart) %>% 
  summarise(res_days = n())

# save intermediate table, will be overwritten
saveRDS(chres_denom_week,  str_c(Rds_data_path, 'sprint_4/chres_denom_weekly.Rds'))


# 2. Count number of days in hospital across weeks 

chres_apcs_denom_week <- chres_apcs_denom %>% 
  group_by(pid, cohort_year, week, weekstart) %>% 
  summarise(hosp_days = n())

# Combine to calculate days in care home

chres_denom_week_comb <- chres_denom_week %>% 
  left_join(chres_apcs_denom_week, by = c("pid", "cohort_year", "week", "weekstart")) %>% 
  mutate(hosp_days = replace_na(hosp_days, 0),
         days_ch = res_days - hosp_days)

saveRDS(chres_denom_week_comb,  str_c(Rds_data_path, 'sprint_4/chres_denom_weekly.Rds'))

#### Overall denominator March - May ----
# 1. Count number of resident days 

# summarise for March, April, May
chres_denom_tot <- chres_denom %>% 
  filter(month(date) %in% c(3,4,5)) %>% 
  group_by(pid, cohort_year) %>% 
  summarise(res_days = n(),
            start_date = min(date),
            end_date = max(date))

# save intermediate table, will be overwritten
saveRDS(chres_denom_tot,  str_c(Rds_data_path, 'sprint_4/chres_denom_March-May.Rds'))

# 2. Count number of days in hospital across weeks 

chres_apcs_denom_tot <- chres_apcs_denom %>% 
  filter(month(spellstartdate) %in% c(3,4,5)) %>% 
  group_by(pid, cohort_year) %>% 
  summarise(hosp_days = n())

# Combine to calculate days in care home

chres_denom_tot_comb <- chres_denom_tot %>% 
  left_join(chres_apcs_denom_tot, by = c("pid", "cohort_year")) %>% 
  mutate(hosp_days = replace_na(hosp_days, 0),
         days_ch = res_days - hosp_days)

saveRDS(chres_denom_tot_comb,  str_c(Rds_data_path, 'sprint_4/chres_denom_March-May.Rds'))
