####
# Cleaning pseudonimysed master patient index for care home residents
####

library(tidyverse)
library(tidylog)
library(janitor)
library(lubridate)
library(rlang)
library(data.table)

source("file_paths.R")
source("functions.R")


### What is it?
# A longitudinal record of care home resident characteristics

### Format
# - each row corresponds to a period of time for which the 
#  characteristics of one resident were valid 
# - residents can have multiple rows, that correspond to different time periods of different lengths

# Where do we want to get to?
# a monthly snap shot of the characteristics of each resident, over time 
# matched to care home characteristics at the time



# Load raw data ---------------------------------------------------------------

# CQC care home charactistics (clean monthly time series)
chchar_ts <- readRDS(str_c(Rds_data_path, 'sprint_4/pseudo_carehome_characteristics_mar20_timeseries.Rds'))
ch_ids <- unique(chchar_ts$ch_id)

# NHAIS extract dates 
# data extract are generated on the Sunday following the 13th day of each month
# this is a lookup file to help us figure out when extracts were generated
nhaisrepdates <- read_csv(str_c(raw_data_path, "care home paper/nhaisrepdates.csv")) %>% 
  filter(extractdate >= ymd("2000-01-01")) %>% 
  rename("prior_extractend" = "extractend") %>% 
  arrange(extractdate) %>% 
  mutate(extractend = lead(prior_extractend)) %>% 
  select(-prior_extractend)

mmpi <- read_csv(str_c(raw_data_path, "care home paper/mmpisubset_chres.csv"),
                         col_types = cols(pid = col_character(),
                                          tnrchid = col_character(),
                                          dob = col_date(format = "%d/%m/%Y"),
                                          datefrom = col_date(format = "%d/%m/%Y"),
                                          dateto = col_date(format = "%d/%m/%Y"),
                                          extdatefrom = col_date(format = "%d/%m/%Y"),
                                          extdateto = col_date(format = "%d/%m/%Y"),
                                          deathdate = col_date(format = "%d/%m/%Y"),
                                          deathmonth = col_date(format = "%d/%m/%Y"),
                                          extdatedeath = col_date(format = "%d/%m/%Y"),
                                          undeaddeathdate = col_date(format = "%d/%m/%Y"))) 
# Cleaning ----------------------------------------------------------------


# Convert to time series 
# this will be done based on extdate (first of the month)
# currently extract end end extract start of the next entry are the same
# so the extract end date needs to be shifted backwards by one month to generate intervals, as the intervals overlap otherwise
# if end date is missing (extract still valid), fill in with the last month in the study period
# afterwards it will be combined with the lookup tables of the acutal extract dates 
mmpi_monthly <- mmpi %>% 
  select(pid, sex, dob, gppraccode, lsoa11pidpcd, ccg15pidlsoa, la11pidlsoa, extdatefrom,
         extdateto, deathdate, deathmonth, tnrchid, undead, undeaddeathdate, uprn, 
         uprnparent) %>% 
  mutate(extdateto_filled = if_else(!is.na(extdateto), extdateto %m-% period("1 month"), extdateto),
         extdateto_filled = if_else(is.na(extdateto_filled), ymd("2020-06-01"), extdateto_filled)) %>% 
  create_ts(month_from = "extdatefrom", month_to = "extdateto_filled") 

saveRDS(mmpi_monthly, str_c(Rds_data_path, 'sprint_4/mmpisubset_chres_monthly_raw.Rds')) # just a backup 


# Merge in NHAIS extract dates
mmpi_monthly <- mmpi_monthly %>%
  select(-extdatefrom, -extdateto, -extdateto_filled) %>% 
  left_join(nhaisrepdates[, c("extractdate", "extractstart", "extractend")], 
            by = c("month" = "extractdate"))

# Fill deathmonth using other records for the same patient
 mmpi_monthly <- mmpi_monthly %>% 
   mutate(deathmonth_filled = if_else(!is.na(deathdate) & is.na(deathmonth),
                                           floor_date(deathdate, "month"), deathmonth))

# Remove extracts after patient death
 # note: date of death is always set to the first day of the month
mmpi_monthly <-  mmpi_monthly %>% 
  filter(is.na(deathdate) | extractstart < deathdate) 


# Some patients have multiple care home ids matched for the same time period
# To resolve multiple matches 
# 1. Unnest rows multiple care home identifiers
mmpi_monthly <- mmpi_monthly %>% 
  mutate(assigned_tnrchid = as.list(str_split(tnrchid, pattern = ",")),
         tnrchid_matches = lengths(assigned_tnrchid)) %>% 
  unnest_dt(col = assigned_tnrchid) %>%
  mutate(assigned_tnrchid = as.numeric(assigned_tnrchid)) 

# 2. Join in monthly care home characteristics and check whether care home was open during the extract month
mmpi_monthly <- mmpi_monthly %>% 
  left_join(chchar_ts %>% 
              mutate(ch_is_open = 1), 
            by = c("assigned_tnrchid" = "pseudo_ch_id", "month" = "month")) %>% 
  mutate(ch_is_open = ifelse(is.na(ch_is_open), 0, ch_is_open))

mmpi_monthly %>% 
  tabyl(ch_is_open) %>% 
  adorn_title()

# For data cleaning flowchart:
# Number of residents in Jan 2019 and 2020 BEFORE filtering out the ones in closed care homes
mmpi_monthly %>%
  filter(month == ymd("2019-01-01") |month == ymd("2020-01-01")) %>% 
  pull(pid) %>%  unique() %>%  length()

mmpi_monthly %>%
  filter(month == ymd("2019-01-01")) %>% 
  pull(pid) %>%  unique() %>%  length()

mmpi_monthly %>%
  filter(month == ymd("2020-01-01")) %>% 
  pull(pid) %>%  unique() %>%  length()

# For data cleaning flowchart:
# Number of residents in Jan 2019 and 2020 AFTER filtering out the ones in closed care homes
mmpi_monthly %>%
  filter(ch_is_open == 1 & (month == ymd("2019-01-01") |month == ymd("2020-01-01"))) %>% 
  pull(pid) %>%  unique() %>%  length()

mmpi_monthly %>%
  filter(ch_is_open == 1 & month == ymd("2019-01-01")) %>% 
  pull(pid) %>%  unique() %>%  length()

mmpi_monthly %>%
  filter(ch_is_open == 1 & month == ymd("2020-01-01")) %>% 
  pull(pid) %>%  unique() %>%  length()

# 3. Remove residents in closed care homes (also gets rid of duplicate matches)
mmpi_monthly <-  mmpi_monthly %>% 
  filter(ch_is_open == 1)

# 4. If multiple matches remain (ie patient matches to multiple *open* care homes in a given month)
# the exclude these records
mmpi_monthly <-  mmpi_monthly %>% 
  group_by(pid, month) %>% 
  filter(n() == 1) %>%
  ungroup()

mmpi_monthly <- mmpi_monthly %>% 
  select(-ch_is_open)

# For data cleaning flowchart:
# Number of residents in Jan 2019 and 2020 AFTER filtering out 
# multiple matches that cannot be resolved
mmpi_monthly %>%
  filter(month == ymd("2019-01-01") | month == ymd("2020-01-01")) %>% 
  pull(pid) %>%  unique() %>%  length()

mmpi_monthly %>%
  filter(month == ymd("2019-01-01")) %>% 
  pull(pid) %>%  unique() %>%  length()

mmpi_monthly %>%
  filter(month == ymd("2020-01-01")) %>% 
  pull(pid) %>%  unique() %>%  length()


saveRDS(mmpi_monthly,  str_c(Rds_data_path, 'sprint_4/mmpisubset_chres_monthly_chchars.Rds'))


# Sense checking ----------------------------------------------------------

# check for duplicates - there should only be one record per resident per month
nrow(mmpi_monthly %>%
       group_by(pid, month) %>%
       count() %>%
       filter(n > 1))

