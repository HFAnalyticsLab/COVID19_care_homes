####
# Cleaning pseudonimysed master patient index for care home residents and long-term conditions
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
# - each row corresponds to a period of timefor which the 
#  characteristics of one resident were valid 
# - residents can have multiple rows, that correspond to different time periods of different lengths

# Where do we want to get to?
# a monthly snap shot of the characteristics of each resident, over time 
# matched to care home characteristics at the time



# Load raw data ---------------------------------------------------------------

chchar_ts <- readRDS(str_c(Rds_data_path, 'pseudo_carehome_characteristics_mar20_timeseries.Rds'))
ch_ids <- unique(chchar_ts$pseudo_ch_id)

mmpi_monthly <- read_csv(str_c(raw_data_path, "April 2020/CHResLTCbyMonth.csv"),
                         col_types = cols(pid = col_character(),
                                          tnrchid = col_character(),
                                          dob = col_date(format = "%d/%m/%Y"),
                                          datefrom = col_date(format = "%d/%m/%Y"),
                                          dateto = col_date(format = "%d/%m/%Y"),
                                          extdatefrom = col_date(format = "%d/%m/%Y"),
                                          extdateto = col_date(format = "%d/%m/%Y"),
                                          deathdate = col_date(format = "%d/%m/%Y"),
                                          extdatedeath = col_date(format = "%d/%m/%Y"),
                                         undeaddeathdate = col_date(format = "%d/%m/%Y")))


# Cleaning ----------------------------------------------------------------

# Remove records after resident died

mmpi_monthly <-  mmpi_monthly %>% 
  filter(is.na(extdatedeath) | extdatefrom <= extdatedeath)

# Split up into long-term conditions, residents and care home assignemnts to make computation quicker
LTPs <- mmpi_monthly %>%
  select(uid, uid_mmpiss, pid, extdatefrom, extdateto, e_alcabuse_h36:a_flupneumonia_h36)

saveRDS(LTPs,  str_c(Rds_data_path, 'mmpiss_bymonth_LTPs.Rds'))
rm(LTPs)

ch_assignment <- mmpi_monthly %>% 
  select(uid, uid_mmpiss, pid, extdatefrom, extdateto, tnrchid)


mmpi_monthly <- mmpi_monthly %>% 
  select(-c(e_alcabuse_h36:a_flupneumonia_h36)) 


# Unnest multiple care home identifiers
ch_assignment <- ch_assignment %>% 
  mutate(assigned_tnrchid = as.list(str_split(tnrchid, pattern = ",")),
         tnrchid_matches = lengths(assigned_tnrchid)) %>% 
  unnest_dt(col = assigned_tnrchid) %>%
  mutate(assigned_tnrchid = as.numeric(assigned_tnrchid),
         chr_exists = ifelse(assigned_tnrchid %in% ch_ids, 1, 0)) 

ch_assignment <-ch_assignment %>% 
  left_join(chchar_ts %>% 
              mutate(ch_is_open = 1), 
            by = c("assigned_tnrchid" = "pseudo_ch_id", "extdatefrom" = "month")) %>% 
  mutate(ch_is_open = ifelse(is.na(ch_is_open), 0, ch_is_open))

ch_assignment %>% 
  tabyl(chr_exists, ch_is_open) %>% 
  adorn_title()

# Remove duplicates - only retain first 
ch_assignment <-  ch_assignment %>% 
  arrange(pid, extdatefrom, desc(ch_is_open)) %>% 
  distinct(pid, extdatefrom, extdateto, .keep_all = TRUE)


# Join back together
mmpi_monthly <- mmpi_monthly %>% 
  right_join(ch_assignment)


saveRDS(mmpi_monthly,  str_c(Rds_data_path, 'mmpiss_bymonth_chchars.Rds'))

# Save from 2017 onwards separately
mmpi_monthly %>% 
  filter(extdatefrom >= ymd("2017-01-01")) %>% 
 saveRDS(str_c(Rds_data_path, 'mmpiss_bymonth_chchars_2017-2020.Rds'))


# Sense checking ----------------------------------------------------------

# check for duplicates - there should only be one record per resident per month
nrow(ch_assignment %>%
       group_by(pid, extdatefrom) %>%
       count() %>%
       #arrange(desc(n)) %>%
       filter(n > 1))


ch_assignment %>%
  tabyl(chr_exists, ch_is_open) %>%
  adorn_title()

# fraction of resident months spent in a care home that wasn't open at the time
# all time
nrow(ch_assignment %>% filter(ch_is_open == 0))/ nrow(ch_assignment)
# 2017 onwards
nrow(ch_assignment %>% filter(ch_is_open == 0 & extdatefrom >= ymd("2017-01-01")))/ nrow(ch_assignment %>% 
                                                                                           filter(extdatefrom >= ymd("2017-01-01")))

# Investigating residents in care homes that aren't open ------------------

chrs_in_closed_ch <- mmpi_monthly %>% 
  filter(ch_is_open == 0) %>% 
  select(extdatefrom, assigned_tnrchid) %>%  
  group_by(extdatefrom, assigned_tnrchid) %>% 
  summarise(chr_records = n()) %>% 
  left_join(chchar_ts %>% 
              select(pseudo_ch_id, ch_startdate_filled, ch_enddate_filled, ch_startdate_filled_floor, ch_enddate_filled_floor) %>% 
              unique(), by = c("assigned_tnrchid" ="pseudo_ch_id"))

chrs_in_closed_ch <- chrs_in_closed_ch %>% 
  arrange(assigned_tnrchid, extdatefrom)

write_csv(chrs_in_closed_ch, str_c(Rds_data_path, 'chrs_in_closed_ch.csv'))
