####
# Cleaning pseudonimysed CQC care home characteristics
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
# A longitudinal record of care homes regulated by the CQC along with their characteristics,
# such as bed capacity, specialties, open and close date etc.

### Format
# - each row corresponds to a period of time (from mincqcfiledate to maxcqcfiledate) for which the 
#  characteristics of one care home (pseudo_ch_id) were valid 
# - care homes can have multiple rows, that correspond to different time periods of different lengths
# - in addition, there is information on when the care home opened and closed 

# Where do we want to get to?
# a monthly snap shot of the characteristics of each carehome, over time in order to extract
# the characteristics for care homes that were open in January 2019 and January 2020


# Load raw data ---------------------------------------------------------------

chchar_raw <- read_delim(str_c(raw_data_path, "care home paper/pseudo_carehome_characteristics_mar20.csv"),
                         delim = "¬") %>% 
  mutate_at(c("mincqcfiledate", "maxcqcfiledate", "ch_startdate", "ch_enddate"), as.Date) 

            
# Cleaning ----------------------------------------------------------------


### Aspects that require cleaning
# 1. Missing data values that are not NA
# 2. Filling in missing values, especially in the open and close date, from more complete rows for the same care home
# 3. Remove duplicate rows. 
# 4. All rows with chactive = 1 show 0 beds, but they always seem to happen shortly after a 
# care home opens - is this a line the system adds when a care home opens?
# 5. Some rows list 0 beds, even when chactive = 0. Fill in bed numbers from other rows.
# 6. Sometimes entries for the same care home are valid for overlapping time periods. Need to be resolved.
# 7. gSometimes there are gaps between records for the same care home. Need to be filled in.
# 8. Conversion to monthly time series
# -----------------

# 1. Missing values not coded as NA
# Excel seems to show missing dates as "1899-12-30"

chchar <- chchar_raw %>% 
  mutate_at(vars("ch_startdate", "ch_enddate"), ~ na_if(., ymd("1899-12-30")))

summary(chchar_raw) # confirm no missing values in any other variable

# 2. Cleaning dates

# fill in missing open and close dates, use earliest open date and latest close date in
# case there are multiple for the same care home 
chchar <- chchar %>% 
  group_by(pseudo_ch_id) %>% 
  mutate(ch_startdate_filled = min(ch_startdate, na.rm = TRUE),
         ch_enddate_filled = max(ch_enddate, na.rm = TRUE))%>% 
  mutate_at(c("ch_startdate_filled", "ch_enddate_filled"), ~na_if(., Inf)) %>% 
  mutate_at(c("ch_startdate_filled", "ch_enddate_filled"), ~na_if(., -Inf)) %>% 
  ungroup()


# where open dates are completely missing, use the earliest cqcfiledate or the close date, 
# whichever was earlier

# if the open date is after the close date, replace with the close date

# standardise the open and close date by setting them to the first of the respective month
# if a care home opens in a given month, we'll assume it was open for the whole month
# if a care home closes in a given month, we'll assume it was closed for the whole month
# - > 1. this will make it easier to match these dates to MPI extract dates
# -> 2. this will make sure there are no gaps or overlaps if the name and therefore the id of a care home changes

chchar <- chchar %>% 
  group_by(pseudo_ch_id) %>% 
  mutate(ch_startdate_filled = case_when(is.na(ch_startdate_filled) & 
                                           min(mincqcfiledate) <= ch_enddate_filled ~ min(mincqcfiledate), 
                                         is.na(ch_startdate_filled) & 
                                           min(mincqcfiledate) > ch_enddate_filled ~ ch_enddate_filled,
                                         TRUE ~ ch_startdate_filled),
         ch_startdate_filled = if_else(ch_startdate_filled > ch_enddate_filled, 
                                       ch_enddate_filled, ch_startdate_filled),
         ch_startdate_filled_floor = floor_date(ch_startdate_filled, "month"), 
         ch_enddate_filled_floor = floor_date(ch_enddate_filled, "month"))


# 3. Remove complete duplicates
# this might happen if characteristics change that are not in our dataset
chchar <- chchar %>% 
  distinct()


# 4. and 5. impute where bed number is 0 (fill from above or below)

# all rows with ch_active == 1 show 0 beds
chchar %>% 
  mutate(zero_beds = ifelse(ch_beds == 0, 1, 0)) %>% 
  tabyl(ch_active, zero_beds) %>% 
  adorn_title()


chchar <- chchar %>% 
  mutate(ch_beds_filled = ifelse(ch_beds == 0, NA, ch_beds)) %>% 
  group_by(pseudo_ch_id) %>% 
  arrange(mincqcfiledate) %>% 
  fill(ch_beds_filled, .direction = "downup")

# 6. and 7. Gaps and overlapping time periods

# calculate the gap between adjacent intervals for the same care home 
# calculate overlap with the previous two intervals, retain the bigger overlap (this is because sometimes
# up to three records overlap)

chchar <- chchar %>%
  mutate(months_valid = count_months_covered(mincqcfiledate, maxcqcfiledate))

chchar <- chchar %>% 
  group_by(pseudo_ch_id) %>% 
  arrange(pseudo_ch_id, mincqcfiledate, desc(months_valid)) %>% 
  mutate(months_gap_to_next = count_months_missing(maxcqcfiledate, lead(mincqcfiledate)),
         months_overlap_with_previous = pmax(count_months_overlap(lag(maxcqcfiledate), mincqcfiledate),
                              count_months_overlap(lag(maxcqcfiledate, 2), mincqcfiledate),
                              count_months_overlap(lag(maxcqcfiledate, 3), mincqcfiledate)))

chchar %>% 
  tabyl(months_gap_to_next) %>% 
  mutate(cum_percent = cumsum(percent))

chchar %>% 
  tabyl(months_overlap_with_previous) 

# adjust rows with overlapping time periods
# cut time periods short if they overlap with the previous -> mincqcfiledate_clean
# (NB this is a somewhat arbitrary choice, we could have also cut short the previous entry)
# extend them if there's a gap to the next one (up to 3 months) -> maxcqcfiledate_clean
chchar <- chchar %>% 
  group_by(pseudo_ch_id) %>% 
  arrange(pseudo_ch_id, mincqcfiledate, desc(months_valid)) %>% 
  mutate(mincqcfiledate_clean = mincqcfiledate,
         mincqcfiledate_clean = if_else(months_overlap_with_previous > 0 ,
                                        shift_date_forward(mincqcfiledate_clean, months_overlap_with_previous), 
                                        mincqcfiledate_clean),
         maxcqcfiledate_clean = maxcqcfiledate,
         maxcqcfiledate_clean = if_else(months_gap_to_next %in% c(1,2,3), 
                                        shift_date_forward(maxcqcfiledate_clean, months_gap_to_next),
                                        maxcqcfiledate_clean))

# If the care home opens before the first mincqcfiledate, shift the first mincqcfiledate back 

# Artificially expand latest extract date to June if the care home is still open
# This makes sure we can use it for MPIs extracted later than March

month_if_still_open <- ymd("2020-06-01") 

chchar <- chchar %>% 
  group_by(pseudo_ch_id) %>% 
  mutate(maxcqcfiledate_clean = if_else(is.na(ch_enddate_filled) &  
                                        maxcqcfiledate_clean == max(maxcqcfiledate_clean), 
                                        month_if_still_open,
                                        maxcqcfiledate_clean),
         mincqcfiledate_clean = if_else(ch_startdate_filled_floor < min(mincqcfiledate_clean) & 
                                          mincqcfiledate_clean == min(mincqcfiledate_clean), 
                                        ch_startdate_filled_floor,
                                        mincqcfiledate_clean))

# remove time periods made redundant by removing the overlap
chchar <- chchar %>% 
  filter(mincqcfiledate_clean <= maxcqcfiledate_clean)

# 8. convert care home file to time series

chchar_ts <- chchar %>% 
  create_ts(month_from = "mincqcfiledate_clean", month_to = "maxcqcfiledate_clean") 

chchar_ts <- chchar_ts %>% 
  select(pseudo_ch_id, month, ch_nursing:ch_beds_filled, mincqcfiledate,  maxcqcfiledate) 


# check for duplicates - there should only be one record per care home per month
nrow(chchar_ts %>% 
  group_by(pseudo_ch_id, month) %>% 
  count() %>% 
  arrange(desc(n)) %>% 
  filter(n > 1)) == 0 


saveRDS(chchar_ts,  str_c(Rds_data_path, 'sprint_4/pseudo_carehome_characteristics_mar20_timeseries.Rds'))
write_csv(chchar_ts,  str_c(Rds_data_path, 'sprint_4/pseudo_carehome_characteristics_mar20_timeseries.csv'))
