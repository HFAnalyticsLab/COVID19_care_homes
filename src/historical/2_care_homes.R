####
# Descriptive analysis of historical data
# 2. Care homes
####

library(tidyverse)
library(tidylog)
library(janitor)
library(lubridate)

source("file_paths.R")

# Import data -------------------------------------------------------------

care_homes <- readRDS(str_c(str_c(hist_data_path_Rds, 'carehome_2018.Rds')))

# unique number of care homes 
unique(care_homes$chid) %>% length()
unique(care_homes$chcharid) %>% length()


# Creating time series dataset --------------------------------------------


# Convert to time series by creating additional rows for each month
# between chcharfrom and chcharto (time period where characteristics are the same)
# this using simplified dates (all rounded to the beginning of the month)

care_homes_ts <- care_homes %>%
  filter(is.na(chcharto) | chcharto >= chcharfrom) %>% 
  mutate(chcharfrom_ceiling = floor_date(chcharfrom, "month"),
         chcharto_floor = floor_date(chcharto, "month"),
         chcharto_floor = if_else(is.na(chcharto_floor), ymd("2018-04-01"), chcharto_floor),
         month = map2(chcharfrom_ceiling, chcharto_floor, 
                          ~seq.Date(from = .x, to = .y, by = "month"))) %>% 
  unnest(cols = month) 

# Age groups catered for
care_homes_ts <- care_homes_ts %>% 
  mutate(age_group_served = case_when(chwholepop == 1 ~ 'all',
                                      chchild == 1 & chyadults == 0 & chelderly == 0  ~ 'children',
                                      chchild == 1 & chyadults == 1 & chelderly == 0  ~ 'children + yadults',
                                      chchild == 0 & chyadults == 1 & chelderly == 0  ~ 'yadults',
                                      chchild == 0 & chyadults == 1 & chelderly == 1  ~ 'yadults + elderly',
                                      chchild == 0 & chyadults == 0 & chelderly == 1  ~ 'elderly',
                                      chchild == 1 & chyadults == 1 & chelderly == 1  ~ 'children + yadults + elderly',
                                      chchild == 0 & chyadults == 0 & chelderly == 0 | chwholepop == 0 ~ 'unknown',
                                      TRUE ~ 'other'))

# Flag for specialist homes and for homes for older people
care_homes_ts <- care_homes_ts %>% 
  mutate(chspecialist = ifelse((chlearning == 1 | chdrug == 1 | cheating == 1 | chmha == 1 | chsensory == 1) &
                                 (chchild == 1 | chyadults == 1 | chwholepop == 1), 1, 0))


saveRDS(care_homes_ts, str_c(hist_data_path_Rds, 'carehome_2018_timeseries.Rds'))

# Number of care homes and beds over time ------------------------------------------

(care_homes_ts %>% 
  group_by(month) %>% 
  summarise(n = n()) %>% 
  ggplot(aes(x = month, y = n)) +
  geom_point() +
  geom_line() + 
  theme_bw() +
  xlab("Time (months)") +
  ylab("Number of care homes") +
  ggtitle("Number of care homes over time")) %>% 
  ggsave(str_c(results_path_hist_ts, "CH_number.png"), .)

(care_homes_ts %>% 
    group_by(month) %>% 
    summarise(n = sum(chnbeds[!is.na(chnbeds)])) %>% 
    ggplot(aes(x = month, y = n)) +
    geom_point() +
    geom_line() + 
    theme_bw() +
    xlab("Time (months)") +
    ylab("Number of care home beds") +
    scale_y_continuous(labels = function(x) format(x, scientific = FALSE)) +
    ggtitle("Number of care home beds over time")) %>% 
  ggsave(str_c(results_path_hist_ts, "CH_beds.png"), .)


# 2014 - now
(care_homes_ts %>% 
    filter(month >= ymd("2014-01-01") & month < ymd("2017-11-01")) %>% 
    group_by(month) %>% 
    summarise(n = n()) %>% 
    ggplot(aes(x = month, y = n)) +
    geom_point() +
    geom_line() + 
    theme_bw() +
    xlab("Time (months)") +
    ylab("Number of care homes") +
    ggtitle("Number of care homes over time")) %>% 
  ggsave(str_c(results_path_hist_ts, "CH_number_since2014.png"), .)

care_homes_ts %>% 
  filter(month >= ymd("2014-01-01") & month < ymd("2017-11-01")) %>% 
  group_by(month) %>% 
  summarise(n = n()) %>% 
  write_csv(str_c(results_path_hist_ts, "CH_number_since2014.csv"))

(care_homes_ts %>% 
    filter(month >= ymd("2014-01-01") & month < ymd("2017-11-01")) %>% 
    group_by(month) %>% 
    summarise(n = sum(chnbeds[!is.na(chnbeds)])) %>% 
    ggplot(aes(x = month, y = n)) +
    geom_point() +
    geom_line() + 
    theme_bw() +
    xlab("Time (months)") +
    ylab("Number of care home beds") +
    scale_y_continuous(labels = function(x) format(x, scientific = FALSE)) +
    ggtitle("Number of care home beds over time")) %>% 
  ggsave(str_c(results_path_hist_ts, "CH_beds_since2014.png"), .)

care_homes_ts %>% 
  filter(month >= ymd("2014-01-01") & month < ymd("2017-11-01")) %>% 
  group_by(month) %>% 
  summarise(n = sum(!is.na(chnbeds))) %>% 
  write_csv(str_c(results_path_hist_ts, "CH_beds_since2014.csv"))


# Snapshot of most recent data --------------------------------------------------------

# looks like October 2017 is the last reliable time point
care_homes_current <- care_homes_ts %>% 
  filter(month == ymd("2017-10-01"))

# Distribution of number of beds per home
care_homes_current %>% 
  tabyl(chnbeds) 

care_homes_current %>% 
  ggplot(aes(x = chnbeds)) +
  geom_histogram(binwidth = 5) +
  theme_bw()

characterise_ch <- function(data, var){
  
  data %>% 
    group_by(!!rlang::sym(var)) %>% 
    summarise(ch = n(),
              resident_ch = sum(chnursing == 0),
              nursing_ch = sum(chnursing == 1),
              mean_chsize = round(mean(chnbeds), 1),
              median_chsize = median(chnbeds),
              sd_chsize = round(sd(chnbeds), 1),
              beds = sum(chnbeds),
              resident_beds = sum(chnbeds[chnursing == 0]),
              nursing_beds = sum(chnbeds[chnursing == 1])) %>% 
    ungroup() %>% 
    mutate(percent_ch = round(100*ch / sum(ch), 1),
           percent_resident_ch = round(100*resident_ch / sum(resident_ch), 1),
           percent_nursing_ch = round(100*nursing_ch / sum(nursing_ch), 1),
           percent_beds = round(100*beds / sum(beds), 1),
           percent_resident_beds = round(100*resident_beds / sum(resident_beds), 1),
           percent_nursing_beds = round(100*nursing_beds / sum(nursing_beds), 1)) %>% 
    arrange(desc(ch)) %>% 
    write_csv(str_c(results_path_hist_snapshot, "CH_2017-10_summary_", var, ".csv"))
  
}

c("chregion", "chown", "ccgname", "la11", "ccg15", "age_group_served", "chspecialist") %>% 
  map(., characterise_ch, data = care_homes_current)

# no grouping
characterise_ch(data = care_homes_current %>%  mutate(dummy = 1), var = "dummy")

