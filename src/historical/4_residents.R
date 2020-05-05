####
# Descriptive analysis of historical data
# 2. Care home residents
####

library(tidyverse)
library(tidylog)
library(janitor)
library(lubridate)
library(rlang)
library(data.table)

source("file_paths.R")


# Functions ---------------------------------------------------------------

# Faster unnesting function
unnest_dt <- function(data, col){
  
  data <- as.data.table(data)
  col <- ensyms(col)
  
  columns <- syms(setdiff(colnames(data), as.character(col)))
  data <- eval(
    expr(data[, unlist(!!!col), by = list(!!!columns)])
    )
  
  colnames(data) <- c(as.character(columns), as.character(col))
  
  return(data)
}

# Count the number of records (ie people) that have the value corresponding to the median of a given column. 
# this is needed for statistical disclosure control.

n_with_median_value <- function(values){
  sum(values == median(values, na.rm = TRUE))
}

# Import data -------------------------------------------------------------

care_homes_ts <- readRDS(str_c(str_c(hist_data_path_Rds, 'carehome_2018_timeseries.Rds')))
residents <- readRDS(str_c(str_c(hist_data_path_Rds, 'carehome_residents_2018.Rds')))

IMD_2015 <- read_csv(str_c(ref_data_path, "IMD_2015_LSOA.csv")) %>%
  select(starts_with('LSOA code'),
         contains('Index of Multiple Deprivation (IMD) Rank'),
         contains('Index of Multiple Deprivation (IMD) Decile')) 
colnames(IMD_2015) <- c("LSOA", "IMD_rank", "IMD_decile")

# Long-term conditions in April 2018, looking back two years
comorbidities <- readRDS(str_c(hist_data_path_Rds, 'apce_comorbidities_2015-2017.Rds'))

# Derive vars -------------------------------------------------------------

residents <- residents %>% 
  mutate(deathdate_ym = floor_date(deathdate, "month"))

# Creating a time series view on residents --------------------------------------------------

# Cleaning up chresto
residents <- residents %>%
  mutate(chresto_filled = if_else(is.na(chresto) & !is.na(chclose), chclose, chresto), 
         chresto_filled = if_else(!is.na(residents$chresto) & 
                                   !is.na(residents$chclose) & 
                                   residents$chresto > residents$chclose, chclose, chresto_filled))

# Rounding dates (will work with monthly aggregates)
residents <- residents %>%
  mutate(chresfrom_floor = floor_date(chresfrom, "month"),
         chresto_filled_floor = floor_date(chresto_filled, "month"),
         chresto_filled_floor = if_else(is.na(chresto_filled_floor), ymd("2018-04-01"), chresto_filled_floor))

residents <- residents %>%
  mutate(month = map2(chresfrom_floor, chresto_filled_floor, 
                      ~as.character(seq.Date(from = .x, to = .y, by = "month"))))

residents_ts <- residents %>% 
  unnest_dt(col = month) 

residents_ts <- residents_ts %>% 
  as_tibble() %>% 
  mutate(month = as.Date(month),
         chstay_td_months = round(time_length(interval(chresfrom_ceiling, month), "month")),
         age = floor(time_length(interval(dob, month), "year"))) 

residents_ts_comb <- residents_ts %>% 
  left_join(care_homes_ts[, c("chnbeds", "chnursing", "chelderly", "la11", "lsoach",
                              "chspecialist", "tnrchid", "chregion", "age_group_served", "month")], 
            by = c("tnrchid", "month"))

# fill missing resident info with care home info, join IMD
residents_ts_comb <- residents_ts_comb %>% 
  mutate(lsoa11_filled = if_else(is.na(lsoa11pidpcd), lsoach, lsoa11pidpcd),
         la11_filled = if_else(is.na(la11pidlsoa), la11, la11pidlsoa))  %>% 
  left_join(IMD_2015, by = c("lsoa11_filled" = "LSOA"))

saveRDS(residents_ts_comb, str_c(hist_data_path_Rds, 'residents_2018_timeseries.Rds'))

# Number of residents by month

(residents_ts_comb %>% 
    group_by(month) %>% 
    summarise(n = n()) %>% 
    ggplot(aes(x = month, y = n)) +
    geom_point() +
    geom_line() + 
    theme_bw() +
    xlab("Month") +
    ylab("Number of residents") +
    ggtitle("Number of care home residents")) %>% 
  ggsave(str_c(results_path_hist_ts, "CHR_since2014.png"), .)

residents_ts_comb %>% 
  group_by(month) %>% 
  summarise(n = n()) %>% 
  write_csv(str_c(results_path_hist_ts, "CHR_since2014.csv"))

# Time series: deaths -------------------------------------------------------------

# Deaths by month
(residents %>% 
  filter(!is.na(deathdate_ym) & deathdate_ym > ymd("2014-01-01") & deathdate_ym < ymd("2017-11-01")) %>% 
  group_by(deathdate_ym) %>% 
  summarise(n = n()) %>% 
  ggplot(aes(x = deathdate_ym, y = n)) +
  geom_point() +
  geom_line() + 
  theme_bw() +
  xlab("Month") +
  ylab("Number of deaths") +
  ggtitle("Number of deaths among care home residents")) %>% 
  ggsave(str_c(results_path_hist_ts, "CHR_deaths_since2014.png"), .)

residents %>% 
  filter(!is.na(deathdate_ym) & deathdate_ym > ymd("2014-01-01")) %>% 
  group_by(deathdate_ym) %>% 
  summarise(n_deaths = n())  %>% 
  write_csv(str_c(results_path_hist_ts, "CHR_deaths_since2014.csv"))

# years overlaid
months <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")

(residents %>% 
  filter(!is.na(deathdate_ym) & deathdate_ym > ymd("2014-01-01") & deathdate_ym < ymd("2017-11-01")) %>% 
  group_by(deathdate_ym) %>% 
  summarise(n_deaths = n()) %>% 
  ggplot(aes(x = month(deathdate_ym), y = n_deaths, color = factor(year(deathdate_ym)))) +
  scale_x_continuous(breaks = c(1:12), labels = months) +
  geom_point() +
  geom_line() + 
  theme_bw() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank()) +
  xlab("Month") +
  ylab("Number of deaths") +
  ggtitle("Number of deaths among care home residents")) %>% 
  ggsave(str_c(results_path_hist_ts, "CHR_deaths_since2014_overlaid.png"), .)

# average per calendar month
(residents %>% 
  filter(!is.na(deathdate_ym) & deathdate_ym > ymd("2014-01-01") & deathdate_ym < ymd("2017-11-01")) %>% 
  group_by(deathdate_ym) %>% 
  summarise(n = n()) %>% 
  ungroup() %>% 
  group_by(month(deathdate_ym)) %>% 
  summarise(mean_deaths = mean(n),
            n_months = n_distinct(year(deathdate_ym))) %>% 
  ggplot(aes(x = `month(deathdate_ym)`, y = mean_deaths)) +
  scale_x_continuous(breaks = c(1:12), labels = months) +
  geom_point() +
  geom_line() + 
  theme_bw() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank()) +
  xlab("Month") +
  ylab("Number of deaths") +
  ggtitle("Number of deaths among care home residents")) %>% 
  ggsave(str_c(results_path_hist_ts, "CHR_deaths_since2014_average.png"), .)

residents %>% 
  filter(!is.na(deathdate_ym) & deathdate_ym > ymd("2014-01-01")) %>% 
  group_by(deathdate_ym) %>% 
  summarise(n = n()) %>% 
  ungroup() %>% 
  group_by(month(deathdate_ym)) %>% 
  summarise(mean_deaths = mean(n),
            n_months = n_distinct(year(deathdate_ym))) %>% 
  write_csv(str_c(results_path_hist_ts, "CHR_deaths_since2014_average.csv"))


# Snapshot October 2017 -----------------------------------------------------

residents_current <- residents_ts_comb %>% 
  filter(month == ymd("2017-10-01")) %>% 
  left_join(comorbidities, by = "pid") 

residents_current <- residents_current %>% 
  mutate(Over65 = case_when(age > 65 ~ "1",
                            age <= 65 ~ "0",
                            TRUE ~ "Missing"))

# IMD quintiles (1 is most deprived, 5 is least deprived)
residents_current <- residents_current %>% 
  mutate(IMD_quintile = cut(IMD_decile, breaks = seq(0, 10, by = 2), labels = c(1:5)),
         IMD_quintile = fct_explicit_na(IMD_quintile, na_level = 'Missing'))

# Exclude residents without a care home match and without IMD
residents_current <- residents_current %>% 
  filter(!is.na(IMD_decile) & !is.na(chnursing))

# Function to characterise care home residents, split by additional variable
characterise_chr <- function(data, var){
  
  data %>% 
    group_by(!!rlang::sym(var)) %>% 
    summarise(chr = n(),
              resident_chr = sum(chnursing == 0, na.rm = TRUE),
              nursing_chr = sum(chnursing == 1, na.rm = TRUE),
              mean_chr_age = round(mean(age, na.rm = TRUE), 1),
              median_chr_age = median(age, na.rm = TRUE),
              median_chr_age_n = n_with_median_value(age),
              sd_chr_age = round(sd(age, na.rm = TRUE), 1),
              mean_chr_chstay_td_months = round(mean(chstay_td_months, na.rm = TRUE), 1),
              median_chr_chstay_td_months = median(chstay_td_months, na.rm = TRUE),
              median_chr_chstay_td_months_n = n_with_median_value(chstay_td_months),
              sd_chr_chstay_td_months = round(sd(chstay_td_months, na.rm = TRUE), 1),
              chr_charlson_dementia = sum(charlson_dementia == 1, na.rm = TRUE),
              chr_charlson_copd = sum(charlson_copd == 1, na.rm = TRUE),
              chr_charlson_chf = sum(charlson_chf == 1, na.rm = TRUE),
              chr_charlson_diab = sum(charlson_diab == 1, na.rm = TRUE),
              chr_charlson_canc = sum(charlson_canc == 1, na.rm = TRUE),
              mean_charlson_score = round(mean(charlson_score, na.rm = TRUE), 1),
              mean_charlson_wscore = round(mean(charlson_wscore, na.rm = TRUE), 1),
              chr_IMD_Q1 = sum(IMD_quintile == 1),
              chr_IMD_Q2 = sum(IMD_quintile == 2),
              chr_IMD_Q3 = sum(IMD_quintile == 3),
              chr_IMD_Q4 = sum(IMD_quintile == 4),
              chr_IMD_Q5 = sum(IMD_quintile == 5),
              chr_male = sum(sex == 1),
              chr_female_or_unknown = sum(sex != 1)) %>% 
    ungroup() %>% 
    mutate(pct_total_chr = round(100*chr / sum(chr), 1),
           pct_resident_chr = round(100*resident_chr / chr, 1),
           pct_nursing_chr = round(100*nursing_chr / chr, 1),
           pct_chr_charlson_dementia = round(100*chr_charlson_dementia / chr, 1),
           pct_chr_charlson_copd = round(100*chr_charlson_copd / chr, 1),
           pct_chr_charlson_chf = round(100*chr_charlson_chf / chr, 1),
           pct_chr_charlson_diab = round(100*chr_charlson_diab / chr, 1),
           pct_chr_charlson_canc = round(100*chr_charlson_canc / chr, 1),
           pct_chr_IMD_Q1 = round(100*chr_IMD_Q1 / chr, 1),
           pct_chr_IMD_Q2 = round(100*chr_IMD_Q2 / chr, 1),
           pct_chr_IMD_Q3 = round(100*chr_IMD_Q3 / chr, 1),
           pct_chr_IMD_Q4 = round(100*chr_IMD_Q4 / chr, 1),
           pct_chr_IMD_Q5 = round(100*chr_IMD_Q5 / chr, 1),
           pct_chr_male = round(100*chr_male / chr, 1),
           pct_chr_female_or_unknown = round(100*chr_female_or_unknown / chr, 1)) %>% 
    write_csv(str_c(results_path_hist_snapshot, "CHR_2017-10_summary_", var, ".csv"))
  
}


c("chregion", "la11pidlsoa", "ccg15pidlsoa", "age_group_served", "chspecialist",
  "chnursing", "IMD_quintile", "Over65") %>% 
  map(., characterise_chr, data = residents_current)

# no grouping
characterise_chr(data = residents_current %>%  mutate(dummy = 1), var = "dummy")

# Mean size of care homes by LA
residents_current %>% 
  group_by(la11pidlsoa, tnrchid, chnursing) %>% 
  summarise(chr = n()) %>% 
  group_by(la11pidlsoa) %>% 
  summarise(n_ch = n(),
            mean_chr_per_ch = round(mean(chr, na.rm = TRUE), 1),
            mean_chr_per_ch_residential = round(mean(chr[chnursing == 0], na.rm = TRUE), 1),
            mean_chr_per_ch_nursing = round(mean(chr[chnursing == 1], na.rm = TRUE), 1)) %>% 
  write_csv(str_c(results_path_hist_snapshot, "CHR_per_CH_2017-10_summary_LA.csv"))

# Mean size of care homes by region
residents_current %>% 
  group_by(chregion, tnrchid, chnursing) %>% 
  summarise(chr = n()) %>% 
  group_by(chregion) %>% 
  summarise(n_ch = n(),
            mean_chr_per_ch = round(mean(chr, na.rm = TRUE), 1),
            mean_chr_per_ch_residential = round(mean(chr[chnursing == 0], na.rm = TRUE), 1),
            mean_chr_per_ch_nursing = round(mean(chr[chnursing == 1], na.rm = TRUE), 1)) %>% 
  write_csv(str_c(results_path_hist_snapshot, "CHR_per_CH_2017-10_summary_chregion.csv"))
