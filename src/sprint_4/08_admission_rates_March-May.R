####
### Calculate admission rates for the time between 1 March and 31 May
####

library(tidyverse)
library(tidylog)
library(janitor)
library(lubridate)

source("file_paths.R")


# Import data -------------------------------------------------------------

chres_denom_apcs_tot <- readRDS(str_c(Rds_data_path, 'sprint_4/analysis_ds_March-May.Rds'))

# elod is based on patient classifcation
# this includes ordinary admissions and day cases (not regular attenders)

# Rates: all residents ---------------------------------

# Calculate annual admission rate per person 
# This is done by summing up the total number of admissions for all care home residents for March-May
# and dividing by the number of days spent in a care home across all residents in that same week
# Finally, this number is multiplied by 365 to provide the rate per person per year

# By care home type

rate_chnursing_tot <- chres_denom_apcs_tot %>%  
  group_by(cohort_year, ch_nursing) %>% 
  summarise_at(vars(matches("admissions|elod$|emergency$|emergency_stroke|emergency_accorosyndr|emergency_noncovid|covid_prim|avoidable"), "days_ch"), sum, na.rm = TRUE) %>% 
  pivot_longer(c(-cohort_year, -days_ch, -ch_nursing), names_to = "type", values_to = "count") %>%
  mutate(rate_daily = count/days_ch,
         rate_annual = 365*rate_daily) %>%
  arrange(type)

write_csv(rate_chnursing_tot, str_c(results_path_sprint4, "adm_rates/Adm_rates_chrs_chnursing_March-May.csv"))

rate_chnursing_tot_annualrate_wide <- rate_chnursing_tot %>% 
  pivot_wider(c(-days_ch, -count, - rate_daily), 
              names_from = "cohort_year", 
              values_from = "rate_annual", names_prefix = "Year_") %>% 
  mutate(change = round(Year_2020 - Year_2019, 5),
         pctchange = round(100*Year_2020/Year_2019 - 100, 0),
         combined = str_c(change, " (", pctchange, ")"),
         Year_2019 = round(Year_2019, 5),
         Year_2020 = round(Year_2020, 5))

write_csv(rate_chnursing_tot_annualrate_wide, str_c(results_path_sprint4, "adm_rates/Adm_rates_chrs_chnursing_March-May_rateswide.csv"))

rate_chnursing_tot_count_wide <- rate_chnursing_tot %>% 
  pivot_wider(c(-days_ch, -rate_annual, - rate_daily), 
              names_from = "cohort_year", 
              values_from = "count", names_prefix = "Year_") %>% 
  mutate(change = Year_2020 - Year_2019,
         pctchange = round(100*Year_2020/Year_2019 - 100, 0),
         combined = str_c(change, " (", pctchange, ")"))

write_csv(rate_chnursing_tot_count_wide, str_c(results_path_sprint4, "adm_rates/Adm_rates_chrs_chnursing_March-May_countwide.csv"))

