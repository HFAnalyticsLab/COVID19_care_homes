####
### Create analysis datas sets by combining
# - weekly admissions
# - admissions between March and May
####

library(tidyverse)
library(tidylog)
library(janitor)
library(lubridate)

source("file_paths.R")

source("functions.R")

# Import data -------------------------------------------------------------

# cohort characteristics
chres_cohorts_ts <- readRDS(str_c(Rds_data_path, 'sprint_4/chres_cohorts_Jan.Rds')) %>% 
  mutate(cohort_year = year(extractstart))

# weekly denominator, days spent in care home per resident
chres_denom_weekly <- readRDS(str_c(Rds_data_path, 'sprint_4/chres_denom_weekly.Rds'))

# denominator for March - May, days spent in care home per resident
chres_denom_tot <- readRDS(str_c(Rds_data_path, 'sprint_4/chres_denom_March-May.Rds'))

# hospital spells
chres_apcs <- readRDS(str_c(Rds_data_path, 'sprint_4/apcs_chr_cohorts.Rds'))

# Summarise weekly admissions per resident --------------------------------

chres_apcs_weekly <- chres_apcs %>% 
  group_by(pid, spellstartweek, spellstartweekstart, year) %>% 
  summarise(admissions = n(),
            emergency = sum(emergency == 1),
            elective = sum(elective == 1),
            elod = sum(elod == 1),
            elective_not_elod = sum(elective_not_elod == 1),
            other = sum(otheradm == 1),
            emergency_avoidable = sum(avoidable_emergency == 1),
            emergency_acuteLRTI = sum(emergency == 1 & avoidable_cause == "acute LRTIs"),
            emergency_chronLRTI = sum(emergency == 1 & avoidable_cause == "chronic LRTIs"),
            emergency_pressuresore = sum(emergency == 1 & avoidable_cause == "pressure sores"),
            emergency_diabetes = sum(emergency == 1 & avoidable_cause == "diabetes"),
            emergency_fdissues = sum(emergency == 1 & avoidable_cause == "food and drink issues"),
            emergency_flpneumon = sum(emergency == 1 & avoidable_cause == "food and liquid pneumonitis"),
            emergency_frac = sum(emergency == 1 & avoidable_cause == "fractures and sprains"),
            emergency_inestinfec = sum(emergency == 1 & avoidable_cause == "intestinal infections"),
            emergency_pneumonia = sum(emergency == 1 & avoidable_cause == "pneunomia"),
            emergency_uti = sum(emergency == 1 & avoidable_cause == "uti"),
            emergency_covid_prim = sum(emergency_covid == 1),
            emelod_covid_notprim = sum(emelod_covid_notprim == 1),
            emergency_noncovid = sum(emergency_noncovid == 1)) 


chres_apcs_weekly_ICD <- chres_apcs %>% 
  filter((elod == 1 | emergency == 1) & !is.na(MainICD10Cat)) %>% 
  group_by(pid, spellstartweek, spellstartweekstart, year, emergency, elod, MainICD10Cat) %>% 
  summarise(admissions = n()) %>% 
  mutate(admission_type = ifelse(emergency == 1, str_c("emergency_ICD", MainICD10Cat), str_c("elod_ICD", MainICD10Cat))) %>% 
  ungroup() %>% 
  select(-emergency, -elod, - MainICD10Cat) %>% 
  pivot_wider(names_from = "admission_type", values_from = "admissions") 


# Combine with resident info and denominator days
# Only retain full weeks in both years

chres_denom_weekly <- chres_denom_weekly %>% 
  left_join(chres_cohorts_ts %>% 
              select(pid, cohort_year, sex, ch_nursing), by = c("pid", "cohort_year")) %>% 
  left_join(chres_apcs_weekly, by = c("pid", "cohort_year" = "year", 
                                      "week" = "spellstartweek",
                                      "weekstart" = "spellstartweekstart"))%>% 
  left_join(chres_apcs_weekly_ICD, by = c("pid", "cohort_year" = "year", 
                                      "week" = "spellstartweek",
                                      "weekstart" = "spellstartweekstart"))


saveRDS(chres_denom_weekly,  str_c(Rds_data_path, 'sprint_4/analysis_ds_weekly.Rds'))

# Summarise admissions per resident in March - May ----

chres_apcs_tot <- chres_apcs %>% 
  filter(month(spellstartdate) %in% c(3,4,5)) %>% 
  group_by(pid, year) %>% 
  summarise(admissions = n(),
            emergency = sum(emergency == 1),
            elective = sum(elective == 1),
            elod = sum(elod == 1),
            elective_not_elod = sum(elective_not_elod == 1),
            other = sum(otheradm == 1),
            emergency_avoidable = sum(avoidable_emergency == 1),
            emergency_acuteLRTI = sum(emergency == 1 & avoidable_cause == "acute LRTIs"),
            emergency_chronLRTI = sum(emergency == 1 & avoidable_cause == "chronic LRTIs"),
            emergency_pressuresore = sum(emergency == 1 & avoidable_cause == "pressure sores"),
            emergency_diabetes = sum(emergency == 1 & avoidable_cause == "diabetes"),
            emergency_fdissues = sum(emergency == 1 & avoidable_cause == "food and drink issues"),
            emergency_flpneumon = sum(emergency == 1 & avoidable_cause == "food and liquid pneumonitis"),
            emergency_frac = sum(emergency == 1 & avoidable_cause == "fractures and sprains"),
            emergency_inestinfec = sum(emergency == 1 & avoidable_cause == "intestinal infections"),
            emergency_pneumonia = sum(emergency == 1 & avoidable_cause == "pneunomia"),
            emergency_uti = sum(emergency == 1 & avoidable_cause == "uti"),
            emergency_covid_prim = sum(emergency_covid == 1),
            emelod_covid_notprim = sum(emelod_covid_notprim == 1),
            emergency_noncovid = sum(emergency_noncovid == 1),
            emergency_stroke = sum(emergency_stroke == 1 ),
            emergency_accorosyndr = sum(emergency_accorosyndr == 1)) %>% 
  
  mutate_at(vars(matches("emergency_|covid_")), replace_na, 0)

chres_apcs_tot_ICD <- chres_apcs %>% 
  filter(month(spellstartdate) %in% c(3,4,5) & (elod == 1 | emergency == 1) & !is.na(MainICD10Cat)) %>% 
  group_by(pid, year, emergency, elod, MainICD10Cat) %>% 
  summarise(admissions = n()) %>% 
  mutate(admission_type = ifelse(emergency == 1, str_c("emergency_ICD", MainICD10Cat), str_c("elod_ICD", MainICD10Cat))) %>% 
  ungroup() %>% 
  select(-emergency, -elod, - MainICD10Cat) %>% 
  pivot_wider(names_from = "admission_type", values_from = "admissions") %>% 
  mutate_at(vars(matches("[emergency|elod]_")), replace_na, 0)

chres_denom_tot <-  chres_denom_tot %>% 
  left_join(chres_cohorts_ts %>% 
              select(pid, cohort_year, sex, ch_nursing), by = c("pid", "cohort_year")) %>% 
  left_join(chres_apcs_tot, by = c("pid", "cohort_year" = "year"))%>% 
  left_join(chres_apcs_tot_ICD, by = c("pid", "cohort_year" = "year")) 

saveRDS(chres_denom_tot,  str_c(Rds_data_path, 'sprint_4/analysis_ds_March-May.Rds'))
