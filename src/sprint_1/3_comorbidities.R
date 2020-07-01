####
# Descriptive analysis of historical data
# 2. LTPs of care homes residents
####

library(tidyverse)
library(tidylog)
library(janitor)
library(lubridate)
library(comorbidity)

source("file_paths.R")

# Import data -------------------------------------------------------------

residents <- readRDS(str_c(str_c(hist_data_path_Rds, 'carehome_residents_2018.Rds')))
episodes <- readRDS(str_c(str_c(hist_data_path_Rds, 'chres_apce_2018.Rds')))

# Deriving LTPs from APC data ---------------------------------------------
# this is only for the October 2017 snapshot
# will take into account spells between Oct 2015 and Sep 2017
# and consider all diagnosis codes for each patient

diag_cols <- c("primdiag", "diag02", "diag03", "diag04", "diag05", 
               "diag06", "diag07", "diag08", "diag09", "diag10", 
               "diag11", "diag12", "diag13", "diag14", "diag15", 
               "diag16", "diag17", "diag18", "diag19", "diag20", 
               "diag21", "diag22", "diag23", "diag24")

# pay attention to how scientific notation is handled when uniting and separating columns
diags <- episodes  %>% 
  filter(epistartdate >= ymd("2015-10-01") & epistartdate <= ymd("2017-09-30")) %>% 
  select_at(vars(c("pid", diag_cols))) %>% 
  pivot_longer(cols = diag_cols, names_to = "temp", values_to = "ICD10code") %>% 
  select(-temp)

diags <- diags  %>% 
  filter(!is.na(ICD10code))

charlson <- comorbidity(x = diags, id = "pid", code = "ICD10code", 
                        score = "charlson", icd = "icd10", assign0 = FALSE) %>% 
  rename_all(list(~str_c("charlson_", .)))

elixhauser <- comorbidity(x = diags, id = "pid", code = "ICD10code", 
                          score = "elixhauser", icd = "icd10", assign0 = FALSE) %>% 
  rename_all(list(~str_c("elixhauser_", .)))

comorbidities <- charlson %>% 
  left_join(elixhauser, by = c("charlson_pid" = "elixhauser_pid")) %>% 
  rename("pid" = charlson_pid) 

saveRDS(comorbidities, str_c(hist_data_path_Rds, 'apce_comorbidities_2015-2017.Rds'))

