####
# Descriptive analysis of historical data
# 1. Import data
####

library(tidyverse)
library(tidylog)
library(janitor)
library(lubridate)

source("file_paths.R")

# carehome_2018.csv - list of care homes with characteristics over time, multiple rows per 
# care home when characteristics changed

# carehome_residents_2018.csv - list of all patients living in care homes and when, based on 
# master patient index combined with carehome_2018 file

# chres_aea_2018.csv - A&E SUS records for patients in the care home residents list (includes
# records after 2018)

# chres_apce_2018.csv - APC episode SUS records for patients in the care home residents list (includes
# records after 2018)

# chres_apcs_2018.csv - APC spell SUS records for patients in the care home residents list (includes
# records after 2018). Includes flags for admission types (emergency, elective, etc)


# functions ---------------------------------------------------------------

# Functions to parse dates written as "ddMMMyyyy" with MMM being an all caps abbreviation of the month
parse_SAS_dates <- function(x){
  
  ymd(str_c(as.numeric(substr(x, 6, 9)), str_to_sentence(substr(x, 3, 5)), as.numeric(substr(x, 1, 2))))
}

# care homes --------------------------------------------------------------

carehome <- read_csv(str_c(hist_data_path, "carehome_2018.csv"), 
                     col_types = cols(chcharfrom = col_date(format = "%d/%m/%Y"),
                                      chcharto = col_date(format = "%d/%m/%Y"),
                                      chopen = col_date(format = "%d/%m/%Y"),
                                      chclose = col_date(format = "%d/%m/%Y"),
                                      oldchstart = col_date(format = "%d/%m/%Y"),
                                      oldchend = col_date(format = "%d/%m/%Y")))

saveRDS(carehome, str_c(hist_data_path_Rds, 'carehome_2018.Rds'))


# residents ---------------------------------------------------------------

# pid is the unique patient ID
# chresid is a record ID

residents <- read_csv(str_c(hist_data_path, "carehome_residents_2018.csv"), 
                      col_types = cols(deathdate = col_date(format = "%d/%m/%Y"),
                                       chresfrom = col_date(format = "%d/%m/%Y"),
                                       chresto = col_date(format = "%d/%m/%Y"),
                                       chopen = col_date(format = "%d/%m/%Y"),
                                       chclose = col_date(format = "%d/%m/%Y"),
                                       datefrom = col_date(format = "%d/%m/%Y"),
                                       dateto = col_date(format = "%d/%m/%Y"),
                                       extdatefrom = col_date(format = "%d/%m/%Y"),
                                       extdateto = col_date(format = "%d/%m/%Y"),
                                       dob = col_date(format = "%d/%m/%Y"),
                                       deathmonth = col_date(format = "%d/%m/%Y"),
                                       undeaddeathdate = col_date(format = "%d/%m/%Y"),
                                       pid = col_character()))

saveRDS(residents, str_c(hist_data_path_Rds, 'carehome_residents_2018.Rds'))


# A&E ---------------------------------------------------------------------

aeattends <- read_csv(str_c(hist_data_path, "chres_aea_2018.csv"), 
                      col_types = cols(arrdate = col_date(format = "%d/%m/%Y"),
                                       iniassdate = col_date(format = "%d/%m/%Y"),
                                       attconcldate = col_date(format = "%d/%m/%Y"),
                                       departdate = col_date(format = "%d/%m/%Y"),
                                       rttperenddate = col_date(format = "%d/%m/%Y"),
                                       rttperstartdate = col_date(format = "%d/%m/%Y"),
                                       arrtime = col_time(format = "%H:%M:%S"),
                                       iniasstime = col_time(format = "%H:%M:%S"),
                                       #asswaittime = col_time(format = "%H:%M:%S"),
                                       treatseentime = col_time(format = "%H:%M:%S"),
                                       attconcltime = col_time(format = "%H:%M:%S"),
                                       #conclwaittime = col_time(format = "%H:%M:%S"),
                                       departtime = col_time(format = "%H:%M:%S"),
                                       primdiag = col_character(),
                                       diag02 = col_character(),
                                       diag03 = col_character(),
                                       diag04 = col_character(),
                                       diag05 = col_character(),
                                       diag06 = col_character(),
                                       diag07 = col_character(),
                                       diag08 = col_character(),
                                       diag09 = col_character(),
                                       diag10 = col_character(),
                                       diag11 = col_character(),
                                       diag12 = col_character(),
                                       diag13 = col_character(),
                                       diag14 = col_character(),
                                       diag15 = col_character(),
                                       diag16 = col_character(),
                                       diag17 = col_character(),
                                       diag18 = col_character(),
                                       diag19 = col_character(),
                                       diag20 = col_character(),
                                       diag21 = col_character(),
                                       diag22 = col_character(),
                                       diag23 = col_character(),
                                       diag24 = col_character(),
                                       treat01 = col_character(),
                                       treat02 = col_character(),
                                       treat03 = col_character(),
                                       treat04 = col_character(),
                                       treat05 = col_character(),
                                       treat06 = col_character(),
                                       treat07 = col_character(),
                                       treat08 = col_character(),
                                       treat09 = col_character(),
                                       treat10 = col_character(),
                                       treat11 = col_character(),
                                       treat12 = col_character(),
                                       treat13 = col_character(),
                                       treat14 = col_character(),
                                       treat15 = col_character(),
                                       treat16 = col_character(),
                                       treat17 = col_character(),
                                       treat18 = col_character(),
                                       treat19 = col_character(),
                                       treat20 = col_character(),
                                       treat21 = col_character(),
                                       treat22 = col_character(),
                                       treat23 = col_character(),
                                       treat24 = col_character()))

# parsing failure in 

saveRDS(aeattends, str_c(hist_data_path_Rds, 'chres_aea_2018.Rds'))



# APC episodes ------------------------------------------------------------

# this is one file where dates are formatted in a weird way

episodes <- read_csv(str_c(hist_data_path, "chres_apce_2018.csv"),
                     col_types = cols(admdate = col_character(),
                                      admtime = col_time(format = "%H:%M:%S"),
                                      decideadmdate = col_character(),
                                      dischdate = col_character(),
                                      dischtime = col_time(format = "%H:%M:%S"),
                                      dischreadydate = col_character(),
                                      proc02 = col_character(),
                                      proc03 = col_character(),
                                      proc04 = col_character(),
                                      proc05 = col_character(),
                                      proc06 = col_character(),
                                      proc07 = col_character(),
                                      proc08 = col_character(),
                                      proc09 = col_character(),
                                      proc10 = col_character(),
                                      proc11 = col_character(),
                                      proc12 = col_character(),
                                      proc13 = col_character(),
                                      proc14 = col_character(),
                                      proc15 = col_character(),
                                      proc16 = col_character(),
                                      proc17 = col_character(),
                                      proc18 = col_character(),
                                      proc19 = col_character(),
                                      proc20 = col_character(),
                                      proc21 = col_character(),
                                      proc22 = col_character(),
                                      proc23 = col_character(),
                                      proc24 = col_character(),
                                      ccstartdate01 = col_character(),
                                      ccstartdate02 = col_character(),
                                      ccstartdate03 = col_character(),
                                      ccstartdate04 = col_character(),
                                      ccstartdate05 = col_character(),
                                      ccstartdate06 = col_character(),
                                      ccstartdate07 = col_character(),
                                      ccstartdate08 = col_character(),
                                      ccstartdate09 = col_character(),
                                      ccdischdate01 = col_character(),
                                      ccdischdate02 = col_character(),
                                      ccdischdate03 = col_character(),
                                      ccdischdate04 = col_character(),
                                      ccdischdate05 = col_character(),
                                      ccdischdate06 = col_character(),
                                      ccdischdate07 = col_character(),
                                      ccdischdate08 = col_character(),
                                      ccdischdate09 = col_character(),
                                      ccunitfunct01 = col_double(),
                                      ccunitfunct02 = col_double(),
                                      ccunitfunct03 = col_double(),
                                      ccunitfunct04 = col_double(),
                                      ccunitfunct05 = col_double(),
                                      ccunitfunct06 = col_double(),
                                      ccunitfunct07 = col_double(),
                                      ccunitfunct08 = col_double(),
                                      ccunitfunct09 = col_double(),
                                      epistartdate = col_character(),
                                      epistarttime = col_time(format = "%H:%M:%S"),
                                      epienddate = col_character(),
                                      epiendtime = col_time(format = "%H:%M:%S"),
                                      pid = col_character(),
                                      tnrrcrdid = col_character()))

# parsing failure in tnrepinum

episodes_date_cols <- c("admdate", "decideadmdate", "dischdate", "dischreadydate", 
                        "ccstartdate01", "ccstartdate02", "ccstartdate03", "ccstartdate04", 
                        "ccstartdate05", "ccstartdate06", "ccstartdate07", "ccstartdate08", "ccstartdate09", 
                        "ccdischdate01", "ccdischdate02", "ccdischdate03", "ccdischdate04", 
                        "ccdischdate05", "ccdischdate06", "ccdischdate07", "ccdischdate08", "ccdischdate09", 
                        "epistartdate","epienddate")

episodes <- episodes %>% 
  mutate_at(episodes_date_cols, parse_SAS_dates) 

saveRDS(episodes, str_c(hist_data_path_Rds, 'chres_apce_2018.Rds'))

# APC spells ------------------------------------------------------------

spells <- read_csv(str_c(hist_data_path, "chres_apcs_2018.csv"), 
                   col_types = cols(spellstartdate = col_date(format = "%d/%m/%Y"),
                                    spellenddate = col_date(format = "%d/%m/%Y"),
                                    dischdate = col_date(format = "%d/%m/%Y")))

saveRDS(spells, str_c(hist_data_path_Rds, 'chres_apcs_2018.Rds'))
