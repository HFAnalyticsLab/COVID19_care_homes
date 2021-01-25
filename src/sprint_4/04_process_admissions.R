####
# Process hospital admissions
####

library(tidyverse)
library(tidylog)
library(janitor)
library(lubridate)
library(data.table)
library(ISOweek)
library(tableone)
library(RColorBrewer)


source("file_paths.R")
source("functions.R")

# Import data -------------------------------------------------------------

# Import cohort data
chres_cohorts <- readRDS(str_c(Rds_data_path, 'sprint_4/chres_cohorts_Jan.Rds')) 

chres_pids <- unique(chres_cohorts$pid)

# SUS spells
apcs <- read_csv(str_c(raw_data_path, "care home paper/apcs_2019_20.csv"),
                 col_types = cols(pid = col_character(),
                                  spellstartdate = col_date(format = "%d/%m/%Y"),
                                  spellenddate = col_date(format = "%d/%m/%Y"),
                                  dischdate = col_date(format = "%d/%m/%Y")))


# SUS diagnosis codes from episodes table
# This was converted into a long-table and tagged with spell IDs by RB
# can be linked back to SUS spells using patient id and spell id
# this will be used to figure out if *any* diagnosis code was covid, as 
# the spells table only contains the primary diagnosis code


apce_diag <- fread(str_c(raw_data_path, "care home paper/apcs_2019_20_diag_codes.csv"))

apce_diag_covid <- apce_diag %>% 
  filter(dc == "U071"| dc == "U072") 

# IDs of spells that have a covid diagnosis code
apce_diag_covid <- apce_diag_covid %>% 
  distinct(spellid) %>% 
  mutate(covid_anydiag = 1)
  
saveRDS(apce_diag_covid,  str_c(Rds_data_path, 'sprint_4/apce_diag_covid.Rds'))

# Filtering hospital spells
# First step:
# only keep admissions for patients in one of the two cohorts, 
# that happen during the study period, ie between Jan and June
# exclude private patients

apcs2 <- apcs %>% 
  filter(pid %in% unique(chres_pids)) %>% 
  filter((spellstartdate >= ymd("2019-01-01") & spellstartdate <= ymd("2019-06-30")) |
           (spellstartdate >= ymd("2020-01-01") & spellstartdate <= ymd("2020-06-30"))) %>% 
  filter(admincat != 2)

saveRDS(apcs2,  str_c(Rds_data_path, 'sprint_4/apcs_chr_cohorts_raw.Rds'))

# Second step:
# Now we need to make sure that the patient had a care home id at the time of admission
# This is done using the interval join function from the data.table package

chres_apcs <- as.data.table(apcs2)
chres_apcs[, spellstartdate2 := spellstartdate]

chres_cohorts_dt <-  chres_cohorts %>% 
  ungroup() %>%  
  mutate(chr_at_spellstart = 1) %>% 

  as.data.table()
setkey(chres_cohorts_dt, pid, extractstart, followup_end)

chres_apcs <- foverlaps(chres_apcs, chres_cohorts_dt, 
                        by.x = c("pid", "spellstartdate", "spellstartdate2"),
                        by.y = c("pid", "extractstart", "followup_end"))

chres_apcs <- chres_apcs %>% 
  rename(MPI_extractstart = "extractstart", MPI_followup_end = "followup_end")

# Filter out admissions where there was no care home id at the time of admission
# Create spell start date dummy for plotting different years in the same graph
# Note that the year needs to be 0004 as one of our years of interest is a leap year
# otherwise Feb 29 converts to NA!
chres_apcs <- chres_apcs %>%  
  filter(chr_at_spellstart == 1) %>% 
  select(-chr_at_spellstart, -spellstartdate2) %>% 
  mutate(year = year(spellstartdate),
         spellstartdate_dummy = `year<-`(spellstartdate, 0004))

# Flag for primary diagnosis COVID
chres_apcs <- chres_apcs %>%  
  mutate(covid_prim = if_else(!is.na(primdiag) & (primdiag == "U071"| primdiag == "U072"), 1, 0))

# Flag for COVID in other diagnosis codes 
chres_apcs <- chres_apcs %>%  
  left_join(apce_diag_covid[, c("spellid", "covid_anydiag")], by = "spellid") %>% 
  mutate(covid_anydiag = replace_na(covid_anydiag, 0),
         covid_notprim = if_else(covid_prim == 0 & covid_anydiag == 1, 1, 0))

# Flags for avoidable admissions and causes (emergency admissions only)
chres_apcs <- chres_apcs %>%
  categorise_avoidableadm("primdiag", new_col = "avoidable_cause") %>% 
  mutate(avoidable_cause = if_else(emergency == 0, NA_character_, avoidable_cause),
         avoidable_emergency = ifelse(!is.na(avoidable_cause), 1, 0))

# Create variables used to aggregate at weekly and monthly level later
chres_apcs <- chres_apcs %>%
  mutate(spellstartweek = format(spellstartdate, format = "%V"),
         spellstartweekstart = ISOweek2date(paste0(year, 
                                                   "-W", 
                                                   str_pad(spellstartweek, width = 2, 
                                                           side = "left", pad = "0"),
                                                   "-1")),
         spellstartmonth = floor_date(spellstartdate, "month"))

# Generate variable with ICD-10 chapters of primary diagnosis
chres_apcs <- chres_apcs %>%
  categorise_ICD10("primdiag", new_col = "MainICD10Cat")

# If admission was for COVID, assume that it was an emergency
chres_apcs %>% 
  tabyl(covid_prim, elective) %>%  adorn_title()

# NB: elod = elective, ordinary admission or day case
chres_apcs <- chres_apcs %>%
  mutate(emergency = if_else(covid_prim == 1 & elective == 1, 1, emergency),
         elective = if_else(covid_prim == 1, 0, elective),
         elod = if_else(covid_prim == 1, 0, elod))

chres_apcs <- chres_apcs %>%
  mutate(elective_not_elod = if_else(elective == 1 & elod == 0, 1, 0),
         emergency_noncovid = if_else(emergency == 1 & covid_prim == 0, 1, 0),
         emergency_covid = if_else(emergency == 1 & covid_prim == 1, 1, 0),
         emelod_covid_notprim = if_else(covid_notprim == 1 & (elod == 1 | emergency == 1), 1, 0))

saveRDS(chres_apcs,  str_c(Rds_data_path, 'sprint_4/apcs_chr_cohorts.Rds'))


# Characteristics of admitted patients ------------------------------------


###########
# Covid vs. non-covid in March to May, by care home type
# Only include electives and emergencies (not "other" admissions)
# --> in paper
###########

vars_tosummarise_emelod <- c("sex", "age", "ch_beds", 
                      "IMD19_QUINTILE", "RGN19NM", "i_charlson_h36",
                      "f_dementia_h36")

cat_vars_tosummarise_emelod <- c('sex',  
                          "IMD19_QUINTILE","RGN19NM",
                          "f_dementia_h36")

table1_covid_emelod <- CreateTableOne(vars = vars_tosummarise_emelod, 
                               data = chres_apcs %>% 
                                 filter(month(spellstartdate) %in% c(3,4,5) & (elod == 1 | emergency == 1)), 
                               strata = c("year", 'covid_prim', "ch_nursing"), 
                               factorVars = cat_vars_tosummarise_emelod,
                               test = FALSE, includeNA = TRUE)

table1_covid_emelod_csv <- print(table1_covid_emelod, 
                          noSpaces = TRUE,
                          showAllLevels = FALSE) 

write.csv(table1_covid_emelod_csv, str_c(results_path_sprint4, 'res_chars/Chr_covid_chnursing_emelod_characteristics_March-May.csv'))

# standardised mean difference
covid_emelod_smd <- ExtractSmd(table1_covid_emelod) %>%  
  as.data.frame() %>% 
  rownames_to_column(var = "variable") %>% 
  select(variable,
         `Nursing: Other (2019) vs Other (2020)` = `5 vs 6`,
         `Nursing: Other (2020) vs COVID (2020)` = `6 vs 8`,
         `Residential: Other (2019) vs Other (2020)` = `1 vs 2`,
         `Residential: Other (2020) vs COVID (2020)` = `2 vs 4`) %>% 
  pivot_longer(-variable, names_to = "comparison", values_to = "SMD") %>% 
  mutate(comparison = gsub("_", " ", comparison),
        variable = case_when(variable == "sex" ~ "Female",
                             variable == "RGN19NM" ~ "Region",
                             variable == "IMD19_QUINTILE" ~ "IMD quintile",
                             variable == "i_charlson_h36" ~ "Charlson Index",
                             variable == "f_dementia_h36" ~ "Dementia",
                             variable == "age" ~ "Age",
                             variable == "ch_beds" ~ "Bed capacity"),
        variable = factor(variable, levels = c("Female", "Age", "Charlson Index", "Dementia",
                                               "IMD quintile", "Region", "Bed capacity")))

write.csv(covid_emelod_smd, str_c(results_path_sprint4, 'res_chars/Chr_covid_chnursing_emelod_characteristics_March-May_SMD.csv'))


(covid_emelod_smd %>% 
    ggplot(aes(x = fct_rev(variable), y = SMD*100, group = comparison, color = comparison)) +
    geom_point() +
    coord_flip() +
    ylab("Absolute standardised mean difference (%)") +
    labs(title = "Hospital admissions from care homes (emelod)",
         subtitle = "March - May") +
    theme_bw() +
    theme(axis.title.y = element_blank(),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          legend.title = element_blank(),
          legend.position =  "right") +
    scale_color_manual(values = brewer.pal(name ="RdBu", n = 4)[c(2,1,3,4)],
                       guide = guide_legend(ncol = 1)) +
    geom_hline(yintercept = 10, linetype = "dashed", alpha = 0.3)) %>% 
  ggsave(str_c(results_path_sprint4, 'res_chars/Chr_covid_chnursing_emelod_characteristics_March-May_SMD.png'), ., device = "png", 
         dpi = 600, width = 7, height = 4)

  

# Outcomes for covid admissions -------------------------------------------

chres_apcs %>% 
  filter(covid_prim == 1 & emergency == 1) %>% 
  mutate(died_in_hospital = ifelse(dischmthd == 4, 1, 0)) %>% 
  group_by(spellstartweekstart) %>% 
  summarise(covid_admissions =  n(),
            died_in_hospital =  sum(died_in_hospital == 1)) %>% 
  mutate(pct_died = round(100*died_in_hospital / (covid_admissions), 2)) %>% 
  write_csv(str_c(results_path_sprint4, 'adm_outcomes/Chr_covid_prim_outcomes.csv'))
  
chres_apcs %>% 
  filter(covid_anydiag == 1 & (emergency == 1 | elod == 1)) %>% 
  mutate(died_in_hospital = ifelse(!is.na(dischmthd) & dischmthd == 4, 1, 0)) %>% 
  group_by(spellstartweekstart) %>% 
  summarise(covid_admissions =  n(),
            died_in_hospital =  sum(died_in_hospital == 1)) %>% 
  mutate(pct_died = round(100*died_in_hospital / (covid_admissions), 2)) %>% 
  write_csv(str_c(results_path_sprint4, 'adm_outcomes/Chr_covid_any_outcomes.csv'))



# Number of residents / percentage of cohort ever admitted for COViD --------

covid_admissions <- chres_apcs %>%
  filter(month(spellstartdate) %in% c(3,4,5)) %>% 
  group_by(pid, year) %>% 
  summarise(emergency_covid_prim = sum(emergency_covid == 1),
            emelod_covid_notprim = sum(emelod_covid_notprim == 1)) 

covid_admissions2 <-  chres_cohorts %>% 
  mutate(cohort_year = year(extractstart)) %>%
  select(pid, cohort_year, ch_nursing) %>%
  left_join(covid_admissions, by = c("pid", "cohort_year" = "year")) 

covid_admissions_summary <- covid_admissions2 %>%  
  group_by(ch_nursing, cohort_year) %>% 
  summarise(cohort_size = n(),
            admitted_covid_primary = sum(!is.na(emergency_covid_prim) & emergency_covid_prim == 1),
            admitted_covid_notprimary = sum(!is.na(emelod_covid_notprim) & emelod_covid_notprim == 1)) %>% 
  filter(cohort_year == 2020) %>% 
  mutate(admitted_covid_prim_pct = round(100*admitted_covid_primary/cohort_size, 2),
         admitted_covid_notprim_pct = round(100*admitted_covid_notprimary/cohort_size, 2))

write_csv(covid_admissions_summary, str_c(results_path_sprint4, "adm_rates/Chres_admitted_covid.csv"))
