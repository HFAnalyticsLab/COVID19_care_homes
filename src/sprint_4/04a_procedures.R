####
# Process hospital procedures
####

library(tidyverse)
library(tidylog)
library(janitor)
library(lubridate)
library(data.table)
library(tableone)


source("file_paths.R")
source("functions.R")

# Import data -------------------------------------------------------------


# SUS procedures codes from episodes table
# This was converted into a long-table and tagged with spell IDs by RB
# can be linked back to SUS spells using patient id and spell id
# this will be used to figure out if *any* diagnosis code was covid, as 
# the spells table only contains the primary diagnosis code

apce_proc <- fread(str_c(raw_data_path, "care home paper/apcs_2019_20_proc_codes.csv"))

# spells (cleaned)
chres_apcs <- readRDS(str_c(Rds_data_path, 'sprint_4/apcs_chr_cohorts.Rds'))


# Process -----------------------------------------------------------------

# Only keep procedures for relevant spells 
apce_proc <- apce_proc %>% 
  semi_join(chres_apcs, by = "spellid")

# Add in primary diagnosis
apce_proc <- apce_proc %>% 
  left_join(chres_apcs[, c("spellid", "primdiag","ch_nursing", "emergency", "elod", "spellstartdate")], by = "spellid")

# de-deduplicate (we don't care if several instances of the same procedures were carried out in a spell)
apce_proc <- apce_proc %>% 
  arrange(spellid, desc(isprimproc)) %>% 
  distinct(spellid, pc, .keep_all = TRUE)


# Filter for right time frame and admission methods
apce_proc <- apce_proc %>% 
  filter(month(spellstartdate) %in% c(3,4,5) & (elod == 1 | emergency == 1))

length(unique(apce_proc$spellid))


# Flag cataract procedures
apce_proc <- apce_proc %>% 
  flag_proc_cataract("pc", new_col = "proc_cat")
  
  mutate(cohort_year = year(spellstartdate),
         pc_3 = substr(pc, 1, 3),
         pc_chapter = substr(pc,1,1)) 

# Create spell-level summary with a flag to indicate whether any cataract procedure was carried out
apce_proc_spellsummary <- apce_proc %>% 
  group_by(ch_nursing, emergency, cohort_year, spellid) %>% 
  summarise(cataract = if_else(sum(proc_cat == TRUE)>0, 1, 0))

# Summary of total number for each year
apce_proc_summary <- apce_proc_spellsummary %>% 
  group_by(ch_nursing, emergency, cohort_year) %>% 
  summarise(cataract = sum(cataract))

write.csv(apce_proc_summary, str_c(results_path_sprint4, 'procedures/Spells_all_proccodes_March-May.csv'))

# Summarise procedures by OPCS4 chapter
# March to May

vars_tosummarise <- c("proc_cat", "pc_chapter")

table1_proccat <- CreateTableOne(vars = vars_tosummarise, 
                          data = apce_proc, 
                          factorVars = vars_tosummarise,
                          strata = c("cohort_year", "emergency", "ch_nursing"),
                          test = FALSE, includeNA = TRUE, addOverall = FALSE)

table1_proccat_csv <- print(table1_proccat, 
                               noSpaces = TRUE,
                               showAllLevels = FALSE, format = "f") 

write.csv(table1_proccat_csv, str_c(results_path_sprint4, 'procedures/All_proccodes_March-May.csv'))
