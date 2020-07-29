####
# Hospital admissions / discharges of care home residents with MPI flag
####

library(tidyverse)
library(tidylog)
library(janitor)
library(lubridate)
library(zoo)
library(data.table)

source("file_paths.R")
source("functions.R")


# Import data -------------------------------------------------------------
# 2017 - now

# Residents and care home characteristics
chres <- readRDS(str_c(Rds_data_path, 'mmpiss_bymonth_chchars.Rds'))  


# Hospital inpatient spells
chres_apcs <- read_csv(str_c(raw_data_path, "April 2020/chres_apcs.csv"), 
                       col_types = cols(pid = col_character(),
                                        spellstartdate =  col_date(format = "%d/%m/%Y"),
                                        spellenddate =  col_date(format = "%d/%m/%Y"),
                                        dischdate = col_date(format = "%d/%m/%Y")))

chres_apcs <- chres_apcs %>% 
  filter(spellstartdate >= ymd("2015-01-01"))

#saveRDS(chres_apcs, str_c(Rds_data_path, 'chres_apcs_2017-2020.Rds'))
#chres_apcs <- readRDS(str_c(Rds_data_path, 'chres_apcs_2017-2020.Rds'))


# Match with care home resident characteristics ---------------------------

#chres_apcs <- chres_apcs %>% 
#  mutate(spellstartdate_floor = floor_date(spellstartdate, "month"),
#         spellenddate_floor = floor_date(spellenddate, "month"))

# Flagging who was a resident when admitted

chres_dt_spellstart <-  chres %>% 
  ungroup() %>% 
  select(pid, assigned_tnrchid, ch_nursing, ch_residential, ch_beds_filled, ch_is_open,
         datefrom, dateto) %>% 
  mutate(chr_at_spellstart = 1) %>% 
  as.data.table()

setkey(chres_dt_spellstart, pid, datefrom, dateto)

chres_dt_spellend <-  chres %>% 
  ungroup() %>% 
  select(pid, assigned_tnrchid, ch_nursing, ch_residential, ch_beds_filled, ch_is_open,
         datefrom, dateto) %>% 
  mutate(chr_at_spellend = 1) %>% 
  as.data.table()

setkey(chres_dt_spellend, pid, datefrom, dateto)


# Duplicate columns (need separate period start and period end for interval join)
chres_apcs <- as.data.table(chres_apcs)
chres_apcs[, spellstartdate2 := spellstartdate]
chres_apcs[, spellenddate2 := spellenddate]


chres_apcs <- foverlaps(chres_apcs, chres_dt_spellstart, 
                        by.x = c("pid", "spellstartdate", "spellstartdate2"),
                        by.y = c("pid", "datefrom", "dateto"))

chres_apcs <- chres_apcs %>% 
  rename(datefrom_ch_spellstart = "datefrom", dateto_ch_spellstart = "dateto",
         assigned_tnrchid_spellstart = "assigned_tnrchid", ch_nursing_spellstart = "ch_nursing", 
         ch_residential_spellstart = "ch_residential", ch_beds_filled_spellstart = "ch_beds_filled", 
         ch_is_open_spellstart = "ch_is_open")


chres_apcs <- foverlaps(chres_apcs, chres_dt_spellend, 
                        by.x = c("pid", "spellenddate", "spellenddate2"),
                        by.y = c("pid", "datefrom", "dateto"))


chres_apcs <- chres_apcs %>% 
  rename(datefrom_ch_spellend = "datefrom", dateto_ch_spellend = "dateto",
         assigned_tnrchid_spellend = "assigned_tnrchid", ch_nursing_spellend = "ch_nursing", 
         ch_residential_spellend = "ch_residential", ch_beds_filled_spellend = "ch_beds_filled", 
         ch_is_open_spellend = "ch_is_open")

chres_apcs <-  chres_apcs %>% 
  mutate(chr_at_spellstart = ifelse(is.na(chr_at_spellstart), 0, chr_at_spellstart),
         chr_at_spellend = ifelse(is.na(chr_at_spellend), 0, chr_at_spellend)) %>% 
  select(-spellstartdate2, -spellenddate2)


# Creating location type variables
chres_apcs <-  chres_apcs %>%
  mutate(ch_admission = ifelse(chr_at_spellstart == 1 & (is.na(admincat) | admincat != 2), 1, 0),
         ch_discharge = ifelse(chr_at_spellend == 1 & (is.na(admincat) | admincat != 2) & 
                                 !is.element(dischdest, 
                                             c(98, 51, 52, 53, 30, 37, 38, 48, 49, 50, 66, 84, 87)), 1 , 0),
         ch_discharge = ifelse(dischdest == 79, NA, ch_discharge))

# Creating admission type variables
chres_apcs <-  chres_apcs %>%
  mutate(adm_type = case_when(admmthd %in% c("21", "22", "23", "24", "25", "28", "2A", "2B", "2C", "2D") ~ "emergency",
                              admmthd %in% c("11", "12", "13") ~ "elective",
                              TRUE ~ "other"))

# Care home type
chres_apcs <-  chres_apcs %>%
  mutate(ch_type_spellstart = case_when(!is.na(ch_nursing_spellstart) & ch_nursing_spellstart == 1 ~ "nursing",
                                        !is.na(ch_residential_spellstart) & ch_residential_spellstart == 1 ~ "residential",
                                        TRUE ~ "unknown"),
         ch_type_spellend = case_when(!is.na(ch_nursing_spellend) & ch_nursing_spellend == 1 ~ "nursing",
                                        !is.na(ch_residential_spellend) & ch_residential_spellend == 1 ~ "residential",
                                        TRUE ~ "unknown"))
         
         
# COVID primary diagnosis
chres_apcs <-  chres_apcs %>%
  mutate(covid_prim = ifelse(!is.na(primdiag) & (primdiag == "U071" | primdiag == "U072"), 1, 0))
         
 
View(chres_apcs %>%  
       filter(spellstartdate == ymd("2020-01-15")) %>%  
       select(pid, spellstartdate, assigned_tnrchid_spellstart, ch_nursing_spellstart, admmthd, adm_type,  datefrom_ch_spellstart, chr_at_spellstart, ch_admission, ch_type_spellstart, spellenddate, assigned_tnrchid_spellend, chr_at_spellend, ch_discharge, dischdest, admincat, datefrom_ch_spellend, dateto_ch_spellend, ch_type_spellend,  ch_nursing_spellend))
        
saveRDS(chres_apcs, str_c(Rds_data_path, 'chres_apcs_2015-2020_mmpiss_matched.Rds'))
chres_apcs <- readRDS(str_c(Rds_data_path, 'chres_apcs_2015-2020_mmpiss_matched.Rds'))


# Counting daily admissions and discharges ------------------------------------


chr_admissions <- chres_apcs %>% 
  filter(ch_admission == 1) %>% 
  group_by(spellstartdate) %>% 
  summarise(adm_all = n(),
            adm_emergency = sum(adm_type == "emergency"),
            adm_elective = sum(adm_type == "elective"),
            adm_other = sum(adm_type == "other"),
            adm_nursing = sum(ch_type_spellstart == "nursing"),
            adm_residential = sum(ch_type_spellstart == "residential"),
            adm_unknown = sum(ch_type_spellstart == "unknown"),
            adm_covid = sum(covid_prim == 1)) 

chr_discharges <- chres_apcs %>% 
  filter(ch_discharge == 1) %>% 
  group_by(spellenddate) %>% 
  summarise(disch_all = n(),
            disch_nursing = sum(ch_type_spellstart == "nursing"),
            disch_residential = sum(ch_type_spellstart == "residential"),
            disch_unknown = sum(ch_type_spellstart == "unknown")) 

chr_adm_disch <- chr_admissions %>% 
  left_join(chr_discharges, by = c("spellstartdate" = "spellenddate")) %>% 
  rename(date = "spellstartdate") %>% 
  pivot_longer(-date, names_to = "temp", values_to = "count") %>% 
  separate("temp", into = c("type", "group"), sep = "_") %>% 
  group_by(type, group) %>% 
  arrange(date) %>% 
  mutate(sevendayavg = sevendayavg(count),
         year = year(date),
         day_dummy = `year<-`(date, 0004))

chr_adm_disch <- chr_adm_disch %>% 
  group_by(type, group, day_dummy) %>% 
  mutate(ref_sevendayavg_2015_to_2019 = mean(sevendayavg[year %in% c(2015:2019)]),
         pct_sevendayavg_2015_to_2019 = round(100*(sevendayavg/ ref_sevendayavg_2015_to_2019), 1))

write_csv(chr_adm_disch, "../results_sprint3/Chr_perm_admissions_discharges.csv")



# Visualisation  ----------------------------------------------------------

(chr_adm_disch %>% 
   filter(group == "all") %>% 
    ggplot(aes(x = day_dummy, y = sevendayavg, group = factor(year),
               color = factor(year))) +
    scale_x_date(date_labels = "%b", date_breaks = "1 month") +
    geom_line() +
    geom_point(size = 1) +
    theme_bw() +
    facet_wrap("type") +
    theme(panel.grid.minor.x = element_line(),
          axis.title.x =  element_blank())+
    ylab("7-day rolling average") +
    labs(title = "Hospital admissions and discharges", subtitle = "Care home residents flagged in the MPI")) %>% 
  ggsave("../results_sprint3/Chr_admissions_discharges.png",., device = "png")

(chr_adm_disch %>% 
    filter(group == "all" & day_dummy %within% interval(ymd("0004-02-01"), ymd("2020-04-30"))) %>% 
    ggplot(aes(x = day_dummy, y = sevendayavg, group = factor(year),
               color = factor(year))) +
    scale_x_date(date_labels = "%b", date_breaks = "1 month") +
    geom_line() +
    geom_point(size = 1) +
    theme_bw() +
    facet_wrap("type") +
    theme(panel.grid.minor.x = element_line(),
          axis.title.x =  element_blank())+
    ylab("7-day rolling average") +
    labs(title = "Hospital admissions and discharges", subtitle = "Care home residents flagged in the MPI")) %>% 
  ggsave("../results_sprint3/Chr_admissions_discharges_Jan_Apr.png",., device = "png")


(chr_adm_disch %>% 
    filter(group == "all") %>% 
    group_by(date, day_dummy, year) %>% 
    mutate(ratio =  sevendayavg[type == "disch"]/sevendayavg[type == "adm"]) %>% 
    ggplot(aes(x = day_dummy, y = ratio, group = factor(year),
               color = factor(year))) +
    scale_x_date(date_labels = "%b", date_breaks = "1 month") +
    geom_line() +
    geom_point(size = 1) +
    theme_bw() +
    theme(panel.grid.minor.x = element_line(),
          axis.title.x =  element_blank())+
    ylab("discharges/admissions (7-day rolling averages)") +
    labs(title = "Discharges/admissions ratio", subtitle = "Care home residents flagged in the MPI")) %>% 
  ggsave("../results_sprint3/Chr_discharges_to_admissions_ratio.png",., device = "png", width = 10)


chr_adm_disch_2020 <- chr_adm_disch %>% 
  filter(group == "all" & year == 2020 & day_dummy %within% interval(ymd("0004-02-01"), ymd("2020-04-30")))




(chr_adm_disch_2020 %>% 
    ggplot(aes(x = day_dummy, y = pct_sevendayavg_2015_to_2019, color = type)) +
    scale_x_date(date_labels = "%b", date_breaks = "1 month") +
    geom_line() +
    geom_point(size = 1) +
    theme_bw() +
    theme(panel.grid.minor.x = element_line(),
          axis.title.x =  element_blank())+
    ylab("Percent") +
    labs(title = "Hospital admissions and discharges", subtitle = "Care home residents flagged in the MPI
         \nSeven-day moving averages, indexed to the average of the same period in 2015-2019")) %>% 
  ggsave("../results_sprint3/Chr_admissions_discharges.png",., device = "png")









