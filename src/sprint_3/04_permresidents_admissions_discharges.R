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
chres <- readRDS(str_c(Rds_data_path, 'mmpiss_bymonth_chchars_2017-2020.Rds'))  %>% 
  mutate(chr_at_spellstart = 1,
         chr_at_spellend = 1)


# Hospital inpatient spells
chres_apcs <- read_csv(str_c(raw_data_path, "April 2020/chres_apcs.csv"), 
                       col_types = cols(pid = col_character(),
                                        spellstartdate =  col_date(format = "%d/%m/%Y"),
                                        spellenddate =  col_date(format = "%d/%m/%Y"),
                                        dischdate = col_date(format = "%d/%m/%Y")))

chres_apcs <- chres_apcs %>% 
  filter(spellstartdate >= ymd("2017-01-01"))

saveRDS(chres_apcs, str_c(Rds_data_path, 'chres_apcs_2017-2020.Rds'))
chres_apcs <- readRDS(str_c(Rds_data_path, 'chres_apcs_2017-2020.Rds'))


# Match with care home resident characteristics ---------------------------

#chres_apcs <- chres_apcs %>% 
#  mutate(spellstartdate_floor = floor_date(spellstartdate, "month"),
#         spellenddate_floor = floor_date(spellenddate, "month"))

# Flagging who was a resident when admitted

chres_apcs <- as.data.table(chres_apcs)
chres_apcs[, spellstartdate2 := spellstartdate]
chres_apcs[, spellenddate2 := spellenddate]

chres_dt <-  chres %>% 
  ungroup() %>% 
  select(pid, assigned_tnrchid, ch_nursing, ch_residential, ch_beds_filled, ch_is_open,
         datefrom, dateto, chr_at_spellstart, chr_at_spellend) %>% 
  as.data.table()
setkey(chres_dt, pid, datefrom, dateto)

chres_apcs <- foverlaps(chres_apcs, chres_dt, 
                        by.x = c("pid", "spellstartdate", "spellstartdate2"),
                        by.y = c("pid", "datefrom", "dateto"))

chres_apcs <- chres_apcs %>% 
  rename(datefrom_ch_spellstart = "datefrom", dateto_ch_spellstart = "dateto",
         assigned_tnrchid_spellstart = "assigned_tnrchid", ch_nursing_spellstart = "ch_nursing", 
         ch_residential_spellstart = "ch_residential", ch_beds_filled_spellstart = "ch_beds_filled", 
         ch_is_open_spellstart = "ch_is_open")


chres_apcs <- foverlaps(chres_apcs, chres_dt, 
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


saveRDS(chres_apcs, str_c(Rds_data_path, 'chres_apcs_2017-2020_mmpiss_matched.Rds'))
chres_apcs <- readRDS(str_c(Rds_data_path, 'chres_apcs_2017-2020_mmpiss_matched.Rds'))


#Counting admissions -----------------------------------------------------


chr_admissions <- chres_apcs %>% 
  filter(chr_at_spellstart == 1 & admincat != 2) %>% 
  group_by(spellstartdate) %>% 
  summarise(adm_all = n(),
            adm_elective = sum(elective == 1),
            adm_emergency = sum(emergency == 1)) %>% 
  mutate(adm_all_7dayavg = sevendayavg(adm_all),
         adm_elective_7dayavg = sevendayavg(adm_elective),
         adm_emergency_7dayavg = sevendayavg(adm_emergency),
         year = year(spellstartdate),
         day_dummy = `year<-`(spellstartdate, 0001))

write_csv(chr_admissions, "../results_sprint3/Chr_admissions.csv")


(chr_admissions %>% 
    ggplot(aes(x = day_dummy, y = adm_all_7dayavg, group = factor(year),
               color = factor(year))) +
    scale_x_date(date_labels = "%b", date_breaks = "1 month") +
    geom_line() +
    geom_point(size = 1) +
    theme_bw() +
    theme(panel.grid.minor.x = element_line(),
          axis.title.x =  element_blank())+
    ylab("all hospital admissions (7-day rolling average)") +
    labs(title = "Hospital admissions", subtitle = "Care home residents flagged in the MPI")) %>% 
  ggsave("../results_sprint3/Chr_admissions.png",., device = "png")


chr_admissions_nursing <- chres_apcs %>% 
  filter(chr_at_spellstart == 1 & admincat != 2) %>% 
  group_by(spellstartdate, ch_nursing_spellstart) %>% 
  summarise(adm_all = n(),
            adm_elective = sum(elective == 1),
            adm_emergency = sum(emergency == 1)) %>% 
  group_by(ch_nursing_spellstart) %>% 
  mutate(adm_all_7dayavg = sevendayavg(adm_all),
         adm_elective_7dayavg = sevendayavg(adm_elective),
         adm_emergency_7dayavg = sevendayavg(adm_emergency),
         year = year(spellstartdate),
         day_dummy = `year<-`(spellstartdate, 0001))

write_csv(chr_admissions_nursing, "../results_sprint3/Chr_admissions_chnursing.csv")


(chr_admissions_nursing %>%
 filter(!is.na(ch_nursing_spellstart)) %>% 
  ggplot(aes(x = day_dummy, y = adm_all_7dayavg, group = factor(year),
             color = factor(year))) +
  facet_grid(ch_nursing_spellstart ~., labeller = as_labeller(c(`0` = "Residential home",
                                                           `1` = "Nursing home"))) +
  scale_x_date(date_labels = "%b", date_breaks = "1 month") +
  geom_line() +
  geom_point(size = 1) +
  theme_bw() +
  theme(panel.grid.minor.x = element_line(),
        axis.title.x =  element_blank())+
  ylab("all hospital admissions (7-day rolling average)") +
  labs(title = "Hospital admissions", subtitle = "Care home residents flagged in the MPI")) %>% 
  ggsave("../results_sprint3/Chr_admissions_chnursing.png",., device = "png")

# Counting discharges -----------------------------------------------------

chr_discharges <- chres_apcs %>% 
  filter(chr_at_spellend == 1 & !is.element(dischdest, c(79, 98, 51, 52, 53, 30, 37, 38, 48, 49, 50, 66, 84, 87)) 
         & admincat != 2) %>% 
  group_by(spellenddate) %>% 
  summarise(disch = n()) %>% 
  mutate(disch_7dayavg = sevendayavg(disch),
         year = year(spellenddate),
         day_dummy = `year<-`(spellenddate, 0001))

write_csv(chr_discharges, "../results_sprint3/Chr_discharges.csv")


(chr_discharges %>% 
    ggplot(aes(x = day_dummy, y = disch_7dayavg, group = factor(year),
               color = factor(year))) +
    scale_x_date(date_labels = "%b", date_breaks = "1 month") +
    geom_line() +
    geom_point(size = 1) +
    theme_bw() +
    theme(panel.grid.minor.x = element_line(),
          axis.title.x =  element_blank())+
    ylab("all hospital discharges (7-day rolling average)") +
    labs(title = "Hospital discharges", subtitle = "Care home residents flagged in the MPI")) %>% 
  ggsave("../results_sprint3/Chr_discharges.png",., device = "png")


chr_discharges_nursing <- chres_apcs %>% 
  filter(chr_at_spellend == 1 & !is.element(dischdest, c(79, 98, 51, 52, 53, 30, 37, 38, 48, 49, 50, 66, 84, 87)) & dischmthd != 4) %>% 
  group_by(spellenddate, ch_nursing_spellend) %>% 
  summarise(disch = n()) %>% 
  group_by(ch_nursing_spellend) %>% 
  mutate(disch_7dayavg = sevendayavg(disch),
         year = year(spellenddate),
         day_dummy = `year<-`(spellenddate, 0001))

write_csv(chr_discharges_nursing, "../results_sprint3/Chr_discharges_chnursing.csv")


(chr_discharges_nursing %>% 
    filter(!is.na(ch_nursing_spellend)) %>% 
    ggplot(aes(x = day_dummy, y = disch_7dayavg, group = factor(year),
               color = factor(year))) +
    facet_grid(ch_nursing_spellend ~., labeller = as_labeller(c(`0` = "Residential home",
                                                                  `1` = "Nursing home"))) +
    scale_x_date(date_labels = "%b", date_breaks = "1 month") +
    geom_line() +
    geom_point(size = 1) +
    theme_bw() +
    theme(panel.grid.minor.x = element_line(),
          axis.title.x =  element_blank())+
    ylab("all hospital discharges (7-day rolling average)") +
    labs(title = "Hospital discharges", subtitle = "Care home residents flagged in the MPI")) %>% 
  ggsave("../results_sprint3/Chr_discharges_chnursing.png",., device = "png")


# Admissions vs discharges ------------------------------------------------

combined_nursing <- chr_admissions_nursing %>% 
  left_join(chr_discharges_nursing, by = c("spellstartdate" = "spellenddate", "ch_nursing_spellstart" =
                                             "ch_nursing_spellend"))

(combined_nursing %>% 
  select(spellstartdate, ch_nursing_spellstart, adm_all_7dayavg, disch_7dayavg) %>% 
  filter(!is.na(ch_nursing_spellstart)) %>% 
  pivot_longer(c(-spellstartdate, -ch_nursing_spellstart), names_to = "type", values_to = "value") %>% 
  mutate(year = year(spellstartdate),
         day_dummy = `year<-`(spellstartdate, 0001)) %>% 
  ggplot(aes(x = day_dummy, y = value, group = factor(type),
             color = factor(type))) +
  facet_grid(ch_nursing_spellstart ~ year, labeller = as_labeller(c(`0` = "Residential home",
                                                              `1` = "Nursing home",
                                                              `2017` = "2017",
                                                              `2018` = "2018",
                                                              `2019` = "2019",
                                                              `2020` = "2020"))) +
  scale_x_date(date_labels = "%b", date_breaks = "1 month") +
  geom_line() +
  geom_point(size = 1) +
  theme_bw() +
  theme(panel.grid.minor.x = element_line(),
        axis.title.x =  element_blank())+
  ylab("all hospital discharges (7-day rolling average)") +
  labs(title = "Hospital discharges", subtitle = "Care home residents flagged in the MPI")) %>% 
  ggsave("../results_sprint3/Chr_admissions_discharges_chnursing.png",., device = "png", width = 10)
         



combined <- chr_admissions %>% 
  left_join(chr_discharges, by = c("spellstartdate" = "spellenddate"))

(combined %>% 
    select(spellstartdate, adm_all_7dayavg, disch_7dayavg) %>% 
    mutate(year = year(spellstartdate),
           day_dummy = `year<-`(spellstartdate, 0001)) %>% 
    ggplot(aes(x = day_dummy, y = disch_7dayavg/adm_all_7dayavg, group = factor(year),
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

