####
# Care home residents - descriptives
####

library(tidyverse)
library(tidylog)
library(janitor)
library(lubridate)

source("file_paths.R")


# Lookup files ------------------------------------------------------------

lsoa_to_region <- read_csv("../reference_data/lsoa11_reg20_lu.csv")
lsoa_to_imd <- read_csv("../reference_data/IMD_2019_LSOA.csv")%>%
  select(starts_with('LSOA code'),
         contains('Index of Multiple Deprivation (IMD) Rank'),
         contains('Index of Multiple Deprivation (IMD) Decile')) 
colnames(lsoa_to_imd) <- c("LSOA", "IMD_rank", "IMD_decile")


# Import data -------------------------------------------------------------

# Residents and care home characteristics
chres <- readRDS(str_c(Rds_data_path, 'mmpiss_bymonth_chchars.Rds'))

# Long-term conditions
chres_LPTS <- readRDS(str_c(Rds_data_path, 'mmpiss_bymonth_LTPs.Rds')) %>% 
  filter(extdatefrom >= ymd("2017-01-01"))  %>% 
  distinct(pid, extdatefrom, .keep_all = TRUE)

# Care home chars
chchar_ts <- readRDS(str_c(Rds_data_path, 'pseudo_carehome_characteristics_mar20_timeseries.Rds'))


# Create additional vars --------------------------------------------------

chres <- chres %>% 
  mutate(age = interval(dob, extdatefrom) %/% years(1),
         extdatefrom_died = ifelse(!is.na(deathdate) & 
                                     floor_date(deathdate, "month") == extdatefrom, 1, 0)) %>% 
  left_join(lsoa_to_region %>% 
               select(lsoa11cd, RGN19CD, RGN19NM), by = c("lsoa11pidpcd" = "lsoa11cd")) 
# the mismatches are all rows where LSOA is NA

chres <- chres %>% 
  left_join(lsoa_to_imd, by = c("lsoa11pidpcd" = "LSOA"))


chres <- chres %>% 
  mutate(IMD_quintile = cut(IMD_decile, breaks = seq(0, 10, by = 2), labels = c(1:5)),
         IMD_quintile = fct_explicit_na(IMD_quintile, na_level = 'Missing'))

saveRDS(chres,  str_c(Rds_data_path, 'mmpiss_bymonth_chchars_region_IMD.Rds'))
write_csv(chres,  str_c(Rds_data_path, 'mmpiss_bymonth_chchars_region_IMD.csv'))


chres <- chres %>% 
  left_join(chres_LPTS %>% 
              select(uid, uid_mmpiss, pid, extdatefrom, extdateto, 
                     e_depression_h36, e_diabcomp_h36, e_diabuncomp_h36,
                     ec_chf_h36, ec_cpd_h36, c_dementia_h36, c_diabchrcomp_h36, c_renaldis_h36,
                     a_covid_h36, a_flupneumonia_h36, f_dementia_h36))
                     
# Resident population descriptives -----------------------------------------------

calc_percent <- function(col, denominator_col, digits){
  round(100*col/denominator_col, digits)
}

create_chr_descriptives <- function(data){
  
  data %>% 
    summarise(chr = n(),
              chr_deaths = sum(extdatefrom_died==1),
              chr_under18 = sum(age < 18),
              chr_under18_pct = calc_percent(chr_under18, chr, 1),
              chr_over65 = sum(age > 65),
              chr_over65_pct = calc_percent(chr_over65, chr, 1),
              
              chr_care_home_closed = sum(ch_is_open == 0),
              chr_care_home_closed_pct = calc_percent(chr_care_home_closed, chr, 1),
              
              chr_nursing = sum(ch_nursing == 1, na.rm = TRUE),
              chr_nursing_pct = calc_percent(chr_nursing, chr, 1),
              
              chr_age_mean = round(mean(age), 1),
              chr_female = sum(sex == 2),
              chr_female_pct = calc_percent(chr_female, chr, 1),
              
              chr_c_dementia_h36 = sum(c_dementia_h36 == 1),
              chr_c_dementia_h36_pct = calc_percent(chr_c_dementia_h36, chr, 1),
              
              chr_f_dementia_h36 = sum(f_dementia_h36 == 1),
              chr_f_dementia_h36_pct = calc_percent(chr_f_dementia_h36, chr, 1),
              
              chr_e_diabcomp_h36 = sum(e_diabcomp_h36 == 1),
              chr_e_diabcomp_h36_pct = calc_percent(chr_e_diabcomp_h36, chr, 1),
              
              chr_e_diabuncomp_h36 = sum(e_diabuncomp_h36 == 1),
              chr_e_diabuncomp_h36_pct = calc_percent(chr_e_diabuncomp_h36, chr, 1),
              
              chr_c_diabchrcomp_h36 = sum(c_diabchrcomp_h36 == 1),
              chr_c_diabchrcomp_h36_pct = calc_percent(chr_c_diabchrcomp_h36, chr, 1),
              
              chr_ec_cpd_h36 = sum(ec_cpd_h36 == 1),
              chr_ec_cpd_h36_pct = calc_percent(chr_ec_cpd_h36, chr, 1),
              
              chr_a_covid_h36 = sum(a_covid_h36 == 1),
              chr_a_covid_h36_pct = calc_percent(chr_a_covid_h36, chr, 1),
              
              chr_a_flupneumonia_h36 = sum(a_flupneumonia_h36 == 1),
              chr_a_flupneumonia_h36_pct = calc_percent(chr_a_flupneumonia_h36, chr, 1),
              
              chr_IMD_decile_1 = sum(IMD_quintile == 1),
              chr_IMD_decile_2 = sum(IMD_quintile == 2),
              chr_IMD_decile_3 = sum(IMD_quintile == 3),
              chr_IMD_decile_4 = sum(IMD_quintile == 4),
              chr_IMD_decile_5 = sum(IMD_quintile == 5),
              chr_IMD_decile_Missing = sum(IMD_quintile == "Missing"),
  
              chr_IMD_decile_1_pct = calc_percent(chr_IMD_decile_1, chr, 1),
              chr_IMD_decile_2_pct = calc_percent(chr_IMD_decile_2, chr, 1),
              chr_IMD_decile_3_pct = calc_percent(chr_IMD_decile_3, chr, 1),
              chr_IMD_decile_4_pct = calc_percent(chr_IMD_decile_4, chr, 1),
              chr_IMD_decile_5_pct = calc_percent(chr_IMD_decile_5, chr, 1),
              chr_IMD_decile_Missing_pct = calc_percent(chr_IMD_decile_Missing, chr, 1)
              )
              
}

# England-level descriptives
chr_descriptives <- chres %>% 
  group_by(extdatefrom)  %>% 
  create_chr_descriptives()

write_csv(chr_descriptives, "../results_sprint3/Chr_descriptives_England.csv")
            
# England-level descriptives by nursing home type        
chr_descriptives_nursing <- chres %>% 
  group_by(extdatefrom, ch_nursing) %>% 
  create_chr_descriptives()

write_csv(chr_descriptives_nursing, "../results_sprint3/Chr_descriptives_England_chnursing.csv")


# # Regional descriptives
chr_descriptives_region <- chres %>%
  group_by(extdatefrom, RGN19NM)  %>%
  create_chr_descriptives()

write_csv(chr_descriptives_region, "../results_sprint3/Chr_descriptives_region.csv")

# Regional descriptives by nursing home type
chr_descriptives_region_nursing <- chres %>%
  group_by(extdatefrom, RGN19NM, ch_nursing) %>%
  create_chr_descriptives()

write_csv(chr_descriptives_region_nursing, "../results_sprint3/Chr_descriptives_region_nursing.csv")

# By IMD quintiles
chr_descriptives_IMD <- chres %>%
  group_by(extdatefrom, IMD_quintile)  %>%
  create_chr_descriptives()

write_csv(chr_descriptives_IMD, "../results_sprint3/Chr_descriptives_IMDquintile.csv")

# Regional descriptives by nursing home type
chr_descriptives_IMD_nursing <- chres %>%
  group_by(extdatefrom, IMD_quintile, ch_nursing) %>%
  create_chr_descriptives()

write_csv(chr_descriptives_IMD_nursing, "../results_sprint3/Chr_descriptives_IMD_nursing.csv")



# Care home level descriptives -----------------------------------------------------

ch_occupancy <- chres %>% 
  group_by(extdatefrom, assigned_tnrchid) %>% 
  count() %>% 
  right_join(chchar_ts %>% 
               select(pseudo_ch_id, month, ch_nursing, ch_beds_filled) %>% 
               filter(month <= ymd("2020-04-01")), 
             by = c("assigned_tnrchid" = "pseudo_ch_id", "extdatefrom" = "month")) %>% 
  mutate(occupancy = calc_percent(n, ch_beds_filled, 1))

ch_occupancy_summary <- ch_occupancy %>% 
  group_by(extdatefrom, ch_nursing) %>% 
  summarise(mean_occupancy = round(mean(occupancy, na.rm = TRUE), 1),
            sd_occupancy = round(sd(occupancy, na.rm = TRUE), 1),
            n_ch = n()) %>% 
  filter(extdatefrom >= ymd("2017-01-01"))

write_csv(ch_occupancy_summary, "../results_sprint3/Ch_mean_occupancy_chnursing.csv")
            
(ch_occupancy_summary %>% 
  ggplot(aes(x = extdatefrom, y = mean_occupancy, 
             group = factor(ch_nursing), color = factor(ch_nursing))) +
  geom_point() +
  geom_line() +
  ylim(c(60,100)) +
  theme_bw() +
  ylab("Mean care home occupancy (%)") +
  labs(title = "Mean care home bed occupancy", 
       subtitle = "based bed capacity and the number of residents identified in the MPI")) %>% 
  ggsave("../results_sprint3/Ch_mean_occupancy_chnursing.png",., device = "png")

ch_occupancy_summary2 <- ch_occupancy %>% 
  filter(extdatefrom >= ymd("2017-01-01") & !is.na(occupancy)) %>% 
  mutate(occupancy_cut = cut(occupancy, breaks = c(0, 70, 85, Inf), labels = c("under 70%",
                                                                                    "70-85%",
                                                                                    "over 85%"),
                             right = TRUE, include.lowest = TRUE ))

ch_occupancy_summary2 <-  ch_occupancy_summary2 %>% 
  group_by(extdatefrom, ch_nursing, occupancy_cut) %>% 
  summarise(n_ch = n()) %>% 
  group_by(extdatefrom, ch_nursing) %>% 
  mutate(pct_ch = n_ch / sum(n_ch))  


# Number of care homes ----------------------------------------------------


care_homes <- chres %>%
  filter(!is.na(lsoa11pidpcd)) %>% 
  select(extdatefrom, assigned_tnrchid, RGN19NM, ch_nursing, ch_beds_filled) %>% 
  distinct() 
  
care_homes %>% 
  group_by(assigned_tnrchid, extdatefrom) %>% 
  count() %>% 
  filter(n>1)
  summarise(n_ch = )