####
### Profile admission causes during March - May
####


library(tidyverse)
library(tidylog)
library(janitor)
library(lubridate) 
library(RColorBrewer)

source("file_paths.R")
source("functions.R")


# Import data -------------------------------------------------------------
#hospital spells
chres_apcs <- readRDS(str_c(Rds_data_path, 'sprint_4/apcs_chr_cohorts.Rds'))

# Filter admissions for March to June ------------------------------------------------

chres_apcs <- chres_apcs %>% 
  filter(month(spellstartdate) %in% c(3,4,5))


# Summarise avoidable causes ----------------------------------------

summary_avoidable <- chres_apcs %>%  
  filter(!is.na(avoidable_cause)) %>% 
  group_by(ch_nursing, year, avoidable_cause) %>% 
  count() %>%
  group_by(ch_nursing, year) %>% 
  mutate(pct = round(100 * n / sum(n), 1))

write_csv(summary_avoidable, str_c(results_path_sprint4, "adm_causes/Avoidable_causes.csv"))

summary_avoidable %>%  group_by(ch_nursing, year) %>%  summarise(sum_pct = sum(pct, na.rm = TRUE))

summary_avoidable_comparison <-  summary_avoidable %>%
  select(-n) %>% 
  pivot_wider(names_from = "year", values_from = "pct", names_prefix = "avoidable_") %>% 
  mutate(pct_change = round(avoidable_2020-avoidable_2019, 2))

write_csv(summary_avoidable_comparison, str_c(results_path_sprint4, "adm_causes/Avoidable_causes_comparison.csv"))

summary_avoidable_count_wide <- summary_avoidable %>% 
  pivot_wider(c(- pct), 
              names_from = "year", 
              values_from = "n", names_prefix = "Year_") %>% 
  mutate_at(vars("Year_2019", "Year_2020"), replace_na, 0) %>%
  mutate(change = Year_2020 - Year_2019,
         pctchange = round(100*Year_2020/Year_2019 - 100, 0),
         combined = str_c(change, " (", pctchange, ")"))

write_csv(summary_avoidable_count_wide, str_c(results_path_sprint4, "adm_causes/Avoidable_causes_chnursing_March-May_countwide.csv"))


(summary_avoidable %>% 
    mutate(chnursing_year = if_else(ch_nursing == 0, 
                                    str_c("Residential - ", year),
                                    str_c("Nursing - ", year)),
           avoidable_cause = gsub(" ", "\n", avoidable_cause)) %>% 
  ggplot(aes(x = avoidable_cause, y = pct, group = chnursing_year, fill = chnursing_year)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  ylab("Percent of avoidable admissions") +
  labs(title = "Causes of avoidable hospital admissions from care homes",
       subtitle = "March - May") +
  theme_bw() +
  theme(axis.title.x = element_blank(),
        panel.grid = element_blank(),
        legend.title = element_blank(),
        legend.position = "top",
        legend.justification = c(1,0),) +
    scale_fill_manual(values = brewer.pal(name ="RdBu", n = 4)[c(2,1,3,4)])) %>% 
  ggsave(str_c(results_path_sprint4, "adm_causes/Avoidable_causes_pct.png"), ., 
         device = "png", dpi = 600, width = 7)

(summary_avoidable %>% 
    mutate(chnursing_year = if_else(ch_nursing == 0, 
                                    str_c("Residential - ", year),
                                    str_c("Nursing - ", year)),
           avoidable_cause = gsub(" ", "\n", avoidable_cause)) %>% 
    ggplot(aes(x = avoidable_cause, y = n, group = chnursing_year, fill =chnursing_year)) +
    geom_bar(stat = "identity", position = position_dodge()) +
    ylab("Admissions") +
    labs(title = "Causes of avoidable hospital admissions from care homes",
         subtitle = "March - May") +
    theme_bw() +
    theme(axis.title.x = element_blank(),
          panel.grid = element_blank(),
          legend.title = element_blank(),
          legend.position = "top",
          legend.justification = c(1,0),) +
    scale_fill_manual(values = brewer.pal(name ="RdBu", n = 4)[c(2,1,3,4)])) %>% 
  ggsave(str_c(results_path_sprint4, "adm_causes/Avoidable_causes_n.png"), ., 
         device = "png", dpi = 600, width = 7)

(summary_avoidable_count_wide %>% 
    ungroup() %>% 
    mutate(ch_nursing = if_else(ch_nursing == 0,"Residential", "Nursing"),
                                    avoidable_cause = gsub(" ", "\n", avoidable_cause)) %>% 
    ggplot(aes(x = avoidable_cause, y = pctchange, group = ch_nursing, fill = factor(ch_nursing))) +
    geom_bar(stat = "identity", position = position_dodge()) +
    ylab("% change relative to 2019") +
    labs(title = "Causes of avoidable hospital admissions from care homes",
         subtitle = "March - May") +
    theme_bw() +
    theme(axis.title.x = element_blank(),
          panel.grid = element_blank(),
          legend.title = element_blank(),
          legend.position = "top",
          legend.justification = c(1,0),) +
    scale_fill_manual(values = brewer.pal(name ="RdBu", n = 4)[c(1,4)])) %>% 
  ggsave(str_c(results_path_sprint4, "adm_causes/Avoidable_causes_pctchange2019.png"), ., 
         device = "png", dpi = 600, width = 7)


condition_order <- summary_avoidable_count_wide$avoidable_cause[summary_avoidable_count_wide$ch_nursing == 0]
condition_order <- condition_order[order(summary_avoidable_count_wide$Year_2019[summary_avoidable_count_wide$ch_nursing == 0])] %>% 
  str_to_sentence() %>% gsub(" ", "\n", .)  %>% gsub("lrti", "LRTI", .) %>% gsub("Uti", "Urinary tract\ninfections",.) %>% 
  gsub("Food\nand\n", "Food and ",.) %>% gsub("Pneunomia", "Pneumonia", .)

summary_avoidable_plot <- summary_avoidable %>% 
  mutate(chnursing_year = if_else(ch_nursing == 0, 
                                  str_c("Residential - ", year),
                                  str_c("Nursing - ", year)),
         chnursing_year = factor(chnursing_year, levels = c("Nursing - 2020", "Nursing - 2019", "Residential - 2020", "Residential - 2019")),
         avoidable_cause = str_to_sentence(avoidable_cause),
         avoidable_cause = gsub(" ", "\n", avoidable_cause),
         avoidable_cause = gsub("lrti", "LRTI", avoidable_cause),
         avoidable_cause = gsub("Pneunomia", "Pneumonia", avoidable_cause),
         avoidable_cause = gsub("Uti", "Urinary tract\ninfections", avoidable_cause),
         avoidable_cause = gsub("Food\nand\n", "Food and ", avoidable_cause),
         avoidable_cause = factor(avoidable_cause, levels = condition_order))

summary_avoidable_count_wide_plot <- summary_avoidable_count_wide %>% 
  mutate(year = 2020,
         chnursing_year = if_else(ch_nursing == 0, 
                                  str_c("Residential - ", year),
                                  str_c("Nursing - ", year)),
         avoidable_cause = str_to_sentence(avoidable_cause),
         avoidable_cause = gsub(" ", "\n", avoidable_cause),
         avoidable_cause = gsub("lrti", "LRTI", avoidable_cause),
         avoidable_cause = gsub("Pneunomia", "Pneumonia", avoidable_cause),
         avoidable_cause = gsub("Uti", "Urinary tract\ninfections", avoidable_cause),
         avoidable_cause = gsub("Food\nand\n", "Food and ", avoidable_cause),
         combined = str_c(sprintf("%+d", change), " (", sprintf("%+d", pctchange), ")"),
         combined = gsub(")", "%)", combined)) %>%   
  mutate(avoidable_cause = factor(avoidable_cause, levels = condition_order),
         x = if_else(ch_nursing == 1, -0.35, 0.35),
         xmax = if_else(ch_nursing == 1, -0.1, 0.1))


(summary_avoidable_plot %>% 
    ggplot(aes(x = avoidable_cause, y = n, group = chnursing_year, fill = chnursing_year)) +
    geom_bar(stat = "identity", position = position_dodge()) +
    geom_text(data = summary_avoidable_count_wide_plot, 
              aes(x = avoidable_cause, y = Year_2019 + 220,
                  label = combined), color = "black", 
              position = position_dodge(width = 1), show.legend = FALSE, size = 3) +
    geom_segment(data = summary_avoidable_count_wide_plot,
                 aes(x = as.numeric(avoidable_cause) +x, xend = as.numeric(avoidable_cause)+xmax, y = Year_2019 + 45, yend = Year_2019 + 45),
                 color = "black", size = 0.25, position = "dodge") +
    geom_segment(data = summary_avoidable_count_wide_plot,
                 aes(x = as.numeric(avoidable_cause) +x, xend = as.numeric(avoidable_cause)+x, y = Year_2019 + 25, yend = Year_2019 + 45),
                 color = "black", size = 0.25, position = "dodge") + 
    geom_segment(data = summary_avoidable_count_wide_plot,
                 aes(x = as.numeric(avoidable_cause) +xmax, xend = as.numeric(avoidable_cause)+xmax, y = Year_2019 + 25, yend = Year_2019 + 45),
                 color = "black", size = 0.25, position = "dodge") + 
    coord_flip()+
    ylab("Admissions") +
    labs(title = "Causes of avoidable hospital admissions from care homes",
         subtitle = "March - May") +
    theme_bw() +
    ylim(0,2800) +
    theme(axis.title.y = element_blank(),
          panel.grid = element_blank(),
          legend.title = element_blank(),
          legend.position = "top",
          legend.justification = c(1,0),) +
    scale_fill_manual(values = brewer.pal(name ="RdBu", n = 4)[c(1,2,4,3)], guide = guide_legend(reverse = TRUE)))%>% 
  ggsave("M:/COVID-19/COVID19_care_homes/graphs/sprint_4/Avoidable_causes_long.png", ., 
         device = "png", dpi = 600, height = 9, width = 7)

# Summary: Covid admissions -----------------------------------------------

# % covid admissions relative to all emergencies or electives (ordinary or day case)
summary_covid <- chres_apcs %>%
  filter(emergency == 1 | elod == 1) %>% 
  group_by(ch_nursing, year, emergency_covid) %>% 
  count() %>%
  group_by(ch_nursing, year) %>% 
  mutate(pct = round(100 * n / sum(n), 1))

write_csv(summary_covid, str_c(results_path_sprint4, "adm_causes/Covid_admissions.csv"))

# Covid admissions - other diagnosis codes --------------------------------

chres_apcs_covid_ids <- chres_apcs %>%
  filter(emergency == 1 | elod == 1) %>%  
  filter(emergency_covid == 1) %>% 
  distinct(spellid, ch_nursing, year)

apce_diag <- fread(str_c(raw_data_path, "care home paper/apcs_2019_20_diag_codes.csv"))

apce_diag_covid_adm <- apce_diag %>% 
  filter(spellid %in% chres_apcs_covid_ids$spellid)

apce_diag_covid_adm <- apce_diag_covid_adm %>%   
  filter(dc != "U071" & dc != "U072" & !is.na(dc) & dc != "") 


apce_diag_covid_adm <-  apce_diag_covid_adm %>% 
  left_join(chres_apcs_covid_ids)

# diagnosis for all spells that *have* additional diagnoses
apce_diag_covid_adm <-  apce_diag_covid_adm %>% 
  distinct()

# Number of additional diagnosis codes for COVID admissions
# per spell
# IMPORTANT: join back onto IDs of all COVID admissions, otherwise
# we lose admissions with 0 additional diagnosis codes
apce_diag_covid_adm %>% 
  group_by(ch_nursing, spellid) %>% 
  count(name = "diag_count") %>% 
  right_join(chres_apcs_covid_ids[, c("spellid")]) %>% 
  mutate(diag_count = replace_na(diag_count, 0)) %>% 
  group_by(ch_nursing) %>% 
  count(diag_count, name = "adm_count") %>% 
  ggplot(aes(x = diag_count, y= adm_count)) +
  geom_bar(stat = "identity") +
  facet_wrap("ch_nursing") +
  xlim(0,100)

adm_diag_count_summary <- apce_diag_covid_adm %>% 
  group_by(ch_nursing, spellid) %>% 
  count(name = "diag_count") %>% 
  right_join(chres_apcs_covid_ids[, c("spellid", "ch_nursing")]) %>% 
  mutate(diag_count = replace_na(diag_count, 0)) %>% 
  mutate(diag_count_cat = cut(diag_count, breaks = c(seq(0, 60, by = 10), Inf),
                              labels = c(str_c(seq(0, 50, by = 10), "-", 
                                                 seq(9, 59, by = 10)), "60+"),
                              right = FALSE)) %>%  
  group_by(ch_nursing, diag_count_cat) %>% 
  count(name = "adm_count") %>% 
  group_by(ch_nursing) %>% 
  mutate(total_adm =  sum(adm_count),
         adm_pct = round(100* adm_count/total_adm, 1))

write_csv(adm_diag_count_summary, str_c(results_path_sprint4, "adm_causes/COVID_num_add_diags.csv"))

adm_diag_count_summary %>% 
  mutate(ch_nursing = if_else(ch_nursing == 0,"Residential", "Nursing"))  %>% 
  ggplot(aes(x = diag_count_cat, y= adm_count)) +
  geom_bar(stat = "identity") +
  facet_wrap("ch_nursing") +
  geom_text(aes(x = diag_count_cat, y = adm_count + 30, label = str_c(adm_pct, "%")),
            size = 3) + 
  ylab("COVID-19 admissions") +
  xlab("Unique additional diagnosis codes")+
  theme_bw() +
  theme(panel.grid = element_blank(),
        legend.title = element_blank(),
        legend.position = "top",
        legend.justification = c(1,0),) 


ggsave(str_c(results_path_sprint4, "adm_causes/COVID_num_add_diags.png"), 
         device = "png", dpi = 600, width = 7)

# By diagnosis chapter
apce_diag_covid_adm <-  apce_diag_covid_adm %>% 
  categorise_ICD10(code = "dc", new_col =  "MainICD10Cat")

adm_diag_chapter_summary <- apce_diag_covid_adm %>% 
  distinct(ch_nursing, spellid, MainICD10Cat) %>%
  group_by(ch_nursing, MainICD10Cat) %>% 
  count(name = "adm_count") %>% 
  left_join(chres_apcs_covid_ids %>%  
              group_by(ch_nursing) %>%  
              count(name = "total_adm"), by = "ch_nursing") %>% 
  mutate(adm_pct = round(100* adm_count/total_adm, 1))

MainICD10Cat_order <- adm_diag_chapter_summary %>% 
  filter(ch_nursing == 1) %>% 
  arrange(adm_count) %>% 
  pull(MainICD10Cat)

adm_diag_chapter_summary <- adm_diag_chapter_summary %>% 
  mutate(MainICD10Cat = factor(MainICD10Cat, levels = paste0(MainICD10Cat_order)))

write_csv(adm_diag_chapter_summary, str_c(results_path_sprint4, "adm_causes/COVID_num_ICD10cat.csv"))

adm_diag_chapter_summary %>%
  filter(MainICD10Cat != 15) %>% 
  mutate(ch_nursing = if_else(ch_nursing == 0,"Residential", "Nursing"),
         ch_nursing = factor(ch_nursing, c("Residential", "Nursing")))  %>% 
  ggplot(aes(x = MainICD10Cat, y= adm_count)) +
  geom_bar(stat = "identity") +
  facet_wrap("ch_nursing", ncol = 2) +
  geom_text(aes(x = MainICD10Cat, y = adm_count + 120, label = str_c(adm_pct, "%")),
            size = 2) + 
  ylab("Number of COVID-19 admissions") +
  xlab("ICD10 chapter")+
  theme_bw() +
  theme(panel.grid = element_blank(),
        legend.title = element_blank(),
        legend.position = "top",
        legend.justification = c(1,0),) +
  coord_flip()


ggsave(str_c(results_path_sprint4, "adm_causes/COVID_num_ICD10cat.png"), 
       device = "png", dpi = 600, width = 8, height = 5)

# Stroke and acute coronary syndrome

apce_diag_covid_adm <-  apce_diag_covid_adm %>% 
 mutate(stroke_code = if_else(detect_codes(dc, "I6[1|3|4].*"), 1, 0),
        accorosyndr_code = if_else(detect_codes(dc, c("I200", "I21[0|1|2|3|4|9]", 
                                                     "I22[0|1|2|8|9]", "I24[8|9]")), 1, 0))

apce_diag_covid_adm  %>% 
  distinct(ch_nursing, spellid, stroke_code, accorosyndr_code) %>%
  filter(!(stroke_code == 0 & accorosyndr_code == 0)) %>% 
  group_by(ch_nursing, stroke_code, accorosyndr_code) %>% 
  count(name = "adm_count") %>% 
  left_join(chres_apcs_covid_ids %>%  
              group_by(ch_nursing) %>%  
              count(name = "total_adm"), by = "ch_nursing") %>% 
  mutate(adm_pct = round(100* adm_count/total_adm, 1)) %>% 
  write_csv( str_c(results_path_sprint4, "adm_causes/COVID_stroke_accorosyndr.csv"))


# Summary causes of elective admissions -----------------------------------

apc_electives <- chres_apcs %>% 
  filter(elod == 1) 

apc_electives %>%  tabyl(MainICD10Cat)
  
summary_electives <-  apc_electives %>% 
  group_by(ch_nursing, year, MainICD10Cat) %>% 
  count() %>% 
  group_by(MainICD10Cat) %>% 
  mutate(n_min = min(n),
         MainICD10Cat_temp = as.character(MainICD10Cat),
         MainICD10Cat_temp = replace_na(MainICD10Cat_temp, "Unknown"),
         MainICD10Cat_temp = ifelse(n_min < 10 & MainICD10Cat_temp != "Unknown", "Other", MainICD10Cat_temp)) %>%
  group_by(ch_nursing, year, MainICD10Cat_temp) %>% 
  summarise(n = sum(n, na.rm = TRUE)) %>% 
  group_by(ch_nursing, year) %>% 
  mutate(pct = round(100 * n / sum(n), 1),
         MainICD10Cat = MainICD10Cat_temp) %>% 
  select(-MainICD10Cat_temp)

summary_electives %>%  group_by(ch_nursing, year) %>%  summarise(sum_pct = sum(pct, na.rm = TRUE))


write_csv(summary_electives, str_c(results_path_sprint4, "adm_causes/Elective_causes.csv"))

summary_electives_comparison <-  summary_electives %>%
  select(-n) %>% 
  pivot_wider(names_from = "year", values_from = "pct", names_prefix = "electives_") %>% 
  mutate(pct_change = round(electives_2020-electives_2019, 2))

write_csv(summary_electives_comparison, str_c(results_path_sprint4, "adm_causes/Elective_causes_comparison.csv"))

summary_electives_count_wide <- summary_electives %>% 
  pivot_wider(c(- pct), 
              names_from = "year", 
              values_from = "n", names_prefix = "Year_") %>% 
  mutate_at(vars("Year_2019", "Year_2020"), replace_na, 0) %>%
  mutate(change = Year_2020 - Year_2019,
         pctchange = round(100*Year_2020/Year_2019 - 100, 0),
         combined = str_c(change, " (", pctchange, ")"))

write_csv(summary_electives_count_wide, str_c(results_path_sprint4, "adm_causes/Elective_causes_chnursing_March-May_countwide.csv"))



(summary_electives %>% 
    mutate(chnursing_year = if_else(ch_nursing == 0, 
                                    str_c("Residential - ", year),
                                    str_c("Nursing - ", year))) %>% 
    ggplot(aes(x = MainICD10Cat, y = pct, group = chnursing_year, fill = chnursing_year)) +
    geom_bar(stat = "identity", position = position_dodge()) +
    ylab("Percent of elective admissions") +
    labs(title = "Causes of elective hospital admissions from care homes",
         subtitle = "March - May") +
    theme_bw() +
    theme(axis.title.x = element_blank(),
          panel.grid = element_blank(),
          legend.title = element_blank(),
          legend.position = "top",
          legend.justification = c(1,0),) +
    scale_fill_manual(values = brewer.pal(name ="RdBu", n = 4)[c(2,1,3,4)])) %>% 
  ggsave(str_c(results_path_sprint4, "adm_causes/Elective_causes_pct.png"), ., 
         device = "png", dpi = 600, width = 7)

(summary_electives %>% 
    mutate(chnursing_year = if_else(ch_nursing == 0, 
                                    str_c("Residential - ", year),
                                    str_c("Nursing - ", year))) %>% 
    ggplot(aes(x = MainICD10Cat, y = n, group = chnursing_year, fill =chnursing_year)) +
    geom_bar(stat = "identity", position = position_dodge()) +
    ylab("Admissions") +
    labs(title = "Causes of elective hospital admissions from care homes",
         subtitle = "March - May") +
    theme_bw() +
    theme(axis.title.x = element_blank(),
          panel.grid = element_blank(),
          legend.title = element_blank(),
          legend.position = "top",
          legend.justification = c(1,0),) +
    scale_fill_manual(values = brewer.pal(name ="RdBu", n = 4)[c(2,1,3,4)])) %>% 
  ggsave(str_c(results_path_sprint4, "adm_causes/Elective_causes_n.png"), ., 
         device = "png", dpi = 600, width = 7)

(summary_electives_count_wide %>% 
    ungroup() %>% 
    mutate(ch_nursing = if_else(ch_nursing == 0,"Residential", "Nursing")) %>% 
    ggplot(aes(x = MainICD10Cat, y = pctchange, group = ch_nursing, fill = factor(ch_nursing))) +
    geom_bar(stat = "identity", position = position_dodge()) +
    ylab("% change relative to 2019") +
    labs(title = "Causes of elective hospital admissions from care homes",
         subtitle = "March - May") +
    theme_bw() +
    theme(axis.title.x = element_blank(),
          panel.grid = element_blank(),
          legend.title = element_blank(),
          legend.position = "top",
          legend.justification = c(1,0),) +
    scale_fill_manual(values = brewer.pal(name ="RdBu", n = 4)[c(1,4)])) %>% 
  ggsave(str_c(results_path_sprint4, "adm_causes/Elective_causes_pctchange2019.png"), ., 
         device = "png", dpi = 600, width = 7)



# Summary causes of emergency admissions -----------------------------------

apc_emergencies <- chres_apcs %>% 
  filter(emergency == 1)

apc_emergencies %>%  tabyl(MainICD10Cat)

summary_emergencies <-  apc_emergencies %>% 
  group_by(ch_nursing, year, MainICD10Cat) %>% 
  count() %>% 
  group_by(MainICD10Cat) %>% 
  mutate(n_min = min(n),
         MainICD10Cat_temp = as.character(MainICD10Cat),
         MainICD10Cat_temp = replace_na(MainICD10Cat_temp, "Unknown"),
         MainICD10Cat_temp = ifelse(n_min < 10 & MainICD10Cat_temp != "Unknown", "Other", MainICD10Cat_temp)) %>%
  group_by(ch_nursing, year, MainICD10Cat_temp) %>% 
  summarise(n = sum(n, na.rm = TRUE)) %>% 
  group_by(ch_nursing, year) %>% 
  mutate(pct = if_else(MainICD10Cat_temp != "22", round(100 * n / sum(n[MainICD10Cat_temp != "22"]), 1), NA_real_),
         MainICD10Cat = MainICD10Cat_temp)%>% 
  select(-MainICD10Cat_temp)

summary_emergencies %>%  group_by(ch_nursing, year) %>%  summarise(sum_pct = sum(pct, na.rm = TRUE))


write_csv(summary_emergencies, str_c(results_path_sprint4, "adm_causes/Emergencies_causes.csv"))

summary_emergencies %>%  group_by(ch_nursing, year) %>%  summarise(sum_pct = sum(pct, na.rm = TRUE))

# pct change excludes chapter 22
summary_emergencies_comparison <-  summary_emergencies %>%
  filter(!is.na(MainICD10Cat)) %>% 
  select(-n,) %>% 
  pivot_wider(names_from = "year", values_from = "pct", names_prefix = "electives_") %>% 
  mutate(pct_change = round(electives_2020-electives_2019,2))

write_csv(summary_emergencies_comparison, str_c(results_path_sprint4, "adm_causes/Emergencies_causes_comparison.csv"))



summary_emergencies_count_wide <- summary_emergencies %>% 
  pivot_wider(c(- pct), 
              names_from = "year", 
              values_from = "n", names_prefix = "Year_") %>% 
  mutate_at(vars("Year_2019", "Year_2020"), replace_na, 0) %>%
  mutate(change = Year_2020 - Year_2019,
         pctchange = round(100*Year_2020/Year_2019 - 100, 0),
         combined = str_c(change, " (", pctchange, ")"))

write_csv(summary_emergencies_count_wide, str_c(results_path_sprint4, "adm_causes/Emergencies_chnursing_March-May_countwide.csv"))




# Summary outcomes emergency admissions -----------------------------------

summary_emoutcome <- chres_apcs %>%  
  filter(emergency == 1) %>% 
  mutate(died_in_hospital = ifelse(!is.na(dischmthd) & dischmthd == 4, 1, 0)) %>% 
  group_by(ch_nursing, year, died_in_hospital) %>% 
  count() %>%
  group_by(ch_nursing, year) %>% 
  mutate(pct = round(100 * n / sum(n), 1))

write_csv(summary_emoutcome, str_c(results_path_sprint4, "adm_outcomes/Em_outcome.csv"))

summary_emoutcome_covid <- chres_apcs %>%  
  filter(emergency == 1) %>% 
  mutate(died_in_hospital = ifelse(!is.na(dischmthd) & dischmthd == 4, 1, 0),
         covid_prim = if_else(is.na(covid_prim), 0, covid_prim)) %>% 
  group_by(ch_nursing, year, covid_prim, died_in_hospital) %>% 
  count() %>%
  group_by(ch_nursing, year, covid_prim) %>% 
  mutate(pct = round(100 * n / sum(n), 1))

write_csv(summary_emoutcome_covid, str_c(results_path_sprint4, "adm_outcomes/Em_outcome_covid.csv"))

summary_emoutcome_covid_notprim <- chres_apcs %>%  
  filter(emergency == 1) %>% 
  mutate(died_in_hospital = ifelse(!is.na(dischmthd) & dischmthd == 4, 1, 0),
         covid_notprim = if_else(is.na(covid_notprim), 0, covid_notprim)) %>% 
  group_by(ch_nursing, year, covid_notprim, died_in_hospital) %>% 
  count() %>%
  group_by(ch_nursing, year, covid_notprim) %>% 
  mutate(pct = round(100 * n / sum(n), 1))

write_csv(summary_emoutcome_covid_notprim, str_c(results_path_sprint4, "adm_outcomes/Em_outcome_covid_notprim.csv"))

summary_emoutcome_covid_anydiag <- chres_apcs %>%  
  filter(emergency == 1) %>% 
  mutate(died_in_hospital = ifelse(!is.na(dischmthd) & dischmthd == 4, 1, 0),
         covid_anydiag = if_else(is.na(covid_anydiag), 0, covid_anydiag)) %>% 
  group_by(ch_nursing, year, covid_anydiag, died_in_hospital) %>% 
  count() %>%
  group_by(ch_nursing, year, covid_anydiag) %>% 
  mutate(pct = round(100 * n / sum(n), 1))

write_csv(summary_emoutcome_covid_anydiag, str_c(results_path_sprint4, "adm_outcomes/Em_outcome_covid_any.csv"))



# Statistics --------------------------------------------------------------

tabyl_all <- chres_apcs %>%  
  filter(emergency == 1) %>% 
  mutate(died_in_hospital = ifelse(!is.na(dischmthd) & dischmthd == 4, 1, 0)) %>% 
  tabyl(year, died_in_hospital, ch_nursing) 

chisq_all <-  map_dfr(tabyl_all, ~broom::tidy(janitor::chisq.test(., correct = FALSE)), .id = "ch_nursing") %>%
  mutate(group = "all emergency admissions")

write_csv(chisq_all, str_c(results_path_sprint4, "adm_outcomes/Em_outcome_all_chisquared.csv"))

tabyl_notcovid <- chres_apcs %>%  
  filter(emergency == 1 & (is.na(covid_anydiag) | covid_anydiag == 0)) %>% 
  mutate(died_in_hospital = ifelse(!is.na(dischmthd) & dischmthd == 4, 1, 0)) %>% 
  tabyl(year, died_in_hospital, ch_nursing) 

chisq_notcovid <-  map_dfr(tabyl_notcovid, ~broom::tidy(janitor::chisq.test(., correct = FALSE)), .id = "ch_nursing") %>%
  mutate(group = "not any COVID")

write_csv(chisq_notcovid, str_c(results_path_sprint4, "adm_outcomes/Em_outcome_notanycoviddiag_chisquared.csv"))
