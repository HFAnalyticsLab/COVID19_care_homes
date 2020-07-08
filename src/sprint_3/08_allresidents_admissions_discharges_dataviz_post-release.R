####
# Descriptive analysis of hospital admissions and discharges (all and from/to care homes)
####

library(tidyverse)
library(tidylog)
library(janitor)
library(lubridate)
library(readxl)
library(zoo)

source("src/functions.R")

# Plotting ------------------------------------------------------------------
THF_red <- '#dd0031'
THF_50pct_light_blue <- '#aad3e5'
THF_1_purple <- '#744284'
THF_dark_grey <-  '#524c48'
THF_light_grey <- '#e2dfd8'
THF_50pct_light_grey <- '#EEEDE8'

#### Admissions & discharges until end of April excluding deaths  -------------------------------------------------

# summary of the number of admissions and discharges in England by day
# discharges exclude deaths
# file created and released from the secure server by RW on 5.6.2020
discharges_april_excldeaths <- read_xlsx("data/sprint_3/2020_06_22/01a Hospital Admission and Discharge to Care Homes Exc Deaths - Summary - Jan to Apr.xlsx",
                              sheet = "Disch 1420 All Exclusions") 

admissions_april_excldeaths <- read_xlsx("data/sprint_3/2020_06_22/01a Hospital Admission and Discharge to Care Homes Exc Deaths - Summary - Jan to Apr.xlsx",
                                         sheet = "Adm 1420 All Exclusions") 

# Clean & derive ----

## Discharges
discharges_april_excldeaths <- discharges_april_excldeaths %>%
  pivot_longer(c(-dischday, -dischmnth), names_to = "type", values_to = "value") %>% 
  separate(type, into = c("type", "year"), sep = "_") %>% 
  group_by(type) %>% 
  filter(!is.na(value)) %>% 
  mutate(date = ymd(str_c(year, 
                          str_pad(dischmnth, 2, side = "left", pad = "0"), 
                          str_pad(dischday, 2, side = "left", pad = "0")))) %>% 
  filter(date > ymd("2014-08-01"))

discharges_april_excldeaths <- discharges_april_excldeaths %>%
  select(-dischmnth, -dischday) %>% 
  separate(type, into = c("type", "sourcedest"), sep = 5)

# calculate seven day rolling averages and create dummy day for grouping and plotting
discharges_april_excldeaths <- discharges_april_excldeaths %>% 
  arrange(date) %>% 
  group_by(sourcedest) %>%
  mutate(sevendayavg = sevendayavg(value),
         day_dummy  = `year<-`(date, 0004)) # Feb 29th only parses when 0004 is used as year

# percent relative to the daily average of 2015-2019
discharges_april_excldeaths <- discharges_april_excldeaths %>%
  group_by(type, sourcedest, day_dummy) %>%
  mutate(ref_2015_to_2019 = mean(sevendayavg[year %in% c(2015:2019)]),
         percent_2015_to_2019 = round(100*sevendayavg / ref_2015_to_2019, 1),
         ref_2019 = mean(sevendayavg[year == 2019]),
         percent_2019 = round(100*sevendayavg / ref_2019, 1)) 


## Admissions
admissions_april_excldeaths <- admissions_april_excldeaths %>%
  pivot_longer(c(-admday, -admmnth), names_to = "type", values_to = "value") %>% 
  separate(type, into = c("type", "year"), sep = "_") %>% 
  group_by(type) %>% 
  filter(!is.na(value)) %>% 
  mutate(date = ymd(str_c(year, 
                          str_pad(admmnth, 2, side = "left", pad = "0"), 
                          str_pad(admday, 2, side = "left", pad = "0")))) %>% 
  filter(date > ymd("2014-08-01"))


admissions_april_excldeaths <- admissions_april_excldeaths %>%
  select(-admmnth, -admday) %>% 
  separate(type, into = c("type", "sourcedest"), sep = 3)


# calculate seven day rolling averages and create dummy day for grouping and plotting
admissions_april_excldeaths <- admissions_april_excldeaths %>% 
  arrange(date) %>% 
  group_by(sourcedest) %>% 
   mutate(sevendayavg = sevendayavg(value),
         day_dummy  = `year<-`(date, 0004))


# percent relative to the daily average of 2015-2019
admissions_april_excldeaths <- admissions_april_excldeaths %>%
  group_by(type, sourcedest, day_dummy) %>%
  mutate(ref_2015_to_2019 = mean(sevendayavg[year %in% c(2015:2019)]),
         percent_2015_to_2019 = round(100*sevendayavg / ref_2015_to_2019, 1),
         ref_2019 = mean(sevendayavg[year == 2019]),
         percent_2019 = round(100*sevendayavg / ref_2019, 1)) 

# Admissions and discharges in a combined graph ----

combined_april_excldeaths <- admissions_april_excldeaths %>% 
  bind_rows(discharges_april_excldeaths) 

combined_april_excldeaths %>% 
  filter(day_dummy <= ymd("0004-04-30") & day_dummy >= ymd("0004-02-01") & sourcedest != "all" & year == "2020") %>% 
  ungroup() %>% 
  select(-ref_2015_to_2019, -value, -sevendayavg, -year, -day_dummy) %>% 
  unite("combined_col", type, sourcedest) %>% 
  pivot_wider(names_from = "combined_col", values_from = "percent_2015_to_2019") %>% 
  rename(pct_admissions_from_carehomes = "adm_ch", pct_admissions_from_other_sources = "adm_other",
         pct_discharges_to_carehomes = "disch_ch", pct_discharges_to_other_destinations = "disch_other") %>% 
  write_csv("graphs/sprint_3/Admissions_discharges_excldeaths.csv")


# Graph with absolute values
(combined_april_excldeaths %>% 
    filter(day_dummy <= ymd("0004-04-30") & day_dummy >= ymd("0004-02-01") & sourcedest != "all") %>% 
    ggplot(aes(x = day_dummy, y = sevendayavg, color = year, group = year)) +
    geom_vline(xintercept = ymd("0004-03-17"), color = THF_dark_grey, linetype = "dashed", alpha = 0.3) +
    geom_line() +
    geom_point(size = 0.5) +
    theme_bw() +
    facet_wrap(vars(type, sourcedest), scales = "free", 
               labeller = as_labeller(c("adm" = "Admissions",
                                        "disch" = "Discharges",
                                        "ch" = "from/to care homes",
                                        "other" = "from/to other dest."))) +
    ylab("7 day rolling average") +
    scale_x_date(date_labels = "%b", date_breaks = "1 month") +
    theme(axis.title.x = element_blank(),
          legend.position = "right",
          legend.justification = c(1,0),
          axis.text = element_text(colour = THF_dark_grey),
          axis.title = element_text(colour = THF_dark_grey),
          axis.ticks = element_line(colour = THF_light_grey),
          panel.grid = element_blank(),
          panel.grid.major.y = element_line(colour = THF_50pct_light_grey),
          legend.text = element_text(colour = THF_dark_grey),
          legend.title = element_blank(),
          plot.title = element_text(colour = "#005078"),
          plot.subtitle = element_text(colour = "#005078"),
          strip.text = element_text(colour = "#524c48"),
          strip.background = element_blank()) +
    expand_limits(y=0) +
    labs(title = "Hospital admissions and discharges")) %>% 
  ggsave("graphs/sprint_3/Admissions_discharges_excldeaths.png", ., width = 9, height = 6, dpi = 600)
  
# Graph with absolute values, showing 2015-2019 as mean
  
  combined_april_excldeaths_2020 <- combined_april_excldeaths %>% 
    filter(day_dummy <= ymd("0004-04-30") & day_dummy >= ymd("0004-02-01") & sourcedest != "all" & year == "2020") 
    
(combined_april_excldeaths %>% 
      filter(day_dummy <= ymd("0004-04-30") & day_dummy >= ymd("0004-02-01") & sourcedest != "all") %>% 
      ggplot(aes(x = day_dummy, y = ref_2015_to_2019)) +
      geom_vline(xintercept = ymd("0004-03-17"), color = THF_dark_grey, linetype = "dashed", alpha = 0.3) +
      geom_line() +
      geom_point(size = 0.5) +
      geom_line(data = combined_april_excldeaths_2020, 
                aes(x = day_dummy, y = sevendayavg),
                color = THF_red) +
      geom_point(data = combined_april_excldeaths_2020, 
                 aes(x = day_dummy, y = sevendayavg),
                 size = 0.5, color = THF_red) +
      theme_bw() +
      facet_wrap(vars(type, sourcedest), scales = "free", 
                 labeller = as_labeller(c("adm" = "Admissions",
                                          "disch" = "Discharges",
                                          "ch" = "from/to care homes",
                                          "other" = "from/to other dest."))) +
      ylab("7 day rolling average") +
      scale_x_date(date_labels = "%b", date_breaks = "1 month") +
      theme(axis.title.x = element_blank(),
            axis.text = element_text(colour = THF_dark_grey),
            axis.title = element_text(colour = THF_dark_grey),
            axis.ticks = element_line(colour = THF_light_grey),
            panel.grid = element_blank(),
            panel.grid.major.y = element_line(colour = THF_50pct_light_grey),
            plot.title = element_text(colour = "#005078"),
            plot.subtitle = element_text(colour = "#005078"),
            strip.text = element_text(colour = "#524c48"),
            strip.background = element_blank()) +
      expand_limits(y=0) +
      labs(title = "Hospital admissions and discharges",
           subtitle = "Black - mean 2015 to 2019, red - 2020")) %>% 
    ggsave("graphs/sprint_3/Admissions_discharges_excldeaths2.png", ., width = 9, height = 6, dpi = 600)
  

# Graph indexed to average 2015-2019
(combined_april_excldeaths  %>% 
    filter(day_dummy <= ymd("0004-04-30") & day_dummy >= ymd("0004-02-01") & sourcedest != "all" & year == "2020") %>% 
    ggplot(aes(x = day_dummy, y = percent_2015_to_2019, color = sourcedest, group = sourcedest)) +
    geom_vline(xintercept = ymd("0004-03-17"), color = THF_dark_grey, linetype = "dashed", alpha = 0.3) +
    annotate("rect", xmin = ymd("0004-04-10"), xmax = ymd("0004-04-13"), 
             ymin = -Inf, ymax = Inf, fill = THF_dark_grey, alpha = 0.3) +
    
    geom_line() +
    geom_point(size = 0.5) +
    theme_bw() +
    facet_grid(.~type, scale = "free_y", 
               labeller = as_labeller(c("adm" = "Admissions",
                                        "disch" = "Discharges"))) +
    ylab("Percent") +
    scale_x_date(date_labels = "%b", date_breaks = "1 month") +
    scale_color_manual(values = c(THF_red, THF_50pct_light_blue), labels = c("from/to care home", "other source/destination")) +
    
    scale_y_continuous(breaks = seq(40, 100, by = 20)) +
    theme(axis.title.x = element_blank(),
          legend.position = "top",
          legend.justification = c(1,0),
          axis.text = element_text(colour = THF_dark_grey),
          axis.title = element_text(colour = THF_dark_grey),
          axis.ticks = element_line(colour = THF_light_grey),
          panel.grid = element_blank(),
          panel.grid.major.y = element_line(colour = THF_50pct_light_grey),
          legend.text = element_text(colour = THF_dark_grey),
          legend.title = element_blank(),
          plot.title = element_text(colour = "#005078"),
          plot.subtitle = element_text(colour = "#005078"),
          strip.text = element_text(colour = "#524c48"),
          strip.background = element_blank()) +
    annotate("text", x = ymd("0004-03-18"), y = 118, 
             label = "17 March,\nNHS discharge\nguidance", color = THF_dark_grey, size = 2, hjust = 0) +
    annotate("text", x = ymd("0004-04-14"), y = 120, 
             label = "10-13 April,\nEaster 2020", color = THF_dark_grey, size = 2, hjust = 0) +
    labs(title = "Hospital admissions and discharges in 2020", 
         subtitle = "Seven day moving average,\nindexed to the average of the same period between 2015 and 2019",
         color = "Source/destination")) %>% 
  ggsave("graphs/sprint_3/Admissions_discharges_excldeaths_rel20215-2019.png", ., width = 7, height = 4, dpi = 600)


# Graph indexed to 2019 only
(combined_april_excldeaths  %>%
    filter(day_dummy <= ymd("0004-04-30") & day_dummy >= ymd("0004-02-01") & sourcedest != "all" & year == "2020") %>% 
    ggplot(aes(x = day_dummy, y = percent_2019, color = sourcedest, group = sourcedest)) +
    geom_vline(xintercept = ymd("0004-03-17"), color = THF_dark_grey, linetype = "dashed", alpha = 0.3) +
    annotate("rect", xmin = ymd("0004-04-10"), xmax = ymd("0004-04-13"), 
             ymin = -Inf, ymax = Inf, fill = THF_dark_grey, alpha = 0.3) +
    
    geom_line() +
    geom_point(size = 0.5) +
    theme_bw() +
    facet_grid(.~type, scale = "free_y", 
               labeller = as_labeller(c("adm" = "Admissions",
                                        "disch" = "Discharges"))) +
    ylab("Percent") +
    scale_x_date(date_labels = "%b", date_breaks = "1 month") +
    scale_color_manual(values = c(THF_red, THF_50pct_light_blue), labels = c("from/to care home", "other source/destination")) +
    
    scale_y_continuous(breaks = seq(40, 100, by = 20)) +
    theme(axis.title.x = element_blank(),
          legend.position = "top",
          legend.justification = c(1,0),
          axis.text = element_text(colour = THF_dark_grey),
          axis.title = element_text(colour = THF_dark_grey),
          axis.ticks = element_line(colour = THF_light_grey),
          panel.grid = element_blank(),
          panel.grid.major.y = element_line(colour = THF_50pct_light_grey),
          legend.text = element_text(colour = THF_dark_grey),
          legend.title = element_blank(),
          plot.title = element_text(colour = "#005078"),
          plot.subtitle = element_text(colour = "#005078"),
          strip.text = element_text(colour = "#524c48"),
          strip.background = element_blank()) +
    annotate("text", x = ymd("0004-03-18"), y = 118, 
             label = "17 March,\nNHS discharge\nguidance", color = THF_dark_grey, size = 2, hjust = 0) +
    annotate("text", x = ymd("0004-04-14"), y = 120, 
             label = "10-13 April,\nEaster 2020", color = THF_dark_grey, size = 2, hjust = 0) +
    labs(title = "Hospital admissions and discharges in 2020", 
         subtitle = "Seven day moving average,\nindexed to the same period in 2019",
         color = "Source/destination")) %>% 
  ggsave("graphs/sprint_3/Admissions_discharges_excldeaths_rel2019.png", ., width = 7, height = 4, dpi = 600)

# Ratio discharges to admissions
(combined_april_excldeaths %>% 
   filter(day_dummy <= ymd("0004-04-30") & day_dummy >= ymd("0004-02-01") & sourcedest != "all") %>% 
   group_by(sourcedest, date, day_dummy) %>% 
   mutate(disch_adm_ratio = sevendayavg[type == "disch"] / sevendayavg[type == "adm"]) %>% 
      ggplot(aes(x = day_dummy, y = disch_adm_ratio, color = year, group = year)) +
      geom_vline(xintercept = ymd("0004-03-17"), color = THF_dark_grey, linetype = "dashed", alpha = 0.3) +
      geom_line() +
      geom_point(size = 0.5) +
      theme_bw() +
      facet_wrap(vars(sourcedest), scales = "free", 
                 labeller = as_labeller(c("ch" = "from/to care homes",
                                          "other" = "from/to other dest."))) +
      ylab("Discharges / admissions (ratio 7 day rolling averages)") +
      scale_x_date(date_labels = "%b", date_breaks = "1 month") +
      theme(axis.title.x = element_blank(),
            legend.position = "right",
            legend.justification = c(1,0),
            axis.text = element_text(colour = THF_dark_grey),
            axis.title = element_text(colour = THF_dark_grey),
            axis.ticks = element_line(colour = THF_light_grey),
            panel.grid = element_blank(),
            panel.grid.major.y = element_line(colour = THF_50pct_light_grey),
            panel.grid.minor.y = element_line(colour = THF_50pct_light_grey),
            legend.text = element_text(colour = THF_dark_grey),
            legend.title = element_blank(),
            plot.title = element_text(colour = "#005078"),
            plot.subtitle = element_text(colour = "#005078"),
            strip.text = element_text(colour = "#524c48"),
            strip.background = element_blank()) +
      expand_limits(y=0) +
      labs(title = "Ratio discharges to admissions")) %>% 
    ggsave("graphs/sprint_3/Admissions_discharges_excldeaths_ratio.png", ., width = 9, height = 4, dpi = 600)
  
  
# During February, what % of admissions and discharges were from/to care homes?
# and how does the absolute number compare to the 5-year average of 2015-2019

admissions_april_excldeaths %>% 
  bind_rows(discharges_april_excldeaths) %>%
  filter(month(date) == 2) %>% 
  group_by(type, year, sourcedest) %>% 
  summarise(sevendayavg_sum = sum(sevendayavg),
            value_sum = sum(value),
            date_start = min(date),
            date_end = max(date)) %>%  
  group_by(type, year) %>% 
  mutate(value_pct = 100* (value_sum / value_sum[sourcedest == "all"]),
         sevendayavg_pct = 100* (sevendayavg_sum / sevendayavg_sum[sourcedest == "all"])) %>% 
  group_by(type, sourcedest) %>% 
  mutate(value_ref_2015_to_2019 = mean(value_sum[year %in% c(2015:2019)]),
         value_percent_2015_to_2019 = round(100*value_sum / value_ref_2015_to_2019, 1)) %>% 
  write_csv("graphs/sprint_3/Admissions_discharges_February_sum.csv")
  
# Same for March and April combined  

admissions_april_excldeaths %>% 
    bind_rows(discharges_april_excldeaths) %>%
    filter(month(date) %in% c(3,4)) %>% 
    group_by(type, year, sourcedest) %>% 
    summarise(sevendayavg_sum = sum(sevendayavg),
              value_sum = sum(value),
              date_start = min(date),
              date_end = max(date)) %>%  
    group_by(type, year) %>% 
    mutate(value_pct = 100* (value_sum / value_sum[sourcedest == "all"]),
           sevendayavg_pct = 100* (sevendayavg_sum / sevendayavg_sum[sourcedest == "all"])) %>% 
    group_by(type, sourcedest) %>% 
    mutate(value_ref_2015_to_2019 = mean(value_sum[year %in% c(2015:2019)]),
           value_percent_2015_to_2019 = round(100*value_sum / value_ref_2015_to_2019, 1)) %>%  
      write_csv("graphs/sprint_3/Admissions_discharges_March-April_sum.csv")
    
# Between 17 March and 30 April
  
  admissions_april_excldeaths %>% 
    bind_rows(discharges_april_excldeaths) %>%
    filter(day_dummy >= ymd("0004-03-17") & day_dummy <= ymd("0004-04-30")) %>% 
    group_by(type, year, sourcedest) %>% 
    summarise(sevendayavg_sum = sum(sevendayavg),
              value_sum = sum(value),
              date_start = min(date),
              date_end = max(date)) %>%  
    group_by(type, year) %>% 
    mutate(value_pct = 100* (value_sum / value_sum[sourcedest == "all"]),
           sevendayavg_pct = 100* (sevendayavg_sum / sevendayavg_sum[sourcedest == "all"])) %>% 
    group_by(type, sourcedest) %>% 
    mutate(value_ref_2015_to_2019 = mean(value_sum[year %in% c(2015:2019)]),
           value_percent_2015_to_2019 = round(100*value_sum / value_ref_2015_to_2019, 1)) %>%  
    write_csv("graphs/sprint_3/Admissions_discharges_March17-April_sum.csv")
  


# Rgional and care home type aggregates -----------------------------------

# Sums betweetn 17 March and 30 April


discharges_regional <- read_csv("data/sprint_3/2020_06_22_2/CH_discharges_regional_Mar-April.csv",
                                col_types = ) 

discharges_regional %>% 
  group_by(year, dischtype) %>% 
  summarise(count = sum(count))

