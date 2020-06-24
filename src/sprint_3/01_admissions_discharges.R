####
# Descriptive analysis of hospital admissions and discharges (all and from/to care homes)
####

library(tidyverse)
library(tidylog)
library(janitor)
library(lubridate)
library(readxl)
library(zoo)

# Plotting ------------------------------------------------------------------
THF_red <- '#dd0031'
THF_50pct_light_blue <- '#aad3e5'
THF_1_purple <- '#744284'
THF_dark_grey <-  '#524c48'
THF_light_grey <- '#e2dfd8'
THF_50pct_light_grey <- '#EEEDE8'

# Functions ---------------------------------------------------------------

sevendayavg <- function(col, rounding_digits = 0){
  
  rollapply(col, 7, 
            mean, align = "right", partial = FALSE, fill = NA) %>% 
    round(rounding_digits)
  
}

#### Admission & discharges until end of April (deaths included in other discharges) ------------------------------------------------------------------

# summary of the number of admissions and discharges in England by day
# file created and released from the secure server by RW on 5.6.2020
discharges_april <- read_xlsx("data/sprint_3/2020_06_15/01 Hospital Admission and Discharge to Care Homes - Summary.xlsx",
                        sheet = "Disch 1420 All Exclusions") 

admissions_april <- read_xlsx("data/sprint_3/2020_06_15/01 Hospital Admission and Discharge to Care Homes - Summary.xlsx",
                              sheet = "Adm 1420 All Exclusions") 


## Discharges
discharges_april <- discharges_april %>%
  pivot_longer(c(-dischday, -dischmnth), names_to = "type", values_to = "value") %>% 
  separate(type, into = c("type", "disch_year"), sep = "_") %>% 
  group_by(type) %>% 
  filter(!is.na(value)) %>% 
  mutate(date = ymd(str_c(disch_year, 
                          str_pad(dischmnth, 2, side = "left", pad = "0"), 
                          str_pad(dischday, 2, side = "left", pad = "0")))) %>% 
  filter(date > ymd("2014-08-01"))

# percent relative to all discharges on a given day
discharges_april <- discharges_april %>% 
  arrange(disch_year, dischmnth, dischday) %>% 
  group_by(disch_year, dischmnth, dischday, date) %>%
  mutate(ref_alldisch = value[type == "dischall"],
         percent_all = round(100*value / ref_alldisch, 1)) 

# percent relative to first half of feb, per year
discharges_april <- discharges_april %>%
    group_by(disch_year, type) %>%
  mutate(ref_feb = mean(value[dischmnth == 2 & dischday %in% c(1:15)]),
         percent_feb = round(100*value / ref_feb, 1))

# moving averages
discharges_april <- discharges_april %>%
  group_by(type) %>%
  mutate(sevendayavg = sevendayavg(value),
         sevendayavg_percent_all = sevendayavg(percent_all,2 ),
         sevendayavg_percent_feb = sevendayavg(percent_feb,2 ),
         day_dummy  = `year<-`(date, 0004)) # Feb 29th only parses when 0004 is used as year

# percent relative to the average of 2015-2019
discharges_april <- discharges_april %>%
  group_by(type, day_dummy) %>%
  mutate(ref_2015_to_2019 = mean(sevendayavg[disch_year %in% c(2015:2019)]),
         percent_2015_to_2019 = round(100*sevendayavg / ref_2015_to_2019, 1)) 
  

(discharges_april %>% 
    filter(type != "dischall" & day_dummy <= ymd("0004-04-30")) %>% 
    ggplot(aes(x = day_dummy, y = sevendayavg, color = disch_year, group = disch_year)) +
    geom_line() +
    geom_point(size = 0.5) +
    theme_bw() +
    facet_grid(type ~ ., scale = "free_y", 
               labeller = as_labeller(c("dischch" = "care home discharges",
                                        "dischother" = "other destination"))) +
    ylab("Number of discharges (7 day rolling average)") +
    scale_x_date(date_labels = "%b", date_breaks = "1 month") +
    theme(axis.title.x = element_blank(),
          panel.grid.major.x = element_blank()) +
    labs(title = "Hospital discharges Jan to April, 2014-2020")) %>% 
  ggsave("graphs/sprint_3/Discharges_abs.png", ., width = 6, height = 4)


(discharges_april %>% 
    filter(day_dummy <= ymd("0004-04-30") & day_dummy >= ymd("0001-01-31")) %>% 
    ggplot(aes(x = day_dummy, y = sevendayavg_percent_feb, color = disch_year, group = disch_year)) +
    geom_line() +
    geom_point(size = 0.5) +
    theme_bw() +
    facet_grid(type ~ .,
               labeller = as_labeller(c("dischall" = "all discharges",
                                        "dischch" = "care home discharges",
                                        "dischother" = "other destination"))) +
    ylab("Percent (7 day rolling average)") +
    scale_x_date(date_labels = "%b", date_breaks = "1 month") +
    annotate("rect", xmin = ymd("0004-02-01"), xmax = ymd("0004-02-15"), 
             ymin = 40, ymax = 120, alpha = 0.7) +
    theme(axis.title.x = element_blank(),
          panel.grid.major.x = element_blank()) +
    labs(title = "Hospital discharges Jan to April, 2014-2020", 
         subtitle = "Shown relative to the average of Feb 1-15 per year")) %>% 
  ggsave("graphs/sprint_3/Discharges_percent_feb.png", ., width = 5, height = 5)

(discharges_april %>% 
    filter(type != "dischall" & day_dummy <= ymd("0004-04-30")) %>% 
    ggplot(aes(x = day_dummy, y = sevendayavg_percent_all, color = disch_year, group = disch_year)) +
    geom_line() +
    geom_point(size = 0.5) +
    theme_bw() +
    facet_grid(type ~ ., scale = "free_y", 
               labeller = as_labeller(c("dischch" = "care home discharges",
                                        "dischother" = "other destination"))) +
    scale_x_date(date_labels = "%b", date_breaks = "1 month") +
    ylab("Percent (7 day rolling average)") +
    theme(axis.title.x = element_blank(),
          panel.grid.major.x = element_blank()) +
    labs(title = "Hospital discharges 2019 and 2020", 
         subtitle = "Shown as percentage of all discharges")) %>% 
  ggsave("graphs/sprint_3/Discharges_percent.png", ., width = 5, height = 4)

(discharges_april %>% 
    filter(type != "dischall" & day_dummy <= ymd("0004-04-30") & disch_year == 2020) %>% 
    ggplot(aes(x = day_dummy, y = percent_2015_to_2019, color = type, group = type)) +
    geom_line() +
    geom_point(size = 0.5) +
    theme_bw() +
    scale_color_manual(values = c(THF_red, THF_50pct_light_blue), labels = c("care home discharges","other destination")) +
    scale_x_date(date_labels = "%b", date_breaks = "1 month") +
    ylab("Percent") +
    geom_vline(xintercept = ymd("0004-03-17"), color = 'grey', linetype = "dashed") +
    theme(axis.title.x = element_blank(),
          panel.grid.major.x = element_blank(),
          legend.title = element_blank(),
          legend.position = "top") +
    labs(title = "Hospital discharges in 2020", 
         subtitle = "7-day moving averages,\nshown relative to the mean of 2015-2019")) %>% 
  ggsave("graphs/sprint_3/Discharges_percent_mean_2015_to_2019.png", ., width = 5, height = 4)

## Admissions
admissions_april <- admissions_april %>%
  pivot_longer(c(-admday, -admmnth), names_to = "type", values_to = "value") %>% 
  separate(type, into = c("type", "adm_year"), sep = "_") %>% 
  group_by(type) %>% 
  filter(!is.na(value)) %>% 
  mutate(date = ymd(str_c(adm_year, 
                          str_pad(admmnth, 2, side = "left", pad = "0"), 
                          str_pad(admday, 2, side = "left", pad = "0")))) %>% 
  filter(date > ymd("2014-08-01"))

# percent relative to all discharges on a given day
admissions_april <- admissions_april %>% 
  arrange(adm_year, admmnth, admday) %>% 
  group_by(adm_year, admmnth, admday, date) %>%
  mutate(ref_alladm = value[type == "admall"],
         percent_all = round(100*value / ref_alladm, 1)) 

# percent relative to first half of feb, per year
admissions_april <- admissions_april %>% 
  group_by(adm_year, type) %>%
  mutate(ref_feb = mean(value[admmnth == 2 & admday %in% c(1:15)]),
         percent_feb = round(100*value / ref_feb, 1)) 

# moving averages
admissions_april <- admissions_april %>% 
  group_by(type) %>%
  mutate(sevendayavg = sevendayavg(value),
         sevendayavg_percent_all = sevendayavg(percent_all,2 ),
         sevendayavg_percent_feb = sevendayavg(percent_feb,2 ),
         day_dummy  = `year<-`(date, 0004))


# percent relative to the average of 2015-2019
admissions_april <- admissions_april %>%
  group_by(type, day_dummy) %>%
  mutate(ref_2015_to_2019 = mean(sevendayavg[adm_year %in% c(2015:2019)]),
         percent_2015_to_2019 = round(100*sevendayavg / ref_2015_to_2019, 1)) 



(admissions_april %>% 
    filter(type != "admall" & day_dummy <= ymd("0004-04-30")) %>% 
    ggplot(aes(x = day_dummy, y = sevendayavg, color = adm_year, group = adm_year)) +
    geom_line() +
    geom_point(size = 0.5) +
    theme_bw() +
    facet_grid(type ~ ., scale = "free_y", 
               labeller = as_labeller(c("admch" = "care home admissions",
                                        "admother" = "other source"))) +
    ylab("Number of admissions (7 day rolling average)") +
    scale_x_date(date_labels = "%b", date_breaks = "1 month") +
    theme(axis.title.x = element_blank(),
          panel.grid.major.x = element_blank()) +
    labs(title = "Hospital admissions Jan to April, 2014-2020")) %>% 
  ggsave("graphs/sprint_3/admissions_abs.png", ., width = 6, height = 4)


(admissions_april %>% 
    filter(day_dummy <= ymd("0004-04-30") & day_dummy >= ymd("0004-01-31")) %>% 
    ggplot(aes(x = day_dummy, y = sevendayavg_percent_feb, color = adm_year, group = adm_year)) +
    geom_line() +
    geom_point(size = 0.5) +
    theme_bw() +
    facet_grid(type ~ .,
               labeller = as_labeller(c("admall" = "all admissions",
                                        "admch" = "care home admissions",
                                        "admother" = "other source"))) +
    ylab("Percent (7 day rolling average)") +
    scale_x_date(date_labels = "%b", date_breaks = "1 month") +
    annotate("rect", xmin = ymd("0004-02-01"), xmax = ymd("0004-02-15"), 
             ymin = 40, ymax = 120, alpha = 0.7) +
    theme(axis.title.x = element_blank(),
          panel.grid.major.x = element_blank()) +
    labs(title = "Hospital admissions Jan to April, 2014-2020", 
         subtitle = "Shown relative to the average of Feb 1-15 per year")) %>% 
  ggsave("graphs/sprint_3/admissions_percent_feb.png", ., width = 5, height = 5)

(admissions_april %>% 
    filter(type != "admall" & day_dummy <= ymd("0004-04-30")) %>% 
    ggplot(aes(x = day_dummy, y = sevendayavg_percent_all, color = adm_year, group = adm_year)) +
    geom_line() +
    geom_point(size = 0.5) +
    theme_bw() +
    facet_grid(type ~ ., scale = "free_y", 
               labeller = as_labeller(c("admch" = "care home admissions",
                                        "admother" = "other source"))) +
    scale_x_date(date_labels = "%b", date_breaks = "1 month") +
    ylab("Percent (7 day rolling average)") +
    theme(axis.title.x = element_blank(),
          panel.grid.major.x = element_blank()) +
    labs(title = "Hospital admissions 2019 and 2020", 
         subtitle = "Shown as percentage of all admissions")) %>% 
  ggsave("graphs/sprint_3/admissions_percent.png", ., width = 5, height = 4)


(admissions_april %>% 
    filter(type != "admall" & day_dummy <= ymd("0004-04-30") & adm_year == 2020) %>% 
    ggplot(aes(x = day_dummy, y = percent_2015_to_2019, color = type, group = type)) +
    geom_line() +
    geom_point(size = 0.5) +
    theme_bw() +
    scale_color_manual(values = c(THF_red, THF_50pct_light_blue), labels = c("care home admissions","other sources")) +
    scale_x_date(date_labels = "%b", date_breaks = "1 month") +
    ylab("Percent") +
    geom_vline(xintercept = ymd("0004-03-17"), color = 'grey', linetype = "dashed") +
    theme(axis.title.x = element_blank(),
          panel.grid.major.x = element_blank(),
          legend.title = element_blank(),
          legend.position = "top") +
    labs(title = "Hospital admissions in 2020", 
         subtitle = "7-day moving averages,\nshown relative to the mean of 2015-2019")) %>% 
  ggsave("graphs/sprint_3/Admissions_percent_mean_2015_to_2019.png", ., width = 5, height = 4)



# Comparing admissions to discharges

combined_april <- admissions_april %>% 
  ungroup() %>% 
  select(date, day_dummy, type, year = adm_year, admissions = value) %>% 
  mutate(type = gsub("adm", "", type)) %>% 
  left_join(discharges_april %>% 
              ungroup() %>% 
              select(date, day_dummy, type, year = disch_year, discharges = value) %>% 
              mutate(type = gsub("disch", "", type))) %>% 
  mutate(disch_over_adm = discharges / admissions) %>% 
  arrange(date) %>% 
  group_by(type) %>%
  mutate(disch_over_adm_sevendayavg = sevendayavg(disch_over_adm, 2))

(combined_april %>%
    filter(day_dummy <= ymd("0004-04-30") & day_dummy >= ymd("0004-03-01") & type != "all") %>% 
    ggplot(aes(x = day_dummy, y = disch_over_adm_sevendayavg, color = year, group = year)) +
    geom_line() +
    geom_point(size = 0.5) +
    theme_bw() +
    facet_grid(.~type, scale = "free_y", 
               labeller = as_labeller(c("all" = "all",
                                        "ch" = "care home",
                                        "other" = "other"))) +
    ylab("Discharges / admissions (7 day rolling average)") +
    scale_x_date(date_labels = "%b", date_breaks = "1 month") +
    theme(axis.title.x = element_blank(),
          panel.grid.major.x = element_blank()) +
    labs(title = "Hospital admissions Jan to April, 2014-2020")) %>% 
  ggsave("graphs/sprint_3/discharges_over_admissions.png", ., width = 6, height = 4)


#### Admissions & discharges until end of April excluding deaths  -------------------------------------------------

# summary of the number of admissions and discharges in England by day
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
         percent_2015_to_2019 = round(100*sevendayavg / ref_2015_to_2019, 1)) 


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
         percent_2015_to_2019 = round(100*sevendayavg / ref_2015_to_2019, 1)) 

# Admissions and discharges in a combined graph ----

combined_april_excldeaths <- admissions_april_excldeaths %>% 
  bind_rows(discharges_april_excldeaths) %>%
  filter(day_dummy <= ymd("0004-04-30") & day_dummy >= ymd("0004-02-01") & sourcedest != "all" & year == "2020")

combined_april_excldeaths %>% 
  ungroup() %>% 
  select(-ref_2015_to_2019, -value, -sevendayavg, -year, -day_dummy) %>% 
  unite("combined_col", type, sourcedest) %>% 
  pivot_wider(names_from = "combined_col", values_from = "percent_2015_to_2019") %>% 
  write_csv("graphs/sprint_3/Admissions_discharges_excldeaths.csv")


(combined_april_excldeaths  %>% 
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
             label = "NHS discharge\nguidance", color = THF_dark_grey, size = 2, hjust = 0) +
    annotate("text", x = ymd("0004-04-21"), y = 120, 
             label = "Easter 2020", color = THF_dark_grey, size = 2) +
    labs(title = "Hospital admissions and discharges in 2020", 
         subtitle = "7-day moving averages, shown as a percentage of the mean of 2015-2019",
         color = "Source/destination")) %>% 
  ggsave("graphs/sprint_3/Admissions_discharges_excldeaths.png", ., width = 7, height = 4, dpi = 600)
