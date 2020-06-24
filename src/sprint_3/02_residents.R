####
# Descriptive analysis of care home residents listed in the MPIs
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

# Import data ------------------------------------------------------------------

chr <- read_csv("data/sprint_3/2020_06_15/Chr_descriptives_England_SDC.csv", 
                col_types = cols(extdatefrom = col_date(format ="%d/%m/%Y"))) %>% 
  mutate(chr_a_covid_h36 = gsub("<", "", chr_a_covid_h36),
         chr_a_covid_h36 = as.numeric(chr_a_covid_h36),
         chr_residential = chr - chr_nursing - chr_care_home_closed,
         chr_residential_pct = 100 - chr_nursing_pct - chr_care_home_closed_pct)


chr <- chr %>% 
  pivot_longer(-extdatefrom, names_to = "type", values_to = "value") %>% 
  mutate(stat = ifelse(str_detect(type, "_pct"), "percent", "count"),
         type = gsub("_pct", "", type)) %>% 
  pivot_wider(names_from =  "stat", values_from = "value") %>% 
  mutate(year = factor(year(extdatefrom)),
         day_dummy = `year<-`(extdatefrom, 0001))
                      

chr_nursing <- read_csv("data/sprint_3/2020_06_15/Chr_descriptives_England_chnursing_SDC.csv", 
                col_types = cols(extdatefrom = col_date(format ="%d/%m/%Y"))) %>% 
  mutate(chr_a_covid_h36 = gsub("<", "", chr_a_covid_h36),
         chr_a_covid_h36 = as.numeric(chr_a_covid_h36))


chr_nursing <- chr_nursing %>% 
  pivot_longer(c(-extdatefrom, -ch_nursing), names_to = "type", values_to = "value") %>% 
  mutate(stat = ifelse(str_detect(type, "_pct"), "percent", "count"),
         type = gsub("_pct", "", type)) %>% 
  pivot_wider(names_from =  "stat", values_from = "value") %>% 
  mutate(year = factor(year(extdatefrom)),
         day_dummy = `year<-`(extdatefrom, 0001))

# Visualisations ----------------------------------------------------------

# Residents
(chr %>% 
  filter(type == "chr") %>% 
  ggplot(aes(x = day_dummy, y = count, group = year, color = year)) +
  geom_point() +
  geom_line() +
  theme_bw() +
  scale_x_date(date_labels = "%b", date_breaks = "1 month") +
  labs(title = "Care home residents in England", subtitle = "Residents with a care home flag in the MPI")) %>% 
  ggsave("graphs/sprint_3/ChResidents.png", ., width = 6, height = 4)


# Residents by care home type
(chr %>% 
    filter(type %in% c("chr_nursing", "chr_residential", "chr_care_home_closed")) %>% 
    ggplot(aes(x = day_dummy, y = percent, group = year, color = year)) +
    facet_wrap("type", nrow = 1)+
    geom_line()+
    geom_point()+
    theme_bw() +
    scale_x_date(date_labels = "%b", date_breaks = "1 month") +
    expand_limits(y = 0) +
    ylab("percent") +
    labs(title = "Care home residents in England by care home type", 
         subtitle = "Residents with a care home flag in the MPI")) %>% 
  ggsave("graphs/sprint_3/ChResidents_carehometype.png", ., width = 6, height = 3)

# Mean age
(chr_nursing %>% 
    filter(type == "chr_age_mean") %>% 
    ggplot(aes(x = day_dummy, y = count, group = year, color = year)) +
    facet_grid(.~ch_nursing, labeller = as_labeller(c(`0` = "Residential",
                                                      `1` = "Nursing"))) +
    geom_line()+
    geom_point()+
    theme_bw() +
    scale_x_date(date_labels = "%b", date_breaks = "1 month") +
    expand_limits(y = 0) +
    ylab("Mean age [years]") +
    labs(title = "Care home residents in England - mean age", 
         subtitle = "Residents with a care home flag in the MPI")) %>% 
  ggsave("graphs/sprint_3/ChResidents_carehometype_age.png", ., width = 6, height = 3)

