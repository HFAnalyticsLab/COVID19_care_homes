####
# PHE data on care home outbreaks
####

library(tidyverse)
library(tidylog)
library(janitor)
library(lubridate)
library(readODS)

# Plotting ------------------------------------------------------------------
THF_red <- '#dd0031'

# Import data  ------------------------------------------------------------

# COVID-19: number of outbreaks in care homes – management information
# https://www.gov.uk/government/statistical-data-sets/covid-19-number-of-outbreaks-in-care-homes-management-information
ch_outbreaks <- read_ods("data/open/Care_home_outbreaks_of_COVID-19_Management_Information.ods",
                        sheet = 'Government_office_regions', skip = 1) %>% 
  rename(region_name = 1) %>% 
  as_tibble() %>% 
  clean_names() %>% 
  select(region_name, rgn09cd, all_outbreaks, number_of_care_homes, pct = percentage_of_care_homes_that_have_reported_an_outbreak)

ch_outbreaks %>% 
  select(region_name, all_outbreaks, number_of_care_homes, pct) %>% 
  write_csv("processed_data/CH_outbreaks_by_region.csv")


(ch_outbreaks %>% 
    filter(region_name != "Unspecified") %>% 
    ggplot(aes(x = fct_reorder(region_name, pct), y = pct)) +
    geom_bar(stat = "identity", position = position_dodge(), width = 0.8, fill = THF_red) +
    theme_bw() +
    ylab("% care homes that have reported an outbreak") +
    coord_flip()+
    theme(axis.title.y = element_blank(),
          legend.position = "none",
          panel.grid.major.y = element_blank()) +
    labs(title = "COVID-19 reported outbreaks in care homes", 
         subtitle = "May 14, 2020", 
         caption = "Source: PHE")) %>% 
  ggsave("graphs/sprint_2/Care_home_outbreaks_bar.png", ., width = 6, height = 4)
