####
# Descriptive analysis of current care home data
####

# Data needs to be read back in from csv files after they were released from the
# secure server

library(tidyverse)
library(tidylog)
library(janitor)
library(lubridate)

library(broom)
library(geojsonio)
library(maptools)

# Shape files -------------------------------------------------------------

# Plotting ------------------------------------------------------------------
THF_red <- '#dd0031'
THF_50pct_light_blue <- '#aad3e5'

region_order <- c("LONDON", "SOUTH EAST", "SOUTH WEST", "EAST OF ENGLAND", "EAST MIDLANDS", "WEST MIDLANDS",
                  "YORKSHIRE AND HUMBER",  "NORTH EAST", "NORTH WEST")
# Regional data  ------------------------------------------------------------------

summary_region <- read_csv("data/sprint_2/CH_summary_2020-04-01_ch_region.csv") %>% 
  pivot_longer(-ch_region, names_to = "type", values_to = "value") %>% 
  filter(ch_region != 'UNSPECIFIED') %>% 
  mutate(ch_region_fct = factor(ch_region, levels = rev(region_order)))

summary_region_nursing <- read_csv("data/sprint_2/CH_summary_2020-04-01_chregion_chnursing.csv") %>% 
  separate(chregion_chnursing, into = c("ch_region", "ch_nursing"), sep = "_") %>% 
  pivot_longer(c(-ch_region, -ch_nursing), names_to = "type", values_to = "value") %>% 
  filter(ch_region != 'UNSPECIFIED') %>% 
  mutate(ch_nursing = as.factor(ch_nursing),
         ch_region_fct = factor(ch_region, levels = rev(region_order)))


# Care home size
(summary_region %>% 
  filter(type %in% c("mean_chsize", "median_chsize")) %>% 
  ggplot(aes(x = ch_region_fct, y = value, group = type, fill = type)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  coord_flip() +
  theme_bw() +
  ylab("Number of beds") +
  theme(axis.title.y = element_blank(),
        legend.position = "top",
        legend.justification= c(1,0),
        panel.grid = element_blank()) +
  scale_fill_manual(values = c(THF_red, THF_50pct_light_blue), labels = c("mean", "median")) +
  labs(title = "Care home size in England, by region", subtitle = "April 2020, CQC care home information", fill = "")) %>% 
  ggsave("graphs/sprint_2/Care_home_size_by_region.png", ., width = 5, height = 4)

(summary_region_nursing %>% 
    filter(type %in% c("mean_chsize", "median_chsize")) %>% 
    ggplot(aes(x = ch_region_fct, y = value, group = type, fill = type)) +
    facet_wrap("ch_nursing", ncol = 2, scales = "free_x", labeller = as_labeller(c("0" = "Residential care homes",
                                                                                   "1" = "Nursing homes"))) +
    geom_bar(stat = "identity", position = position_dodge()) +
    coord_flip() +
    theme_bw() +
    ylab("Number of beds") +
    theme(axis.title.y = element_blank(),
          legend.position = "top",
          legend.justification= c(1,0),
          panel.grid = element_blank()) +
    scale_fill_manual(values = c(THF_red, THF_50pct_light_blue), labels = c("mean", "median")) +
    labs(title = "Care home size in England, by region", subtitle = "April 2020, CQC care home information", fill = "")) %>% 
  ggsave("graphs/sprint_2/Care_home_size_by_region_and_type.png", ., width = 7, height = 5)


# Number of homes and beds
(summary_region %>% 
  filter(type %in% c("resident_ch", "nursing_ch", "resident_beds", "nursing_beds")) %>% 
  mutate(type = factor(type, levels = c("resident_ch", "resident_beds", "nursing_ch", "nursing_beds")),
         type = fct_recode(type,  `Nursing homes` = "nursing_ch", `Nursing home beds` = "nursing_beds",
                           `Residential care homes` = "resident_ch" , `Residential care home beds` = "resident_beds")) %>% 
  group_by(type) %>% 
  mutate(pct = str_c(round(100* value / sum(value), 0), "%")) %>% 
  ggplot(aes(x = ch_region_fct, y = value)) +
  facet_wrap("type", ncol = 2, scales = "free_x") +
  geom_bar(stat = "identity", position = position_dodge(), fill = THF_red) +
  geom_text(aes(label = pct, y = 0.9*value), size = 3, color = "white") +
  coord_flip() +
  theme_bw() +
  theme(axis.title = element_blank(),
        legend.position = "top",
        legend.justification= c(1,0),
        panel.grid.major.y = element_blank()) +
  labs(title = "Care homes and beds in England, by region", subtitle = "April 2020, CQC care home information", fill = "")) %>% 
  ggsave("graphs/sprint_2/Care_home_numbers_by_region.png", ., width = 9, height = 6)


# Deprivation -------------------------------------------------------------

summary_IMD <- read_csv("data/sprint_2/CH_summary_2020-04-01_IMD_decile.csv") %>% 
  pivot_longer(-IMD_decile, names_to = "type", values_to = "value") %>% 
  filter(!is.na(IMD_decile)) %>% 
  mutate(ch_region = "ENGLAND")
 
summary_region_IMD <- read_csv("data/sprint_2/CH_summary_2020-04-01_chregion_IMDdecile.csv") %>% 
  separate(chregion_IMDdecile, into = c("ch_region", "IMD_decile"), sep = "_") %>% 
  pivot_longer(c(-ch_region, -IMD_decile), names_to = "type", values_to = "value") %>% 
  filter(ch_region != 'UNSPECIFIED' & !is.na(IMD_decile)) %>% 
  mutate(IMD_decile = as.numeric(IMD_decile)) %>% 
  bind_rows(summary_IMD)  %>% 
  mutate(ch_region_fct = factor(ch_region, levels = rev(c("ENGLAND", region_order))),
         IMD_decile_fct = factor(IMD_decile, levels = str_c(10:1))) 
  

(summary_region_IMD %>% 
  filter(type %in% c("resident_ch", "nursing_ch", "resident_beds", "nursing_beds")) %>% 
  mutate(type = factor(type, levels = c("resident_ch", "resident_beds", "nursing_ch", "nursing_beds")),
         type = fct_recode(type,  `Nursing homes` = "nursing_ch", `Nursing home beds` = "nursing_beds",
                           `Residential care homes` = "resident_ch" , `Residential care home beds` = "resident_beds")) %>% 
  group_by(ch_region, type) %>% 
  mutate(pct = 100* value / sum(value)) %>% 
  ggplot(aes(x = ch_region_fct, y = pct, fill = IMD_decile_fct)) +
  facet_wrap("type", ncol = 2, scales = "free_x") +
  geom_bar(stat = "identity", position = position_stack()) +
  coord_flip() +
  theme_bw() +
  ylab("Percent") +
  scale_fill_manual(values = scales::seq_gradient_pal(THF_50pct_light_blue, THF_red, "Lab")(seq(0, 1,length.out = 10)),
                    drop = FALSE) +
  guides(fill = guide_legend(nrow = 1, byrow = TRUE, reverse = TRUE)) +
  theme(axis.title.y = element_blank(),
        legend.position = "top",
        legend.justification= c(1,0),
        panel.grid.major.y = element_blank()) +
  labs(title = "Care homes and beds England, by region and IMD 2019 decile", 
       subtitle = "April 2020, CQC care home information, 1 = most deprived 10%", fill = "")) %>% 
  ggsave("graphs/sprint_2/Care_home_IMD_by_region.png", ., width = 9, height = 6)
