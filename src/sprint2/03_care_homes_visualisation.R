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

# Import data  ------------------------------------------------------------------

# care homes split by region
summary_region <- read_csv("data/sprint_2/CH_summary_2020-04-01_ch_region.csv") %>% 
  pivot_longer(-ch_region, names_to = "type", values_to = "value") %>% 
  filter(ch_region != 'UNSPECIFIED') %>% 
  mutate(ch_region = str_to_title(ch_region))

# care homes split by region and residential/nursing
summary_region_nursing <- read_csv("data/sprint_2/CH_summary_2020-04-01_chregion_chnursing.csv") %>% 
  separate(chregion_chnursing, into = c("ch_region", "ch_nursing"), sep = "_") %>% 
  pivot_longer(c(-ch_region, -ch_nursing), names_to = "type", values_to = "value") %>% 
  filter(ch_region != 'UNSPECIFIED') %>% 
  mutate(ch_nursing = as.factor(ch_nursing),
         ch_region = str_to_title(ch_region))

# care homes split by IMD decile
summary_IMD <- read_csv("data/sprint_2/CH_summary_2020-04-01_IMD_decile.csv") %>% 
  pivot_longer(-IMD_decile, names_to = "type", values_to = "value") %>% 
  filter(!is.na(IMD_decile)) %>% 
  mutate(ch_region = "ENGLAND")

# care homes split by region and IMD decile
summary_region_IMD <- read_csv("data/sprint_2/CH_summary_2020-04-01_chregion_IMDdecile.csv") %>% 
  separate(chregion_IMDdecile, into = c("ch_region", "IMD_decile"), sep = "_") %>% 
  pivot_longer(c(-ch_region, -IMD_decile), names_to = "type", values_to = "value") %>% 
  filter(ch_region != 'UNSPECIFIED' & !is.na(IMD_decile)) %>% 
  mutate(IMD_decile = as.numeric(IMD_decile)) %>% 
  bind_rows(summary_IMD)  %>% 
  mutate(ch_region = str_to_title(ch_region),
         IMD_decile_fct = factor(IMD_decile, levels = str_c(10:1)),
         IMD_quintile_fct = fct_collapse(IMD_decile_fct, "1 most deprived" = c("1", "2"),
                                         "2" = c("3", "4"), "3" = c("5", "6"), 
                                         "4" = c("7", "8"), "5 least deprived" = c("9", "10"))) 

# Visualise care home sizes  ------------------------------------------------------------------

chnursion_regions <- summary_region_nursing[summary_region_nursing$type == "mean_chsize" & 
                                              summary_region_nursing$ch_nursing == 0,]
size_order <- chnursion_regions$ch_region[order(chnursion_regions$value)] 



# Care home size, mean and median
(summary_region %>% 
  filter(type %in% c("mean_chsize", "median_chsize")) %>% 
  ggplot(aes(x = ch_region, y = value, group = type, fill = type)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  scale_x_discrete(limits = rev(size_order)) +
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

# Care home size, mean and median, split by residential/nursing
(summary_region_nursing %>% 
    filter(type %in% c("mean_chsize", "median_chsize")) %>% 
    ggplot(aes(x = ch_region, y = value, group = type, fill = type)) +
    facet_wrap("ch_nursing", ncol = 2, scales = "free_x", labeller = as_labeller(c("0" = "Residential care homes",
                                                                                   "1" = "Nursing homes"))) +
    geom_bar(stat = "identity", position = position_dodge()) +
    scale_x_discrete(limits = rev(size_order)) +
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

# Care home size, mean only, split by residential/nursing
(summary_region_nursing %>% 
    filter(type == "mean_chsize") %>% 
    ggplot(aes(x = ch_region, y = value, group = ch_nursing, fill = ch_nursing)) +
    geom_bar(stat = "identity", position = position_dodge()) +
    geom_text(aes(label = value, y = value + 2), size = 3, color = "black", position = position_dodge(width = 1)) +
    scale_x_discrete(limits = rev(size_order)) +
    coord_flip() +
    theme_bw() +
    ylab("Average number of beds") +
    theme(axis.title.y = element_blank(),
          legend.position = "top",
          legend.title = element_blank(),
          legend.justification= c(1,0),
          panel.grid = element_blank()) +
    scale_fill_manual(values = c(THF_red, THF_50pct_light_blue), labels = c("Residential care homes", "Nursing homes")) +
    labs(title = "Mean size of care homes within regions in England", subtitle = "April 2020", caption = "Source: CQC")) %>% 
  ggsave("graphs/sprint_2/Care_home_size_by_region_and_type2.png", ., width = 6, height = 5)

summary_region_nursing %>% 
  filter(type == "mean_chsize") %>% 
  select(-type, mean_chsize = value) %>% 
  mutate(ch_nursing = ifelse(ch_nursing == 0, "residential", "nursing")) %>% 
  pivot_wider(names_from = "ch_nursing", values_from = "mean_chsize") %>% 
 write_csv("processed_data/CH_by_region_chnursing_meansize.csv")


# Visualise care home location  ------------------------------------------------------------------

# Care home number by region
(summary_region %>% 
    filter(type == "ch") %>% 
    mutate(pct = str_c(round(100* value / sum(value), 0), "%")) %>% 
    ggplot(aes(x = fct_reorder(ch_region, value), y = value)) +
    geom_bar(stat = "identity", position = position_dodge(), fill = THF_red, width = 0.8) +
    geom_text(aes(label = pct, y = value + 100), size = 2, color = "black") +
    theme_bw() +
    coord_flip() +
    theme(axis.title = element_blank(),
          legend.position = "top",
          legend.justification= c(1,0),
          panel.grid = element_blank()) +
    labs(title = "Number of care homes in England", subtitle = "April 2020", caption = "Source: CQC")) %>% 
  ggsave("graphs/sprint_2/Care_home_numbers_by_region.png", ., width = 5, height = 4)

# Number of homes and beds
(summary_region %>% 
  filter(type %in% c("resident_ch", "nursing_ch", "resident_beds", "nursing_beds")) %>% 
  mutate(type = factor(type, levels = c("resident_ch", "resident_beds", "nursing_ch", "nursing_beds")),
         type = fct_recode(type,  `Nursing homes` = "nursing_ch", `Nursing home beds` = "nursing_beds",
                           `Residential care homes` = "resident_ch" , `Residential care home beds` = "resident_beds")) %>% 
  group_by(type) %>% 
  mutate(pct = str_c(round(100* value / sum(value), 0), "%")) %>% 
  ggplot(aes(x = ch_region, y = value)) +
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
  ggsave("graphs/sprint_2/Care_home_numbers_beds_by_region.png", ., width = 9, height = 6)


# Visualise care home deprivation  ------------------------------------------------------------------


(summary_region_IMD %>% 
  filter(type %in% c("resident_ch", "nursing_ch", "resident_beds", "nursing_beds")) %>% 
  mutate(type = factor(type, levels = c("resident_ch", "resident_beds", "nursing_ch", "nursing_beds")),
         type = fct_recode(type,  `Nursing homes` = "nursing_ch", `Nursing home beds` = "nursing_beds",
                           `Residential care homes` = "resident_ch" , `Residential care home beds` = "resident_beds")) %>% 
  group_by(ch_region, type) %>% 
  mutate(pct = 100* value / sum(value)) %>% 
  ggplot(aes(x = ch_region, y = pct, fill = IMD_decile_fct)) +
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

# Quintiles, all care homes
summary_region_IMD_all <- summary_region_IMD %>% 
  filter(type == "ch") %>% 
  group_by(ch_region, IMD_quintile_fct) %>% 
  summarise(ch = sum(value)) %>% 
  group_by(ch_region) %>% 
  mutate(pct = 100* ch / sum(ch))

summary_region_IMD_all %>% 
  write_csv("processed_data/CH_by_region_IMDquintiles.csv")


IMDquintiles_regions <- summary_region_IMD_all[summary_region_IMD_all$IMD_quintile_fct == "1 most deprived",]
IMD_order <- IMDquintiles_regions$ch_region[order(IMDquintiles_regions$pct)] 

(summary_region_IMD_all %>% 
    ggplot(aes(x = ch_region, y = pct, fill = IMD_quintile_fct)) +
    geom_bar(stat = "identity", position = position_stack()) +
    scale_x_discrete(limits = rev(IMD_order)) +
    coord_flip() +
    theme_bw() +
    ylab("Care homes (%)") +
    scale_fill_manual(values = scales::seq_gradient_pal(THF_50pct_light_blue, THF_red)(seq(0, 1,length.out = 5)),
                      drop = FALSE) +
    guides(fill = guide_legend(nrow = 1, byrow = TRUE, reverse = TRUE)) +
    theme(axis.title.y = element_blank(),
          legend.position = "top",
          legend.justification= c(1,0),
          panel.grid.major.y = element_blank()) +
    labs(title = "Care homes by local area deprivation", 
         subtitle = "April 2020", caption = "Source: CQC",
         fill = "IMD quintile")) %>% 
  ggsave("graphs/sprint_2/Care_home_IMDquintiles_by_region.png", ., width = 6, height = 5)
