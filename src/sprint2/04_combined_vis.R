####
# Combining data on care home locations and deaths
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


# Plotting ------------------------------------------------------------------
THF_red <- '#dd0031'
THF_50pct_light_blue <- '#aad3e5'
THF_1_purple <- '#744284'


# Shape files -------------------------------------------------------------

# Regions
# https://data.gov.uk/dataset/18991e29-872b-41e0-8fe0-1bb30d17aee8/regions-december-2016-ultra-generalised-clipped-boundaries-in-england

regions_json <- geojson_read("shapefiles/Regions_(December_2016)_Boundaries.geojson", what = "sp")
plot(regions_json)
regions_json_df <- tidy(regions_json, region = c("rgn16nm"))

# Import data  ------------------------------------------------------------

# regional aggregates of care home deaths produced in script 1
ch_deaths_region <- readRDS("processed_data/CH_deaths_by_region.Rds")

ch_deaths_region_cum <- ch_deaths_region %>% 
  group_by(RGN19NM) %>% 
  summarise(deaths = sum(deaths),
            COVID_deaths = sum(COVID_deaths)) %>% 
  ungroup() %>% 
  mutate(deaths_pct = round(100 * deaths/sum(deaths), 1),
         COVID_deaths_pct = round(100 * COVID_deaths/sum(COVID_deaths), 1))

# care homes and beds split by region
summary_region <- read_csv("data/sprint_2/CH_summary_2020-04-01_ch_region.csv") %>% 
  mutate(ch_region = str_to_title(ch_region),
         ch_region = gsub("Yorkshire And Humber", "Yorkshire and The Humber", ch_region),
         ch_region = gsub("Of", "of", ch_region))

# Combine
region_combined <- summary_region %>% 
  left_join(ch_deaths_region_cum, by = c("ch_region" = "RGN19NM")) %>% 
  filter(ch_region != "Unspecified") %>% 
  pivot_longer(-ch_region, names_to = "type", values_to = "value")


# Show care home bed and all-cause death distribution

order_table <- region_combined[region_combined$type == "deaths_pct",]
region_order <- order_table$ch_region[order(order_table$value)] 
 

(region_combined %>% 
  filter(type %in% c("percent_beds", "deaths_pct")) %>% 
  ggplot(aes(x = ch_region, y = value, group = type, fill = type)) +
  geom_bar(stat = "identity", position = position_dodge(), width = 0.8) +
  scale_x_discrete(limits = region_order) +
  theme_bw() +
  coord_flip() +
  ylab("Percent") +
  scale_fill_manual(values = c(THF_50pct_light_blue, THF_red),
                    labels = c("All-cause deaths", "Care home beds"), 
                    drop = FALSE) +
  theme(axis.title.y = element_blank(),
        legend.position = "top",
        legend.title = element_blank(),
        legend.justification= c(1,0),
        panel.grid = element_blank()) +
  labs(title = "Care home beds vs. all-cause deaths", 
       subtitle = "Beds: April 2020, Deaths: May 13, 2020", 
       caption = "Source: CQC, ONS")) %>% 
  ggsave("graphs/sprint_2/Care_home_beds_allcause_deaths.png", ., width = 7, height = 5)

# Show care home bed and COVID death distribution

order_table_COVID <- region_combined[region_combined$type == "COVID_deaths_pct",]
region_order_COVID <- order_table_COVID$ch_region[order_table_COVID(order_table$value)] 


(region_combined %>% 
    filter(type %in% c("percent_beds", "COVID_deaths_pct")) %>% 
    ggplot(aes(x = ch_region, y = value, group = type, fill = type)) +
    geom_bar(stat = "identity", position = position_dodge(), width = 0.8) +
    scale_x_discrete(limits = region_order) +
    theme_bw() +
    coord_flip() +
    ylab("Percent") +
    scale_fill_manual(values = c(THF_50pct_light_blue, THF_red),
                      labels = c("COVID deaths", "Care home beds"), 
                      drop = FALSE) +
    theme(axis.title.y = element_blank(),
          legend.position = "top",
          legend.title = element_blank(),
          legend.justification= c(1,0),
          panel.grid = element_blank()) +
    labs(title = "Care home beds vs. COVID deaths", 
         subtitle = "Beds: April 2020, Deaths: May 13, 2020", 
         caption = "Source: CQC, ONS")) %>% 
  ggsave("graphs/sprint_2/Care_home_beds_COVID_deaths.png", ., width = 7, height = 5)


# all combined
(region_combined %>% 
    filter(type %in% c("percent_beds", "deaths_pct", "COVID_deaths_pct")) %>% 
    ggplot(aes(x = ch_region, y = value, group = type, fill = type)) +
    geom_bar(stat = "identity", position = position_dodge(), width = 0.8) +
    scale_x_discrete(limits = region_order) +
    theme_bw() +
    coord_flip() +
    ylab("Percent") +
    scale_fill_manual(values = c(THF_1_purple, THF_50pct_light_blue, THF_red),
                      labels = c("COVID deaths", "All-cause deaths", "Care home beds"), 
                      drop = FALSE) +
    theme(axis.title.y = element_blank(),
          legend.position = "top",
          legend.title = element_blank(),
          legend.justification= c(1,0),
          panel.grid = element_blank()) +
    labs(title = "Care home beds vs. all-cause deaths", 
         subtitle = "Beds: April 2020, Deaths: Apr 10 - May 13", 
         caption = "Source: CQC, ONS")) %>% 
  ggsave("graphs/sprint_2/Care_home_beds_allcause_COVID_deaths.png", ., width = 7, height = 5)

# maps showing deaths/bed

RelDeath_region <- region_combined %>% 
  filter(type %in% c("beds", "deaths", "COVID_deaths")) %>% 
  pivot_wider(names_from = "type", values_from = "value") %>% 
  mutate(deaths_per_bed  = deaths / beds,
         COVID_deaths_per_bed = COVID_deaths / beds) 

RelDeath_region_shape <- regions_json_df %>% 
  left_join(RelDeath_region, by = c("id" = "ch_region"))

# Plot number of deaths and deaths per bed per region
(ggplot() + 
    geom_polygon(data = RelDeath_region_shape, 
                 aes(x = long, y = lat, group = group, fill = COVID_deaths), 
                 colour = "grey80") +
    theme_void() +
    coord_map() +
    scale_fill_gradient(low = "white", high = THF_red) +
    labs(title = "Deaths related to COVID", 
         subtitle =  "Apr 10 - May 13", 
         caption = "Source: ONS")) %>% 
  ggsave("graphs/sprint_2/Care_home_COVID_deaths_map.png", ., width = 5, height = 5)


(ggplot() + 
    geom_polygon(data = RelDeath_region_shape, 
                 aes(x = long, y = lat, group = group, fill = deaths_per_bed), 
                 colour = "grey80") +
    theme_void() +
    coord_map() +
    scale_fill_gradient(low = "white", high = THF_red) +
    labs(title = "Deaths per care home bed", 
         subtitle =  "Beds: April 2020, Deaths: Apr 10 - May 13", 
    caption = "Source: CQC, ONS")) %>% 
  ggsave("graphs/sprint_2/Care_home_beds_allcause_deaths_map.png", ., width = 5, height = 5)

(ggplot() + 
    geom_polygon(data = RelDeath_region_shape, 
                 aes(x = long, y = lat, group = group, fill = COVID_deaths_per_bed), 
                 colour = "grey80") +
    theme_void() +
    coord_map() +
    scale_fill_gradient(low = "white", high = THF_red) +
    labs(title = "COVID deaths per care home bed", 
         subtitle =  "Beds: April 2020, Deaths: Apr 10 - May 13", 
         caption = "Source: CQC, ONS")) %>% 
  ggsave("graphs/sprint_2/Care_home_beds_COVID_deaths_map.png", ., width = 5, height = 5)
