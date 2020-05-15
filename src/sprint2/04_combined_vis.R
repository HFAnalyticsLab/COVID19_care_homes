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

ch_deaths_region <- ch_deaths_region %>% 
  group_by(type, date) %>% 
  mutate(deaths_cum_pct = round(100 * deaths_cum/sum(deaths_cum), 1),
         region = gsub("Yorkshire and the Humber", "Yorkshire and The Humber", region),
         region = gsub("^East$", "East of England", region))


# care homes and beds split by region
summary_region <- read_csv("data/sprint_2/CH_summary_2020-04-01_ch_region.csv") %>% 
  mutate(ch_region = str_to_title(ch_region),
         ch_region = gsub("Yorkshire And Humber", "Yorkshire and The Humber", ch_region),
         ch_region = gsub(" Of England", " of England", ch_region))

# Combine
region_combined <- ch_deaths_region %>% 
  ungroup() %>% 
  filter(date == max(date)) %>% 
  select(-date, -deaths) %>% 
  pivot_wider(names_from = "type", values_from = c("deaths_cum", "deaths_cum_pct")) %>% 
  janitor::clean_names() %>% 
  left_join(summary_region %>% 
              filter(ch_region != "Unspecified"), 
            by = c("region" = "ch_region")) %>% 
  pivot_longer(-region, names_to = "var", values_to = "value")

# Show care home bed and all-cause death distribution

order_table <- region_combined[region_combined$var == "deaths_cum_pct_all_cause",]
region_order <- order_table$region[order(order_table$value)] 
 

(region_combined %>% 
  filter(var %in% c("deaths_cum_pct_all_cause", "percent_beds")) %>% 
  ggplot(aes(x = region, y = value, group = var, fill = var)) +
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
  labs(title = "Regional distribution of care home beds and\nall-cause deaths of residents", 
       subtitle = "Beds: April 2020, Deaths: 28 Dec, 2019 - May 1, 2020", 
       caption = "Source: CQC, ONS")) %>% 
  ggsave("graphs/sprint_2/Care_home_beds_allcause_deaths.png", ., width = 7, height = 5)

# Show care home bed and COVID death distribution

order_table_COVID <- region_combined[region_combined$var == "deaths_cum_pct_covid",]
region_order_COVID <- order_table_COVID$region[order(order_table_COVID$value)] 

(region_combined %>% 
    filter(var %in% c("deaths_cum_pct_covid", "percent_beds")) %>% 
    ggplot(aes(x = region, y = value, group = var, fill = var)) +
    geom_bar(stat = "identity", position = position_dodge(), width = 0.8) +
    scale_x_discrete(limits = region_order_COVID) +
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
    labs(title = "Regional distribution of care home beds and\nCOVID-19 deaths of residents", 
         subtitle = "Beds: April 2020, Deaths: 28 Dec, 2019 - May 1, 2020", 
         caption = "Source: CQC, ONS")) %>% 
  ggsave("graphs/sprint_2/Care_home_beds_COVID_deaths.png", ., width = 7, height = 5)


# all combined
(region_combined %>% 
    filter(var %in% c("percent_beds", "deaths_cum_pct_all_cause", "deaths_cum_pct_covid")) %>% 
    ggplot(aes(x = region, y = value, group = var, fill = var)) +
    geom_bar(stat = "identity", position = position_dodge(), width = 0.8) +
    scale_x_discrete(limits = region_order_COVID) +
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
    labs(title = "Regional distribution of care home beds and\ndeaths of residents", 
         subtitle = "Beds: April 2020, Deaths: 28 Dec, 2019 - May 1, 2020", 
         caption = "Source: CQC, ONS")) %>% 
    ggsave("graphs/sprint_2/Care_home_beds_allcause_COVID_deaths.png", ., width = 7, height = 5)

# maps showing deaths/bed

region_shape <- regions_json_df %>% 
  left_join(region_combined %>% 
              pivot_wider(names_from = "var", values_from = "value"), by = c("id" = "region"))

# Plot number of deaths and deaths per bed per region
(region_shape %>% 
  ggplot() + 
    geom_polygon(aes(x = long, y = lat, group = group, fill = deaths_cum_covid), 
                 colour = "grey80") +
    theme_void() +
    coord_map() +
    scale_fill_gradient(low = "white", high = THF_red) +
    labs(title = "Regional distribution of COVID-19 deaths\nin care home residents", 
         subtitle = "Beds: April 2020, Deaths: 28 Dec, 2019 - May 1, 2020", 
         caption = "Source: ONS")) %>% 
  ggsave("graphs/sprint_2/Care_home_COVID_deaths_map.png", ., width = 5, height = 5)

(region_shape %>% 
    ggplot() + 
    geom_polygon(aes(x = long, y = lat, group = group, fill = deaths_cum_covid), 
                 colour = "grey80") +
    theme_void() +
    coord_map() +
    scale_fill_gradient(low = "white", high = THF_red) +
    labs(title = "Regional distribution of COVID-19 deaths\nin care home residents", 
         subtitle = "Beds: April 2020, Deaths: 28 Dec, 2019 - May 1, 2020", 
         caption = "Source: ONS")) %>% 
  ggsave("graphs/sprint_2/Care_home_COVID_deaths_map.png", ., width = 5, height = 5)


(region_shape %>% 
    ggplot() +
    geom_polygon(aes(x = long, y = lat, group = group, fill = deaths_cum_all_cause/beds), 
                 colour = "grey80") +
    theme_void() +
    coord_map() +
    scale_fill_gradient(low = "white", high = THF_red) +
    labs(title = "Regional distribution of all-cause deaths\nin care home residents per bed", 
         subtitle = "Beds: April 2020, Deaths: 28 Dec, 2019 - May 1, 2020", 
         caption = "Source: ONS")) %>% 
  ggsave("graphs/sprint_2/Care_home_beds_allcause_deaths_map.png", ., width = 5, height = 5)


(region_shape %>% 
    ggplot() +
    geom_polygon(aes(x = long, y = lat, group = group, fill = deaths_cum_covid/beds), 
                 colour = "grey80") +
    theme_void() +
    coord_map() +
    scale_fill_gradient(low = "white", high = THF_red) +
    labs(title = "Regional distribution of COVID-19 deaths\nin care home residents per bed", 
         subtitle = "Beds: April 2020, Deaths: 28 Dec, 2019 - May 1, 2020", 
         caption = "Source: ONS")) %>% 
  ggsave("graphs/sprint_2/Care_home_beds_COVID_deaths_map.png", ., width = 5, height = 5)
