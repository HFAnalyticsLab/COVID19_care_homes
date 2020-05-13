####
## Visualise ONS data on COVID deaths in care homes
####

library(tidyverse) 
library(tidylog)
library(readxl)
library(janitor)

library(broom)
library(geojsonio)
library(maptools)

# Colors ------------------------------------------------------------------
THF_red <- '#dd0031'
THF_50pct_light_blue <- '#aad3e5'
THF_1_purple <- '#744284'

# Shape file  -------------------------------------------------------------

# Upper tier local authority shape file
# https://data.gov.uk/dataset/53831348-9733-4e52-b9e6-1ddd6be94535/counties-and-unitary-authorities-december-2019-boundaries-uk-buc

utla_2019_json <- geojson_read("shapefiles/Counties_and_Unitary_Authorities_(December_2019)_Boundaries_UK_BUC.geojson", what = "sp")
plot(utla_2019_json)

utla_lookup <- utla_2019_json@data %>% 
  select(ctyua19cd, ctyua19nm)

# Filter for England only
utla_2019_json_df <- tidy(utla_2019_json, region = c("ctyua19cd")) %>% 
  filter(grepl("^E.*", id)) %>% 
  left_join(utla_lookup, by = c("id" = "ctyua19cd")) 

# UTLA to region
region_lookup <- read_csv("shapefiles/Ward_to_Local_Authority_District_to_County_to_Region_to_Country_(December_2019)_Lookup_in_United_Kingdom.csv",
                          col_types = cols(
                            FID = col_double(),
                            WD19CD = col_character(),
                            WD19NM = col_character(),
                            LAD19CD = col_character(),
                            LAD19NM = col_character(),
                            CTY19CD = col_character(),
                            CTY19NM = col_character(),
                            RGN19CD = col_character(),
                            RGN19NM = col_character(),
                            CTRY19CD = col_character(),
                            CTRY19NM = col_character()
                          )) 

# Import data -------------------------------------------------------------


# Number of deaths in care homes notified to the Care Quality Commission, England
# https://www.ons.gov.uk/peoplepopulationandcommunity/birthsdeathsandmarriages/deaths/datasets/numberofdeathsincarehomesnotifiedtothecarequalitycommissionengland

file <- "data/open/20200510officialsensitivecoviddeathnotificationschdata20200508.xlsx"

# COVID mortality
ch_deaths_covid_eng <- read_xlsx(file, sheet = "Table 1", skip = 2,
                                 col_types =  c("date", "numeric", "numeric")) %>% 
  rename(notification_date = 1, care_homes = 2, location_not_stated = 3) %>% 
  filter(!is.na(notification_date)) %>% 
  mutate(notification_date = as.Date(notification_date, format = "%Y-%m-%d"))

# be careful with formatting: sometimes LAs dont't report, this messes up the number of rows to read in 
ch_deaths_covid_la <- read_xlsx(file, sheet = "Table 2", skip = 2, n_max = 150) %>% 
  setNames(., c('local_authority_name', format(as.Date(as.numeric(names(.)[-1]), 
                                     origin = '1899-12-30'), '%d-%m-%Y'))) %>% 
  pivot_longer(-local_authority_name, names_to = "date", values_to = "COVID_deaths") %>%
  mutate(date = as.Date(date, format = "%d-%m-%Y")) %>% 
  filter(local_authority_name != "England" & local_authority_name != "Notes:")

# All cause mortality
ch_deaths_allcause_la <- read_xlsx(file, sheet = "Table 3", skip = 2, n_max = 150) %>% 
  setNames(., c('local_authority_name', format(as.Date(as.numeric(names(.)[-1]), 
                                                       origin = '1899-12-30'), '%d-%m-%Y'))) %>% 
  pivot_longer(-local_authority_name, names_to = "date", values_to = "deaths")  %>% 
  mutate(date = as.Date(date, format = "%d-%m-%Y"))  

ch_dealths_allcause_eng <- ch_deaths_allcause_la %>% 
  filter(local_authority_name == "England") %>% 
  select(-local_authority_name)
  
ch_deaths_allcause_la <- ch_deaths_allcause_la %>% 
  filter(local_authority_name != "England")

# Line graphs - England ---------------------------------------------------

ch_deaths_eng <- ch_dealths_allcause_eng %>% 
  left_join(ch_deaths_covid_eng %>% mutate(location_not_stated = replace_na(location_not_stated, 0)), 
            by = c("date" = "notification_date")) %>% 
  rename(covid_deaths_care_homes = "care_homes", 
         covid_deaths_location_not_stated = "location_not_stated", 
         all_cause_deaths_care_homes = "deaths") 

(ch_deaths_eng %>% 
  pivot_longer(-date, names_to = "type", values_to = "deaths")  %>% 
  ggplot(aes(x = date, y = deaths, group = type, color = type)) +
  geom_line() +
  geom_point() +
  theme_bw() +
  scale_color_manual(values = c(THF_red, THF_50pct_light_blue, THF_1_purple),
                     labels = c("Deaths in care homes, all-cause", "Deaths in care homes, COVID", "Deaths with unknown location, COVID")) +
  theme(axis.title.x = element_blank(),
        legend.position = "top",
        legend.title = element_blank(),
        legend.justification= c(1,0),
        panel.grid = element_blank()) +
  guides(color = guide_legend(nrow = 2, byrow = TRUE)) +
  ylab("Number of deaths") +
  labs(title = "Daily deaths in care homes", 
       subtitle =  str_c("CQC data from ", min (ch_deaths_covid_la$date), " to ", 
                         max(ch_deaths_covid_la$date)),
       fill = "COVID deaths")) %>% 
  ggsave("graphs/sprint_2/Care_homes_deaths_England.png", ., width = 6, height = 5)

(ch_deaths_eng %>% 
    pivot_longer(-date, names_to = "type", values_to = "deaths")  %>% 
    group_by(type) %>% 
    arrange(date) %>% 
    mutate(deaths_cum = cumsum(deaths)) %>% 
    ggplot(aes(x = date, y = deaths_cum, group = type, color = type)) +
    geom_line() +
    geom_point() +
    theme_bw() +
    scale_color_manual(values = c(THF_red, THF_50pct_light_blue, THF_1_purple),
                       labels = c("Deaths in care homes, all-cause", "Deaths in care homes, COVID", "Deaths with unknown location, COVID")) +
    theme(axis.title.x = element_blank(),
          legend.position = "top",
          legend.title = element_blank(),
          legend.justification= c(1,0),
          panel.grid = element_blank()) +
    guides(color = guide_legend(nrow = 2, byrow = TRUE)) +
    ylab("Number of deaths") +
    labs(title = "Cumulative deaths in care homes", 
         subtitle =  str_c("CQC data from ", min (ch_deaths_covid_la$date), " to ", 
                           max(ch_deaths_covid_la$date)),
         fill = "COVID deaths")) %>% 
  ggsave("graphs/sprint_2/Care_homes_deaths_England_cumulative.png", ., width = 6, height = 5)



# Regional aggregates ---------------------------------------------------

ch_deaths_la <- ch_deaths_allcause_la %>% 
  left_join(ch_deaths_covid_la, by = c("local_authority_name", "date")) %>% 
  left_join(region_lookup[,c("LAD19NM", "RGN19CD", "RGN19NM")] %>% unique(), by = c("local_authority_name" = "LAD19NM"))  %>% 
  left_join(region_lookup[,c("CTY19NM", "RGN19CD", "RGN19NM")] %>% unique(), by = c("local_authority_name" = "CTY19NM")) %>% 
  mutate(RGN19NM = if_else(is.na(RGN19NM.x), RGN19NM.y, RGN19NM.x))

ch_deaths_region <- ch_deaths_la %>% 
  group_by(date, RGN19NM) %>% 
  summarise(deaths = sum(deaths, na.rm = TRUE),
            COVID_deaths = sum(COVID_deaths, na.rm = TRUE))

# Daily deaths
(ch_deaths_region %>% 
    pivot_longer(c(-date, - RGN19NM), names_to = "type", values_to = "deaths")  %>% 
    ggplot(aes(x = date, y = deaths, group = RGN19NM, color = RGN19NM)) +
    facet_wrap("type", ncol = 2) +
    geom_line() +
    geom_point() +
    theme_bw() +
    theme(axis.title.x = element_blank(),
          legend.position = "top",
          legend.title = element_blank(),
          legend.justification= c(1,0),
          panel.grid = element_blank()) +
    guides(color = guide_legend(nrow = 3, byrow = TRUE)) +
    ylab("Number of deaths") +
    labs(title = "Daily deaths in care homes", 
         subtitle =  str_c("CQC data from ", min (ch_deaths_covid_la$date), " to ", 
                           max(ch_deaths_covid_la$date)),
         fill = "COVID deaths")) %>% 
  ggsave("graphs/sprint_2/Care_homes_deaths_regions.png", ., width = 7, height = 5)

# Cumulative deaths

(ch_deaths_region %>% 
    pivot_longer(c(-date, - RGN19NM), names_to = "type", values_to = "deaths")  %>% 
    group_by(type, RGN19NM) %>% 
    arrange(date) %>% 
    mutate(deaths_cum = cumsum(deaths)) %>% 
    ggplot(aes(x = date, y = deaths_cum, group = RGN19NM, color = RGN19NM)) +
    facet_wrap("type", ncol = 2) +
    geom_line() +
    geom_point() +
    theme_bw() +
    theme(axis.title.x = element_blank(),
          legend.position = "top",
          legend.title = element_blank(),
          legend.justification= c(1,0),
          panel.grid = element_blank()) +
    guides(color = guide_legend(nrow = 3, byrow = TRUE)) +
    ylab("Number of deaths") +
    labs(title = "Cumulative deaths in care homes", 
         subtitle =  str_c("CQC data from ", min (ch_deaths_covid_la$date), " to ", 
                           max(ch_deaths_covid_la$date)),
         fill = "COVID deaths")) %>% 
  ggsave("graphs/sprint_2/Care_homes_deaths_regions_cumulative.png", ., width = 7, height = 5)


region_order <- c("London", "South East", "South West", "East of England", "East Midlands", "West Midlands",
                  "Yorkshire and The Humber",  "North East", "North West")

(ch_deaths_region %>% 
    pivot_longer(c(-date, - RGN19NM), names_to = "type", values_to = "deaths")  %>% 
    mutate(type = factor(type, levels = c("deaths", "COVID_deaths"))) %>% 
    group_by(RGN19NM, type) %>% 
    summarise(deaths = sum(deaths)) %>% 
    group_by(type) %>% 
    mutate(pct = str_c(round(100* deaths / sum(deaths), 0), "%"),
           ch_region_fct = factor(RGN19NM, levels = rev(region_order))) %>% 
    ggplot(aes(x = ch_region_fct, y = deaths)) +
    facet_wrap("type", ncol = 2, scales = "free_x", labeller = as_labeller(c("deaths" = "All-cause deaths",
                                                                             "COVID_deaths" = "Deaths related to COVID"))) +
    geom_bar(stat = "identity", position = position_dodge(), fill = THF_red) +
    geom_text(aes(label = pct, y = 0.9*deaths), size = 2, color = "white") +
    coord_flip() +
    theme_bw() +
    theme(axis.title = element_blank(),
          legend.position = "top",
          legend.justification= c(1,0),
          panel.grid.major.y = element_blank()) +
    labs(title = "Cumulative deaths in care homes, by region", 
         subtitle = str_c("CQC data from ", min (ch_deaths_covid_la$date), " to ", 
                          max(ch_deaths_covid_la$date)))) %>% 
    ggsave("graphs/sprint_2/Care_homes_deaths_by_region_bar.png", ., width = 6, height = 3)


# UTLA maps --------------------------------------------------------------------

# Care home COVID deaths to date
ch_deaths_covid_la_cum <- ch_deaths_covid_la  %>% 
  group_by(local_authority_name) %>% 
  summarise(COVID_deaths_cum = sum(COVID_deaths))

ch_deaths_covid_la_cum_shape <- utla_2019_json_df %>% 
  left_join(ch_deaths_covid_la_cum, by = c("ctyua19nm" = "local_authority_name"))

(ggplot() + 
    geom_polygon(data = ch_deaths_covid_la_cum_shape, 
                 aes(x = long, y = lat, group = group, fill = COVID_deaths_cum), 
                 colour = "grey80", size = 0.2) +
    theme_void() +
    coord_map() +
    scale_fill_gradient(low = "white", high = THF_red) +
    labs(title = "COVID deaths in care homes", 
         subtitle =  str_c(min (ch_deaths_covid_la$date), " to ", 
                           max(ch_deaths_covid_la$date), ", by Upper Tier Local Authority"),
         fill = "COVID deaths")) %>% 
  ggsave("graphs/sprint_2/Care_homes_deaths_COVID.png", ., width = 6, height = 6)

# Care homes all-cause deaths
ch_deaths_allcause_la_cum <- ch_deaths_allcause_la  %>% 
  group_by(local_authority_name) %>% 
  summarise(deaths_cum = sum(deaths)) 

ch_deaths_allcause_la_cum_shape <- utla_2019_json_df %>% 
  left_join(ch_deaths_allcause_la_cum, by = c("ctyua19nm" = "local_authority_name"))

(ggplot() + 
    geom_polygon(data = ch_deaths_allcause_la_cum_shape, 
                 aes(x = long, y = lat, group = group, fill = deaths_cum), 
                 colour = "grey80", size = 0.2) +
    theme_void() +
    coord_map() +
    scale_fill_gradient(low = "white", high = THF_red) +
    labs(title = "All-cause deaths in care homes", 
         subtitle =  str_c(min (ch_deaths_allcause_la$date), " to ", 
                           max(ch_deaths_allcause_la$date), ", by Upper Tier Local Authority"),
         fill = "All-cause deaths")) %>% 
  ggsave("graphs/sprint_2/Care_homes_deaths_all-cause.png", ., width = 6, height = 6)

# COVID deaths as fraction of all deaths

ch_deaths_la_cum <- ch_deaths_allcause_la_cum %>% 
  left_join(ch_deaths_covid_la_cum, by = "local_authority_name") %>% 
  mutate(pct_covid_deaths = 100 * COVID_deaths_cum / deaths_cum,
         pct_covid_deaths_cut = cut(pct_covid_deaths, 
                                    breaks = c(seq(0, 100, by = 20)),
                                    labels = str_c(c(0, seq(21, 81, by = 20)), 
                                                   "-", 
                                                   c(seq(20, 100, by = 20))),
                                    include.lowest = TRUE))

ch_deaths_la_cum_shape <- utla_2019_json_df %>% 
  left_join(ch_deaths_la_cum, by = c("ctyua19nm" = "local_authority_name"))

(ggplot() + 
    geom_polygon(data = ch_deaths_la_cum_shape, 
                 aes(x = long, y = lat, group = group, fill = fct_rev(pct_covid_deaths_cut)), 
                 colour = "grey80", size = 0.2) +
    theme_void() +
    coord_map() +
    scale_fill_manual(values = scales::seq_gradient_pal("white", THF_red, "Lab")(seq(1, 0, length.out = nlevels(ch_deaths_la_cum_shape$pct_covid_deaths_cut))),
                      drop = FALSE, na.value = "grey80") +
    labs(title = "Percentage of deaths in care homes involving COVID", 
         subtitle =  str_c(min (ch_deaths_allcause_la$date), " to ", 
                           max(ch_deaths_allcause_la$date), ", by Upper Tier Local Authority"),
         fill = "% of deaths")) %>% 
  ggsave("graphs/sprint_2/Care_homes_deaths_COVID_pct.png", ., width = 6, height = 6)



# Line graphs -------------------------------------------------------------

ch_deaths_covid_la %>% 
  group_by(local_authority_name) %>% 
  arrange(local_authority_name, date) %>% 
  mutate(COVID_deaths_cum = cumsum(COVID_deaths)) %>% 
  ggplot(aes(x = date, y = COVID_deaths_cum, group = local_authority_name,
             color = local_authority_name)) +
  geom_line() +
  theme_bw() +
  theme(legend.position = "none")

