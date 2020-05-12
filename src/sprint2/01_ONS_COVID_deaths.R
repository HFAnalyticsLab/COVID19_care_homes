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


# Maps --------------------------------------------------------------------

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

