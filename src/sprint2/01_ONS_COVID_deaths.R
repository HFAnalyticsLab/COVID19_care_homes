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

# Shape file  -------------------------------------------------------------

# Upper tier local authority lookup file
UTLA_lookup_file <- read_csv("shapefiles/Lower_Tier_Local_Authority_to_Upper_Tier_Local_Authority_(April_2019)_Lookup_in_England_and_Wales.csv")%>% 
  filter(grepl("^E.*", LTLA19CD)) 

# LA
# https://data.gov.uk/dataset/e73c1990-ad5d-4184-8fb1-b474b94918ba/local-authority-districts-december-2019-boundaries-uk-buc

la_2019_json <- geojson_read("shapefiles/Local_Authority_Districts_(December_2019)_Boundaries_UK_BUC.geojson", what = "sp")
plot(la_2019_json)

la_lookup <- la_2019_json@data %>% 
  select(lad19cd, lad19nm)

# Filter for England only
la_2019_json_df <- tidy(la_2019_json, region = c("lad19cd")) %>% 
  filter(grepl("^E.*", id)) %>% 
  left_join(la_lookup, by = c("id" = "lad19cd")) %>% 
  left_join(UTLA_lookup_file, by = c("id" = "LTLA19CD", "lad19nm" = "LTLA19NM"))

utla_2019_json_df <- unionSpatialPolygons()

# Import data -------------------------------------------------------------


# Number of deaths in care homes notified to the Care Quality Commission, England
# https://www.ons.gov.uk/peoplepopulationandcommunity/birthsdeathsandmarriages/deaths/datasets/numberofdeathsincarehomesnotifiedtothecarequalitycommissionengland

# COVID mortality
ch_deaths_covid_eng <- read_xlsx("data/open/cqcwk17.xlsx", sheet = "Table 1", skip = 2, n_max = 22,
                                 col_types =  c("date", "numeric", "numeric")) %>% 
  rename(notification_date = 1, care_homes = 2, location_not_stated = 3) %>% 
  mutate(notification_date = as.Date(notification_date, format = "%Y-%m-%d"))

ch_deaths_covid_la <- read_xlsx("data/open/cqcwk17.xlsx", sheet = "Table 2", skip = 2, n_max = 150) %>% 
  setNames(., c('local_authority_name', format(as.Date(as.numeric(names(.)[-1]), 
                                     origin = '1899-12-30'), '%m-%d-%Y'))) %>% 
  pivot_longer(-local_authority_name, names_to = "date", values_to = "COVID_deaths") %>% 
  filter(local_authority_name != "England")

# All cause mortality
ch_deaths_allcause_la <- read_xlsx("data/open/cqcwk17.xlsx", sheet = "Table 3", skip = 2, n_max = 150) %>% 
  setNames(., c('local_authority_name', format(as.Date(as.numeric(names(.)[-1]), 
                                                       origin = '1899-12-30'), '%m-%d-%Y'))) %>% 
  pivot_longer(-local_authority_name, names_to = "date", values_to = "deaths")


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
  
ch_deaths_covid_la_cum_shape <- la_2019_json_df %>% 
  left_join(ch_deaths_covid_la_cum, by = c("lad19nm" = "local_authority_name"))

setdiff(sort(unique(la_2019_json_df$lad19nm)), sort(ch_deaths_covid_la_cum$local_authority_name))
setdiff(sort(ch_deaths_covid_la_cum$local_authority_name), sort(unique(la_2019_json_df$lad19nm)))
