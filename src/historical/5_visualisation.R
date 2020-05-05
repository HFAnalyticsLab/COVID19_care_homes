####
# Descriptive analysis of historical data
# 5. Visulualisation
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

# Regions
# https://data.gov.uk/dataset/18991e29-872b-41e0-8fe0-1bb30d17aee8/regions-december-2016-ultra-generalised-clipped-boundaries-in-england
# LA
# https://data.gov.uk/dataset/45a1aaed-503a-4259-bd3e-27ce2ddc7b16/local-authority-districts-december-2016-super-generalised-clipped-boundaries-in-the-uk

regions_json <- geojson_read("shapefiles/Regions_(December_2016)_Boundaries.geojson", what = "sp")
plot(regions_json)
regions_json_df <- tidy(regions_json, region = c("rgn16nm"))

LA_json <- geojson_read("shapefiles/Local_Authority_Districts_(December_2016)_Boundaries_UK.geojson", what = "sp")
plot(LA_json)

# Filter for England only
LA_json_df <- tidy(LA_json, region = "lad16cd") %>% 
  filter(grepl("^E.*", id))


# Colors ------------------------------------------------------------------
THF_red <- '#dd0031'

# Deaths ------------------------------------------------------------------

residents_ts <- read_csv("data/time_series/CHR_since2014.csv")
deaths_ts <- read_csv("data/time_series/CHR_deaths_since2014.csv")

residents_ts <- residents_ts %>% 
  left_join(deaths_ts, by = c("month" = "deathdate_ym")) %>% 
  mutate(pct_deaths =  100* n_deaths/n)

# deaths over time
(residents_ts %>% 
    pivot_longer(cols = c("n", "n_deaths", "pct_deaths"), names_to = "type", values_to = "value") %>% 
    filter(month >= ymd("2014-09-01") & month <= ymd("2017-10-01")) %>% 
    ggplot(aes(x = month, y = value, group = type)) +
    geom_line() +
    geom_point() +
    facet_wrap("type", scales = "free_y", labeller = as_labeller(c("n" = "Residents",
                                                                   "n_deaths" = "Deaths",
                                                                   "pct_deaths" = "Deaths [%]"))) +
    ylab("") +
    theme_bw()) %>% 
  ggsave("graphs/Care_home_deaths.png", ., width = 8, height = 3)

# Crude look at pct deaths with mean and sd
(residents_ts %>% 
    filter(month >= ymd("2014-09-01") & month <= ymd("2017-10-01")) %>% 
    ggplot(aes(x = month, y = pct_deaths)) +
    geom_line() +
    geom_point() +
    ylab("Percent") +
    ggtitle("Deaths in care homes as % of the population") +
    theme_bw()+
    geom_hline(yintercept = mean(residents_ts$pct_deaths, na.rm =  TRUE), color = "red") +
    geom_ribbon(aes(ymin = mean(residents_ts$pct_deaths, na.rm = TRUE) - sd(residents_ts$pct_deaths, na.rm = TRUE),
                    ymax = mean(residents_ts$pct_deaths, na.rm = TRUE) + sd(residents_ts$pct_deaths, na.rm = TRUE)),
                fill = "red", alpha = 0.3) +
    annotate("text", label = "mean +/- sd", color = "red", x = ymd("2014-12-15"), y = 4.3, size = 3)) %>% 
  ggsave("graphs/Care_home_pct_deaths.png", ., width = 5, height = 4)



# Care home size REGIONAL -----------------------------------------------------
# residents per care home 

CHsize_region <- read_csv("data/snapshot_Oct2017/CHR_per_CH_2017-10_summary_chregion_SDC.csv")

CHsize_region_shape <- regions_json_df %>% 
  left_join(CHsize_region, by = c("id" = "chregion"))

CHsize_region_shape <- CHsize_region_shape %>%
  mutate(n_ch_dec = cut(n_ch, breaks = 10, dig.lab = 5),
         mean_chr_per_ch_dec = cut(mean_chr_per_ch, breaks = 10, dig.lab = 5),
         mean_chr_per_ch_residential_dec = cut(mean_chr_per_ch_residential, breaks = 10, dig.lab = 5),
         mean_chr_per_ch_nursing_dec = cut(mean_chr_per_ch_nursing, breaks = 10, dig.lab = 5)) %>% 
  mutate(id = factor(id, levels = unique(id)))

# Plot number of care homes per region
(ggplot() + 
  geom_polygon(data = CHsize_region_shape, 
               aes(x = long, y = lat, group = group, fill = n_ch_dec), 
               colour = "grey80") +
  theme_void() +
  coord_map() +
  scale_fill_manual(values = scales::seq_gradient_pal("white", THF_red, "Lab")(seq(0, 1,length.out = 10)),
                    drop = FALSE) +
  labs(title = "Number of care homes per region", subtitle =  "Oct 2017, based on residents, split into deciles")) %>% 
  ggsave("graphs/Care_homes_region.png", ., width = 6, height = 6)

(ggplot() + 
    geom_polygon(data = CHsize_region_shape, 
                 aes(x = long, y = lat, group = group, fill = mean_chr_per_ch_dec), 
                 colour = "grey80") +
    theme_void() +
    coord_map() +
    scale_fill_manual(values = scales::seq_gradient_pal("white", THF_red, "Lab")(seq(0, 1,length.out = 10)),
                      drop = FALSE) +
    labs(title = "Mean number of residents per care home", subtitle =  "Oct 2017, split into deciles")) %>% 
  ggsave("graphs/CHR_per_CH_region.png", ., width = 6, height = 6)

(ggplot() + 
    geom_polygon(data = CHsize_region_shape, 
                 aes(x = long, y = lat, group = group, fill = mean_chr_per_ch_residential_dec), 
                 colour = "grey80") +
    theme_void() +
    coord_map() +
    scale_fill_manual(values = scales::seq_gradient_pal("white", THF_red, "Lab")(seq(0, 1,length.out = 10)),
                      drop = FALSE) +
    labs(title = "Mean number of residents per RESIDENTIAL care home", subtitle =  "Oct 2017, split into deciles")) %>% 
  ggsave("graphs/CHR_per_CH_residential_region.png", ., width = 6, height = 6)

(ggplot() + 
    geom_polygon(data = CHsize_region_shape, 
                 aes(x = long, y = lat, group = group, fill = mean_chr_per_ch_nursing_dec), 
                 colour = "grey80") +
    theme_void() +
    coord_map() +
    scale_fill_manual(values = scales::seq_gradient_pal("white", THF_red, "Lab")(seq(0, 1,length.out = 10)),
                      drop = FALSE) +
    labs(title = "Mean number of residents per NURSING home", subtitle =  "Oct 2017, split into deciles")) %>% 
  ggsave("graphs/CHR_per_CH_nursing_region.png", ., width = 6, height = 6)


# Care home size LOCAL AUTHORITY -----------------------------------------------------
# residents per care home 

CHsize_LA <- read_csv("data/snapshot_Oct2017/CHR_per_CH_2017-10_summary_LA_SDC.csv")

# Need to manually update some LA identifiers (DOUBLE CHECK THIS)

CHsize_LA <- CHsize_LA %>% 
  mutate(la11pidlsoa_updated = case_when(la11pidlsoa == "E06000048" ~ "E06000057",
                                         la11pidlsoa == "E07000097" ~ "E07000242",
                                         la11pidlsoa == "E07000100" ~ "E07000240",
                                         la11pidlsoa == "E07000101" ~ "E07000243",
                                         la11pidlsoa == "E07000104" ~ "E07000241",
                                         la11pidlsoa == "E08000020" ~ "E08000037",
                                         TRUE ~ la11pidlsoa))

CHsize_LA_shape <- LA_json_df %>% 
  left_join(CHsize_LA, by = c("id" = "la11pidlsoa_updated"))

setdiff(CHsize_LA$la11pidlsoa_updated, unique(LA_json_df$id))

CHsize_LA_shape <- CHsize_LA_shape %>%
  mutate(n_ch_dec = cut(n_ch, breaks = 10, dig.lab = 5),
         mean_chr_per_ch_dec = cut(mean_chr_per_ch, breaks = 10, dig.lab = 5),
         mean_chr_per_ch_residential_dec = cut(mean_chr_per_ch_residential, breaks = 10, dig.lab = 5),
         mean_chr_per_ch_nursing_dec = cut(mean_chr_per_ch_nursing, breaks = 10, dig.lab = 5)) %>% 
  mutate(id = factor(id, levels = unique(id)))

# Plot number of care homes per LA
(ggplot() + 
    geom_polygon(data = CHsize_LA_shape, 
                 aes(x = long, y = lat, group = group, fill = n_ch_dec), 
                 colour = "grey80") +
    theme_void() +
    coord_map() +
    scale_fill_manual(values = scales::seq_gradient_pal("white", THF_red, "Lab")(seq(0, 1,length.out = 10)),
                      drop = FALSE, na.value = 'grey80') +
    labs(title = "Number of care homes per LA", subtitle =  "Oct 2017, based on residents, split into deciles")) %>% 
  ggsave("graphs/Care_homes_LA.png", ., width = 6, height = 6)

(ggplot() + 
    geom_polygon(data = CHsize_LA_shape, 
                 aes(x = long, y = lat, group = group, fill = mean_chr_per_ch_dec), 
                 colour = "grey80") +
    theme_void() +
    coord_map() +
    scale_fill_manual(values = scales::seq_gradient_pal("white", THF_red, "Lab")(seq(0, 1,length.out = 10)),
                      drop = FALSE, na.value = 'grey80') +
    labs(title = "Mean number of residents per care home", subtitle =  "Oct 2017, split into deciles")) %>% 
  ggsave("graphs/CHR_per_CH_LA.png", ., width = 6, height = 6)

(ggplot() + 
    geom_polygon(data = CHsize_LA_shape, 
                 aes(x = long, y = lat, group = group, fill = mean_chr_per_ch_residential_dec), 
                 colour = "grey80") +
    theme_void() +
    coord_map() +
    scale_fill_manual(values = scales::seq_gradient_pal("white", THF_red, "Lab")(seq(0, 1,length.out = 10)),
                      drop = FALSE, na.value = 'grey80') +
    labs(title = "Mean number of residents per RESIDENTIAL care home", subtitle =  "Oct 2017, split into deciles")) %>% 
  ggsave("graphs/CHR_per_CH_residential_LA.png", ., width = 6, height = 6)

(ggplot() + 
    geom_polygon(data = CHsize_LA_shape, 
                 aes(x = long, y = lat, group = group, fill = mean_chr_per_ch_nursing_dec), 
                 colour = "grey80") +
    theme_void() +
    coord_map() +
    scale_fill_manual(values = scales::seq_gradient_pal("white", THF_red, "Lab")(seq(0, 1,length.out = 10)),
                      drop = FALSE, na.value = 'grey80') +
    labs(title = "Mean number of residents per NURSING home", subtitle =  "Oct 2017, split into deciles")) %>% 
  ggsave("graphs/CHR_per_CH_nursing_LA.png", ., width = 6, height = 6)



# Regional stats ----------------------------------------------------------

CH_region <- read_csv("data/snapshot_Oct2017/CH_2017-10_summary_chregion.csv")

CHR_region <- read_csv("data/snapshot_Oct2017/CHR_2017-10_summary_chregion_SDC.csv") %>% 
  filter(chregion != "Unspecified") %>% 
  mutate_at(vars(chr_charlson_copd, chr_charlson_diab, chr_charlson_canc), as.numeric)

regional_summary <- CH_region %>% 
  inner_join(CHR_region, by = "chregion")

regional_summary_shape <- regions_json_df %>% 
  left_join(regional_summary, by = c("id" = "chregion"))

# Prevalence of conditions among residents
plot_region_prev_map <- function(data, col, condition){

  (ggplot() + 
     geom_polygon(data = data, 
                  aes(x = long, y = lat, group = group, fill = !!rlang::sym(col)), 
                  colour = "grey80") +
     theme_void() +
     coord_map() +
     scale_fill_gradient(low = "white", high = THF_red) +
     theme(legend.title = element_blank()) +
     labs(title = condition, subtitle =  "Prevalence among care home residents in Oct 2017,\nbased on Charlson index")) %>% 
    ggsave(str_c("graphs/Region_", condition, "_prev.png"), ., width = 4, height = 4)
}

walk2(c("pct_chr_charlson_dementia", "pct_chr_charlson_copd", "pct_chr_charlson_chf", "pct_chr_charlson_diab", "pct_chr_charlson_canc"),
      c("Dementia", "COPD", "CHF", "Diabetes", "Cancer"), 
      ~plot_region_prev_map(data = regional_summary_shape, col = .x, condition = .y))

# Number of residents with conditions 
plot_region_rwithcond_map <- function(data, col, condition){
  
  (ggplot() + 
     geom_polygon(data = data, 
                  aes(x = long, y = lat, group = group, fill = !!rlang::sym(col)), 
                  colour = "grey80") +
     theme_void() +
     coord_map() +
     scale_fill_gradient(low = "white", high = THF_red) +
     theme(legend.title = element_blank()) +
     labs(title = condition, subtitle =  "Number of care home residents with condition in Oct 2017,\nbased on Charlson index")) %>% 
    ggsave(str_c("graphs/Region_", condition, "_nchr.png"), ., width = 5, height = 4)
}

walk2(c("chr_charlson_dementia", "chr_charlson_copd", "chr_charlson_chf", "chr_charlson_diab", "chr_charlson_canc"),
      c("Dementia", "COPD", "CHF", "Diabetes", "Cancer"), 
      ~plot_region_rwithcond_map(data = regional_summary_shape, col = .x, condition = .y))


# Number of residents
(ggplot() + 
    geom_polygon(data = regional_summary_shape, 
                 aes(x = long, y = lat, group = group, fill = chr), 
                 colour = "grey80") +
    theme_void() +
    coord_map() +
    scale_fill_gradient(low = "white", high = THF_red) +
    labs(title = "Number of care home residents per region", subtitle =  "Oct 2017")) %>% 
  ggsave("graphs/Care_home_residents_region.png", ., width = 6, height = 6)

(ggplot() + 
    geom_polygon(data = regional_summary_shape, 
                 aes(x = long, y = lat, group = group, fill = resident_chr), 
                 colour = "grey80") +
    theme_void() +
    coord_map() +
    scale_fill_gradient(low = "white", high = THF_red) +
    labs(title = "Number of care home residents per region", subtitle =  "Oct 2017")) %>% 
  ggsave("graphs/Care_home_residents_nursing_region.png", ., width = 6, height = 6)

(ggplot() + 
    geom_polygon(data = regional_summary_shape, 
                 aes(x = long, y = lat, group = group, fill = nursing_chr), 
                 colour = "grey80") +
    theme_void() +
    coord_map() +
    scale_fill_gradient(low = "white", high = THF_red) +
    labs(title = "Number of nursing home residents per region", subtitle =  "Oct 2017")) %>% 
  ggsave("graphs/Care_home_residents_residential_region.png", ., width = 6, height = 6)


# Deprivation
region_order <- regional_summary$chregion[order(regional_summary$pct_chr_IMD_Q1)]
(regional_summary %>% 
  select(chregion, pct_chr_IMD_Q1:pct_chr_IMD_Q5) %>% 
  pivot_longer(-chregion, names_to = "IMD_quintile", values_to = "pct") %>% 
  mutate(IMD_quintile = gsub("pct_chr_IMD_", "", IMD_quintile)) %>% 
  ggplot(aes(x = factor(chregion,region_order), y = pct, fill = IMD_quintile)) +
  geom_bar(stat = "identity", position = position_stack()) +
  coord_flip() +
  theme_bw() +
  ylab("% of the care home population") +
  labs(title = "Socioeconomic deprivation", subtitle = "Q1 = most deprived") +  
  theme(axis.title.y = element_blank(),
        panel.grid = element_blank(),
        legend.position = "bottom") +
  scale_fill_manual(values = scales::seq_gradient_pal(low = "grey90", high = THF_red)(seq(1, 0, length.out = 5))))  %>% 
  ggsave("graphs/Region_CHR_IMD.png", ., width = 4.5, height = 4)


# LA stats ----------------------------------------------------------


CH_LA <- read_csv("data/snapshot_Oct2017/CH_2017-10_summary_la11.csv")

# round censored values (just remove < or >), recalculate percentages

CHR_LA <- read_csv("data/snapshot_Oct2017/CHR_2017-10_summary_la11pidlsoa_SDC.csv") %>% 
  mutate_if(is.character, function(x) gsub("<|>", "", x)) %>% 
  mutate_at(vars(resident_chr, nursing_chr, chr_charlson_copd, chr_charlson_chf, 
                 chr_charlson_canc, chr_IMD_Q1, chr_IMD_Q2, chr_IMD_Q3, chr_IMD_Q4, chr_IMD_Q5,
                 pct_resident_chr, pct_nursing_chr), as.numeric) %>% 
  mutate(pct_chr_charlson_dementia = round(100*chr_charlson_dementia / chr, 1),
         pct_chr_charlson_copd = round(100*chr_charlson_copd / chr, 1),
         pct_chr_charlson_chf = round(100*chr_charlson_chf / chr, 1),
         pct_chr_charlson_diab = round(100*chr_charlson_diab / chr, 1),
         pct_chr_charlson_canc = round(100*chr_charlson_canc / chr, 1),
         pct_chr_IMD_Q1 = round(100*chr_IMD_Q1 / chr, 1),
         pct_chr_IMD_Q2 = round(100*chr_IMD_Q2 / chr, 1),
         pct_chr_IMD_Q3 = round(100*chr_IMD_Q3 / chr, 1),
         pct_chr_IMD_Q4 = round(100*chr_IMD_Q4 / chr, 1),
         pct_chr_IMD_Q5 = round(100*chr_IMD_Q5 / chr, 1)) 

LA_summary <- CH_LA %>% 
  inner_join(CHR_LA, by = c("la11" = "la11pidlsoa")) %>% 
  mutate(la11_updated = case_when(la11 == "E06000048" ~ "E06000057",
                                  la11 == "E07000097" ~ "E07000242",
                                  la11 == "E07000100" ~ "E07000240",
                                  la11 == "E07000101" ~ "E07000243",
                                  la11 == "E07000104" ~ "E07000241",
                                  la11 == "E08000020" ~ "E08000037",
                                         TRUE ~ la11))

LA_summary_shape <- LA_json_df %>% 
  left_join(LA_summary, by = c("id" = "la11_updated"))

setdiff(LA_summary$la11_updated, unique(LA_json_df$id))


# Prevalence of conditions among residents
plot_LA_prev_map <- function(data, col, condition){
  
  (ggplot() + 
     geom_polygon(data = data, 
                  aes(x = long, y = lat, group = group, fill = !!rlang::sym(col)), 
                  colour = "grey80", size = 0.2) +
     theme_void() +
     coord_map() +
     scale_fill_gradient(low = "white", high = THF_red) +
     theme(legend.title = element_blank()) +
     labs(title = condition, subtitle =  "Prevalence among care home residents in Oct 2017,\nbased on Charlson index")) %>% 
    ggsave(str_c("graphs/LA_", condition, "_prev.png"), ., width = 5, height = 4)
}

walk2(c("pct_chr_charlson_dementia", "pct_chr_charlson_copd", "pct_chr_charlson_chf", "pct_chr_charlson_diab", "pct_chr_charlson_canc"),
      c("Dementia", "COPD", "CHF", "Diabetes", "Cancer"), 
      ~plot_LA_prev_map(data = LA_summary_shape, col = .x, condition = .y))
