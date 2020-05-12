####
# Descriptive analysis of current care home data
####

library(tidyverse)
library(tidylog)
library(janitor)
library(lubridate)
library(rlang)
library(data.table)


source("file_paths.R")

# cqc_carehome_latest.csv - list of care homes with characteristics over time, multiple rows per 
# care home when characteristics changed

# Functions ---------------------------------------------------------------

# Faster unnesting function
unnest_dt <- function(data, col){
  
  data <- as.data.table(data)
  col <- ensyms(col)
  
  columns <- syms(setdiff(colnames(data), as.character(col)))
  data <- eval(
    expr(data[, unlist(!!!col), by = list(!!!columns)])
  )
  
  colnames(data) <- c(as.character(columns), as.character(col))
  
  return(data)
}

# Convert to time series
create_ts <- function(data, month_from, month_to){
  
  data <- data %>% 
    mutate(month = map2(!!rlang::sym(month_from), !!rlang::sym(month_to), 
                      ~as.character(seq.Date(from = .x, to = .y, by = "month")))) %>% 
    unnest_dt(col = month)  %>% 
    as_tibble() %>% 
    mutate(month = as.Date(month))
  
  return(data)
}

# Characterise care homes split by one var
characterise_ch <- function(data, var){
  
  data_month <- unique(data$month)
  
  data %>% 
    group_by(!!rlang::sym(var)) %>% 
    summarise(ch = n(),
              resident_ch = sum(chnursing == 0),
              nursing_ch = sum(chnursing == 1),
              mean_chsize = round(mean(chnbeds), 1),
              median_chsize = median(chnbeds),
              sd_chsize = round(sd(chnbeds), 1),
              beds = sum(chnbeds),
              resident_beds = sum(chnbeds[chnursing == 0]),
              nursing_beds = sum(chnbeds[chnursing == 1])) %>% 
    ungroup() %>% 
    mutate(percent_ch = round(100*ch / sum(ch), 1),
           percent_resident_ch = round(100*resident_ch / sum(resident_ch), 1),
           percent_nursing_ch = round(100*nursing_ch / sum(nursing_ch), 1),
           percent_beds = round(100*beds / sum(beds), 1),
           percent_resident_beds = round(100*resident_beds / sum(resident_beds), 1),
           percent_nursing_beds = round(100*nursing_beds / sum(nursing_beds), 1)) %>% 
    arrange(desc(ch)) %>% 
    write_csv(str_c(results_path_sprint2, "CH_summary_", data_month, "_", var, ".csv"))

}




# Lookup files ------------------------------------------------------------

postcode_lookup <- read_csv(str_c(ref_data_path, "PCD_OA_LSOA_MSOA_LAD_FEB20_UK_LU.csv"))

imd_2019 <- read_csv(str_c(ref_data_path, "IMD_2019_LSOA.csv")) %>%
  select(starts_with('LSOA code'),
         contains('Index of Multiple Deprivation (IMD) Rank'),
         contains('Index of Multiple Deprivation (IMD) Decile')) 
colnames(imd_2019) <- c("LSOA", "IMD_rank", "IMD_decile")



# Load data ---------------------------------------------------------------

care_homes <- read_csv(str_c(hist_data_path, "cqc_carehome_latest.csv"), 
                     col_types = cols(chcharfrom = col_date(format = "%d/%m/%Y"),
                                      chcharto = col_date(format = "%d/%m/%Y"),
                                      chopen = col_date(format = "%d/%m/%Y"),
                                      chclose = col_date(format = "%d/%m/%Y")))

# Creating time series dataset --------------------------------------------


# Convert to time series by creating additional rows for each month
# between chcharfrom and chcharto (time period where characteristics are the same)
# chcharto and chcharfrom are already set to the first day of each month, no rounding needed

care_homes_ts <- care_homes %>%
  filter(is.na(chcharto) | chcharto >= chcharfrom) %>% 
  create_ts(month_from = "chcharfrom", month_to = "chcharto")


# Flag for specialist homes and for homes for older people
care_homes_ts <- care_homes_ts %>% 
  mutate(chspecialist = ifelse((chlearning == 1 | chdrug == 1 | cheating == 1 | chmha == 1 | chsensory == 1) &
                                 (chchild == 1 | chyadults == 1 | chwholepop == 1), 1, 0))


saveRDS(care_homes_ts, str_c(hist_data_path_Rds, 'carehome_2020_timeseries.Rds'))


# Snapshot of most recent data --------------------------------------------------------

care_homes_current <- care_homes_ts %>% 
  filter(month == max(care_homes_ts$month)) %>% 
  left_join(postcode_lookup, by = c("ch_postcode" = "pcds")) %>% 
  left_join(imd_2019, by = c("lsoa11cd" = "LSOA"))

c("chnursing", "ch_region", "chown", "chspecialist", "IMD_decile", "ladnm") %>% 
  map(., characterise_ch, data = care_homes_current)

# no grouping
characterise_ch(data = care_homes_current %>%  mutate(dummy = 1), var = "dummy")

# Region + chnursing
care_homes_current %>%
  unite(chregion_chnursing , c("ch_region", "chnursing"), remove = FALSE) %>% 
  characterise_ch(var = "chregion_chnursing")

# Region + chspecialist
care_homes_current %>%
  unite(chregion_chspecialist , c("ch_region", "chspecialist"), remove = FALSE) %>% 
  characterise_ch(var = "chregion_chspecialist")

# Region + IMD_decile
care_homes_current %>%
  unite(chregion_IMDdecile , c("ch_region", "IMD_decile"), remove = FALSE) %>% 
  characterise_ch(var = "chregion_IMDdecile")
