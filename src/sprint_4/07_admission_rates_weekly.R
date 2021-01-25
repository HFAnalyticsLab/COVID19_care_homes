####
### Calculate weekly admission rates
####

library(tidyverse)
library(tidylog)
library(janitor)
library(lubridate)
library(ggrepel)

source("file_paths.R")


# Import data -------------------------------------------------------------

chres_denom_apcs <- readRDS(str_c(Rds_data_path, 'sprint_4/analysis_ds_weekly.Rds'))

# elod is based on patient classification
# this includes ordinary admissions and day cases (not regular attenders)

# Rates: Residential vs nursing homes --------------------------------------------

# Calculate annual admission rate per person (weekly)
# This is done by summing up the total number of admissions for all care home residents per week
# and dividing by the number of days spent in a care home across all residents in that same week
# Finally, this number is multiplied by 365 to provide the rate per person per year

rate_chnursing <- chres_denom_apcs %>%  
  mutate(elective_not_elod = ifelse(!is.na(elod), elective - elod, elective)) %>% 
  group_by(cohort_year, ch_nursing, week, weekstart) %>% 
  summarise_at(vars(c("admissions", "emergency", "elod", "emergency_avoidable", "emergency_covid_prim", "emergency_noncovid",
                      "days_ch")), sum, na.rm = TRUE) %>% 
  pivot_longer(c(-cohort_year, -week, -weekstart, -days_ch, -ch_nursing), names_to = "type", values_to = "count") %>%
  mutate(rate_daily = count/days_ch,
         rate_annual = 365*rate_daily, 
         weekstart_dummy = `year<-`(weekstart, 0004)) %>%
  filter(!(weekstart == ymd("2019-01-14") | weekstart == ymd("2020-01-13") |
             weekstart == ymd("2020-06-29"))) %>%  
  arrange(type)

write_csv(rate_chnursing, str_c(results_path_sprint4, "adm_rates/Adm_rates_chrs_chnursing.csv"))


for (i in unique(rate_chnursing$type)){
  
  if(i == "emergency_covid_prim"){
    
    data <- rate_chnursing %>%  filter(cohort_year == 2020 
                                       & count > 10)
    
    (data %>%  
        filter(type == i) %>% 
        ggplot(aes(x = weekstart_dummy, y = rate_annual, color = factor(ch_nursing))) +
        geom_smooth(se = FALSE, color = "black", size = 0.25) +
        geom_point() +
        geom_vline(xintercept = c(ymd("0004-03-23"), ymd("0004-04-29")), linetype = "dashed", alpha = 0.3) +
        geom_label_repel(data = data %>%  
                           group_by(ch_nursing) %>% 
                           filter(type == i) %>% 
                           filter(weekstart == min(weekstart_dummy)),
                         aes(label = str_c("w/c ", format(weekstart_dummy, "%d %b"))), show.legend = FALSE, 
                         color = "grey33",
                         segment.size = 0.25, segment.linetype = 2,
                         segment.colour = "grey", nudge_x = 35, 
                         direction = "y", size = 3, force = 5, fill = "white") +
        theme_bw() +
        facet_wrap("ch_nursing", labeller = as_labeller(c(`1` = "nursing home",
                                                          `0` = "residential care home"))) +
        scale_x_date( date_labels = "%b", date_breaks = "1 month") +
        theme(legend.position = "top",
              legend.justification = c(1,0),
              panel.grid = element_blank(),
              legend.title = element_blank(),
              legend.background = element_blank(),
              legend.margin = margin(c(0,0,0,0)),
              strip.background = element_blank(),
              axis.title.x = element_blank()) +
        expand_limits(x = c(ymd("0004-01-20", "0004-06-24"))) +
        ylab("Admission rate per person per year") ) %>% 
      ggsave(str_c(results_path_sprint4, "adm_rates/Adm_rates_chrs_chnursing_", i ,".png"), ., device = "png", 
             dpi = 600, width = 3, height = 6.5)
    
  }else{
  
  (rate_chnursing %>%  
      filter(type == i) %>% 
      ggplot(aes(x = weekstart_dummy, y = rate_annual, group = factor(cohort_year), color = factor(cohort_year))) +
      geom_smooth(se = FALSE, color = "black", size = 0.25) +
      geom_point() +
      geom_vline(xintercept = c(ymd("0004-03-23"), ymd("0004-04-29")), linetype = "dashed", alpha = 0.3) +
      geom_label_repel(data = rate_chnursing %>%  
                   group_by(ch_nursing, cohort_year) %>% 
                   filter(type == i & weekstart == min(weekstart_dummy)),
                 aes(label = str_c("w/c ", format(weekstart_dummy, "%d %b"))), show.legend = FALSE, color = "grey33",
                 segment.size = 0.25, segment.linetype = 2,
                 segment.colour = "grey", nudge_y = max(rate_chnursing$rate_annual[rate_chnursing$type == i])*1.4, 
                 direction = "x", size = 3, force = 5, fill = "white") +
      theme_bw() +
      facet_wrap("ch_nursing", labeller = as_labeller(c(`1` = "nursing home",
                                                      `0` = "residential care home"))) +
      scale_x_date(date_labels = "%b", date_breaks = "1 month") +
      theme(legend.position = "top",
            legend.justification = c(1,0),
            panel.grid = element_blank(),
            legend.title = element_blank(),
            legend.background = element_blank(),
            legend.margin = margin(c(0,0,0,0)),
            strip.background = element_blank(),
            axis.title.x = element_blank()) +
      expand_limits(y=c(0, max(rate_chnursing$rate_annual[rate_chnursing$type == i])*1.15)) +
      ylab("Admission rate per person per year") ) %>% 
    ggsave(str_c(results_path_sprint4, "adm_rates/Adm_rates_chrs_chnursing_", i ,".png"), ., device = "png", 
           dpi = 600, width = 3, height = 6.5)
  
}}
  


(rate_chnursing %>%  
    ggplot(aes(x = weekstart_dummy, y = count, color = factor(cohort_year), shape = factor(ch_nursing))) +
    geom_line() +
    geom_point() +
    theme_bw() +
    scale_x_date(name = "Week starting",
                 date_labels = "%b", date_breaks = "1 month") +
    facet_wrap("type", scales = "free_y") + 
    theme(legend.position = "top",
          legend.justification = c(1,0),
          legend.title = element_blank(),
          strip.background = element_blank()) +
    expand_limits(y=0) +
    ylab("Number of admissions") +
    labs(title = "Hospital admissions from care homes")) %>% 
  ggsave(str_c(results_path_sprint4, "adm_rates/Adm_counts_chrs_chnursing.png"), ., device = "png", dpi = 600)
