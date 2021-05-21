
library(tidyverse)
library(tidylog)
library(janitor)
library(lubridate)
library(rlang)
library(data.table)

## Functions used in several sprints

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


# -------------------------------------------------------------------------

# Count the number of months covered by a time period defined by start and end date
# starting at date1 and ending at date 2 (inclusive, result is rounded to whole months)
# Returns a double
# returns 0 if one or both of the dates is missing, or if the period ends before it starts
count_months_covered <- function(period_start, period_end){
  map2_dbl(period_start, period_end, 
           ~ ifelse(is.na(.x) | is.na(.y) | .y < .x, 0, length(seq.Date(from = .x, to = .y, by = "month")))
  )
}

# Count the number of months missing between the time period defined by end date
# of first period and start date of second period (exclusive of start and end, result is rounded to whole months)
# # Returns a double
# returns 0 if one or both of the dates is missing, or if the first period ends before the second period starts
count_months_missing <- function(period1_end, period2_start){
  map2_dbl(period1_end, period2_start, 
           ~ ifelse(is.na(.x) | is.na(.y) | .y <= .x, 0, length(seq.Date(from = .x, to = .y, by = "month")) -2)
  )
}

# Count the overlap between time periods in months
# time period defined by end date
# of first period and start date of second period (exclusive of start and end, result is rounded to whole months)
# Returns a double
# returns 0 if one or both of the dates is missing, or if the first period ends before the second period starts
count_months_overlap <- function(period1_end, period2_start){
  map2_dbl(period1_end, period2_start, 
           ~ ifelse(is.na(.x) | is.na(.y) | .y > .x, 0, length(seq.Date(from = .y, to = .x, by = "month")))
  )
}

# Shift a date forwards by a number of months
# Requires the start date of the time period and the number of months to shift it by
# Returns a date
shift_date_forward <- function(date, months){
    date %m+% period(str_c(months, " month"))
}

# Shift a date backwards by a number of months
# Requires the start date of the time period and the number of months to shift it by
# Returns a date
shift_date_backwards <- function(date, months){
  date %m-% period(str_c(months, " month"))
}

# Count the overlap between time periods in days
# time period defined by end date
# of first period and start date of second period (exclusive of start and end)
# Returns a double
# returns 0 if one or both of the dates is missing, or if the first period ends before the second period starts
count_days_overlap <- function(period1_end, period2_start){
  map2_dbl(period1_end, period2_start, 
           ~ ifelse(is.na(.x) | is.na(.y) | .y > .x, 0, length(seq.Date(from = .y, to = .x, by = "days")))
  )
}


# rolling averages --------------------------------------------------------


sevendayavg <- function(col, rounding_digits = 0){
  
  rollapply(col, 7,
            mean, align = "right", partial = FALSE, fill = NA) %>% 
    round(rounding_digits)
}


# Categorising admissions -------------------------------------------------

detect_codes <- function(x, codes){
  return(str_detect(replace_na(x, ""), str_c(codes, collapse = "|")))
}

# Create main categories of ICD10 codes
# Requires a dataframe, a column with ICD 10 codes.
# Returns a modified dataframe.

categorise_ICD10 <- function(data, code, new_col){
  
  data <- data %>% 
    mutate(clean_code := toupper(!!rlang::sym(code)),
           clean_code = gsub("[[:punct:]]", "", clean_code),
           clean_code = gsub(" ", "", clean_code))
  
  data <- data %>%
    mutate(!!new_col := case_when(detect_codes(clean_code, c("A.*", "B.*")) ~ 1,
                                  detect_codes(clean_code, c("C.*", "D[0|1|2|3|4].*")) ~ 2,
                                  detect_codes(clean_code, c("D[5|6|7|8].*"))~ 3,
                                  detect_codes(clean_code, c("E.*")) ~ 4,
                                  detect_codes(clean_code, c("F.*")) ~ 5,
                                  detect_codes(clean_code, c("G.*")) ~ 6,
                                  detect_codes(clean_code, c("H[0|1|2|3|4|5].*")) ~ 7, 
                                  detect_codes(clean_code, c("H[6|7|8|9].*")) ~ 8,
                                  detect_codes(clean_code, c("I.*")) ~ 9,
                                  detect_codes(clean_code, c("J.*")) ~ 10,
                                  detect_codes(clean_code, c("K.*")) ~ 11,
                                  detect_codes(clean_code, c("L.*")) ~ 12,
                                  detect_codes(clean_code, c("M.*")) ~ 13,
                                  detect_codes(clean_code, c("N.*")) ~ 14,
                                  detect_codes(clean_code, c("O.*")) ~ 15,
                                  detect_codes(clean_code, c("P.*")) ~ 16,
                                  detect_codes(clean_code, c("Q.*")) ~ 17,
                                  detect_codes(clean_code, c("R.*")) ~ 18,
                                  detect_codes(clean_code, c("S.*", "T.*")) ~ 19,
                                  detect_codes(clean_code, c("V.*", "W.*", "X.*", "Y.*")) ~ 20,
                                  detect_codes(clean_code, c("Z.*")) ~ 21,
                                  detect_codes(clean_code, c("U.*")) ~ 22)) %>% 
    select(-clean_code) 
}

categorise_avoidableadm <- function(data, code, new_col){
  
  # The following changes to the CQC defition (as published, 2013) were introduced by the IAU
  # uti: N39 removed, N390 Added
  # food and drink issues: R630, R633 added 
  # After consultation CQC analysts, they realised that R620 and R633 were left off the list by mistake
  # and that N39 was a typo
  
  
  data <- data %>% 
    mutate(clean_code := toupper(!!rlang::sym(code)),
           clean_code = gsub("[[:punct:]]", "", clean_code),
           clean_code = gsub(" ", "", clean_code))
  
  data <- data %>%
    mutate(!!new_col := case_when(detect_codes(clean_code, c("J2[0|1|2].*")) ~ "acute LRTIs",
                                  detect_codes(clean_code, c("J4[0|1|2|3|4].*")) ~ "chronic LRTIs",
                                  detect_codes(clean_code, c("L89.*"))~ "pressure sores",
                                  detect_codes(clean_code, c("E1[0|1|2|3|4].*")) ~ "diabetes",
                                  detect_codes(clean_code, c("R63[0|1|2|3|4|6|8]")) ~ "food and drink issues",
                                  detect_codes(clean_code, c("J69*")) ~ "food and liquid pneumonitis",
                                  detect_codes(clean_code, c("S[0|1|2|3|4|5|6|7|8|9][2|3|8].*",
                                                             "T0[2|3|5|8].*","T1[0|2].*")) ~ "fractures and sprains", 
                                  detect_codes(clean_code, c("A0[2|3|4|5|6|7|8|9].*")) ~ "intestinal infections",
                                  detect_codes(clean_code, c("J1[2|3|4|5|6|7|8].*")) ~ "pneunomia",
                                  detect_codes(clean_code, c("N390")) ~ "uti",
                                  TRUE ~ NA_character_)) %>% 
    select(-clean_code) 
}



## Functions to flag procedures 

# Cataract surgery (Floud et al)
flag_proc_cataract <- function(data, code, new_col){
  
  data <- data %>%
    mutate(!!new_col := detect_codes(!!rlang::sym(code), 
                                     c("C7[1|2|3|4|5]")))
}
