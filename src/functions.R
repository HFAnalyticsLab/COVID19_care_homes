
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

# Count the overlap between time periods time period defined by end date
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



# rolling averages --------------------------------------------------------


sevendayavg <- function(col, rounding_digits = 0){
  
  rollapply(col, 7,
            mean, align = "right", partial = FALSE, fill = NA) %>% 
    round(rounding_digits)
}
