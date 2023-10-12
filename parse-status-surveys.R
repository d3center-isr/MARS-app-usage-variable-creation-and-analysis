source("paths.R")
library(tidyverse)

list_all_system_log <- readRDS(file = file.path(path_manipulated_data, "list_all_system_log.rds"))

list_all_2qs <- list()
list_all_ema <- list()

for(i in 1:length(list_all_system_log)){
  dat_system_log <- list_all_system_log[[i]]
  
  dat_status_2qs <- dat_system_log %>%
    filter(grepl(pattern = "BackgroundService/Listen(init)/Scheduler(EMA-TAILOR)", .data[["V5"]], fixed = TRUE)) %>%
    mutate(is_cancelled = grepl(pattern = "response=Cancel nextstate=null", .data[["V6"]], fixed = TRUE),
           is_missed = grepl(pattern = "response=MISSED nextstate=null", .data[["V6"]], fixed = TRUE),
           is_completed = grepl(pattern = "response=COMPLETED nextstate=null", .data[["V6"]], fixed = TRUE),
           is_timeout = grepl(pattern = "response=TIMEOUT nextstate=null", .data[["V6"]], fixed = TRUE))  %>%
    filter(is_cancelled | is_missed | is_completed | is_timeout) %>%
    mutate(status_survey_2qs = case_when(
      is_cancelled ~ "cancelled",
      is_missed ~ "missed",
      is_completed ~ "completed",
      is_timeout ~ "timedout",
      .default = NULL
    )) %>%
    mutate(V1 = as.numeric(V1)) %>%
    mutate(ts_status_survey_2qs_mountain = with_tz(time = as_datetime(x = V1/1000, tz = "UTC"), tzone = "US/Mountain")) %>%
    mutate(olson = case_when(
      V2/(1000*60*60) == -5 ~ "US/Eastern", 
      V2/(1000*60*60) == -6 ~ "US/Central", 
      V2/(1000*60*60) == -7 ~ "US/Mountain", 
      V2/(1000*60*60) == -8 ~ "US/Pacific",
      .default = NULL
    )) %>%
    select(mars_id, olson, ts_status_survey_2qs_mountain, status_survey_2qs)

  list_all_2qs <- append(list_all_2qs, list(dat_status_2qs))
  
  dat_status_ema <- dat_system_log %>%
    filter(grepl(pattern = "BackgroundService/Listen(init)/Scheduler(EMA-RANDOM)", .data[["V5"]], fixed = TRUE)) %>%
    mutate(is_cancelled = grepl(pattern = "response=Cancel nextstate=null", .data[["V6"]], fixed = TRUE),
           is_missed = grepl(pattern = "response=MISSED nextstate=null", .data[["V6"]], fixed = TRUE),
           is_completed = grepl(pattern = "response=COMPLETED nextstate=INCENTIVE_DATA_QUALITY_LAST_EMA", .data[["V6"]], fixed = TRUE),
           is_timeout = grepl(pattern = "response=TIMEOUT nextstate=null", .data[["V6"]], fixed = TRUE))  %>%
    filter(is_cancelled | is_missed | is_completed | is_timeout) %>%
    mutate(status_survey_ema = case_when(
      is_cancelled ~ "cancelled",
      is_missed ~ "missed",
      is_completed ~ "completed",
      is_timeout ~ "timedout",
      .default = NULL
    )) %>%
    mutate(V1 = as.numeric(V1)) %>%
    mutate(ts_status_survey_ema_mountain = with_tz(time = as_datetime(x = V1/1000, tz = "UTC"), tzone = "US/Mountain")) %>%
    mutate(olson = case_when(
      V2/(1000*60*60) == -5 ~ "US/Eastern", 
      V2/(1000*60*60) == -6 ~ "US/Central", 
      V2/(1000*60*60) == -7 ~ "US/Mountain", 
      V2/(1000*60*60) == -8 ~ "US/Pacific",
      .default = NULL
    )) %>%
    select(mars_id, olson, ts_status_survey_ema_mountain, status_survey_ema)
  
  list_all_ema <- append(list_all_ema, list(dat_status_ema))
}

# -----------------------------------------------------------------------------
# Sanity check: Do we have EXACT duplicates?
# Yes we do
# -----------------------------------------------------------------------------

count_events_within_list <- function(x){
  # Args: 
  #   x -- a list element
  #
  # Output:
  #   out - a tibble
  
  is_col_here <- ("ts_status_survey_2qs_mountain" %in% colnames(x))
  
  if(is_col_here){
    curr_id <- x[["mars_id"]][1]
    out <- x %>% 
      group_by(ts_status_survey_2qs_mountain) %>% 
      summarise(count = n()) %>%
      mutate(mars_id = curr_id) %>%
      select(mars_id, everything())
  }else{
    curr_id <- x[["mars_id"]][1]
    out <- x %>% 
      group_by(ts_status_survey_ema_mountain) %>% 
      summarise(count = n()) %>%
      mutate(mars_id = curr_id) %>%
      select(mars_id, everything())
  }
  
  return(out)
}

table_2qs <- lapply(list_all_2qs, FUN = count_events_within_list)
table_2qs <- bind_rows(table_2qs)
#print(table_2qs)  # Uncomment to reveal table

table_random_ema <- lapply(list_all_ema, FUN = count_events_within_list)
table_random_ema <- bind_rows(table_random_ema)
#print(table_random_ema)  # Uncomment to reveal table

dedup_within_list <- function(x){
  # Args: 
  #   x -- a list element
  #
  # Output:
  #   x -- the list element, with EXACT duplicates removed
  
  x <- x[!duplicated(x),]
  return(x)
}

list_all_2qs <- lapply(list_all_2qs, FUN = dedup_within_list)
list_all_ema <- lapply(list_all_ema, FUN = dedup_within_list)

# -----------------------------------------------------------------------------
# Save output
# -----------------------------------------------------------------------------

saveRDS(list_all_2qs, file.path(path_manipulated_data, "list_status_survey_2qs.rds"))
saveRDS(list_all_ema, file.path(path_manipulated_data, "list_status_survey_ema.rds"))

