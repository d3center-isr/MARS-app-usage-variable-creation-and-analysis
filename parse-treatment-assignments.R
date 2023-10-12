source("paths.R")
library(tidyverse)

list_all_system_log <- readRDS(file = file.path(path_manipulated_data, "list_all_system_log.rds"))

list_all_emi <- list()

for(i in 1:length(list_all_system_log)){
  dat_randomized_assignments <- list_all_system_log[[i]] %>% 
    filter(V6=="random selection=0" | V6=="random selection=1" | V6=="random selection=2" | V6=="random selection=3") %>%
    mutate(A = V6) %>%
    mutate(A = replace(A, A=="random selection=0","none"),
           A = replace(A, A=="random selection=1","none"),
           A = replace(A, A=="random selection=2","low_effort"),
           A = replace(A, A=="random selection=3","mars")) 
  
  if(is.numeric(dat_randomized_assignments[["V1"]]) == FALSE){
    # Unusually, there are a few participants whose V1 is of character type.
    # In this case, dividing by 1000 will lead to an ERROR message.
    # For the vast majority of participants, their V1 is of numeric type.
    dat_randomized_assignments <- dat_randomized_assignments %>% mutate(V1 = as.numeric(V1))
  }
  
  dat_randomized_assignments <- dat_randomized_assignments %>% 
    mutate(ts_recorded_utc = as_datetime(x = V1/1000, tz = "UTC"))
  
  # One participant seemed to have switched time zone during the course of the study
  # The code chunks below handle how to represent time when time zone switching is possible
  # Since R cannot have a date-time column where one row comes from one time zone
  # and another row within the same column comes from another time zone,
  # we need to find a way to represent events in the study in a way that
  # considers this limitation of R. What we have done here is to save the 
  # specific time zone the event happened in variable called 'olson';
  # 'olson' is of character type. Then, we construct time variables in
  # Mountain Time. You can always construct a new column by converting
  # from Mountain Time to another time zone.
  dat_randomized_assignments <- dat_randomized_assignments %>%
    mutate(olson = case_when(
      V2/(1000*60*60) == -5 ~ "US/Eastern", 
      V2/(1000*60*60) == -6 ~ "US/Central", 
      V2/(1000*60*60) == -7 ~ "US/Mountain", 
      V2/(1000*60*60) == -8 ~ "US/Pacific"
    ))
  
  dat_randomized_assignments[["ts_recorded_mountain"]] <- with_tz(time = dat_randomized_assignments[["ts_recorded_utc"]], tzone = "US/Mountain")
  
  # The curated dataset will look cluttered if we create five columns, one for each time zone.
  # We simply store the participant's local time zone and create a column for time in Mountain Time
  dat_randomized_assignments <- dat_randomized_assignments %>% 
    select(mars_id, olson, ts_recorded_mountain, A)
  
  list_all_emi <- append(list_all_emi, list(dat_randomized_assignments))
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
  
  curr_id <- x[["mars_id"]][1]
  
  out <- x %>% 
    group_by(ts_recorded_mountain) %>% 
    summarise(count = n()) %>%
    mutate(mars_id = curr_id) %>%
    select(mars_id, everything())
  
  return(out)
}

table_randomized_assignments <- lapply(list_all_emi, FUN = count_events_within_list)
table_randomized_assignments <- bind_rows(table_randomized_assignments)
print(table_randomized_assignments)

dedup_within_list <- function(x){
  # Args: 
  #   x -- a list element
  #
  # Output:
  #   x -- the list element, with EXACT duplicates removed
  
  x <- x[!duplicated(x),]
  return(x)
}

list_all_emi <- lapply(list_all_emi, FUN = dedup_within_list)

# -----------------------------------------------------------------------------
# Save output
# -----------------------------------------------------------------------------

saveRDS(list_all_emi, file.path(path_manipulated_data, "list_all_emi.rds"))
