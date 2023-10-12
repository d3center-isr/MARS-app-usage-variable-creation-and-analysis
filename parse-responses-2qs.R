source("paths.R")
library(tidyverse)

list_all_loweffort_log <- readRDS(file = file.path(path_manipulated_data, "list_all_loweffort_log.rds"))

# Note that responses from 2qs can be generated in two ways:
#
#  -- As part of the sequence of events orchestrated by the software
#     in each of the 6 blocks. In this case, the 2qs is what we will call
#     'not triggered by the participant'.
#
#  -- On their own volition, the participant may access Quit Tip.
#     However, before a Quit Tip message is displayed, the phone will
#     prompt the participant to complete a 2qs. In this case, the 2qs is what 
#     we will call 'triggered by the participant'.
#
# In summary, we will use the terms 'not triggered by the participant' and 
# 'triggered by the participant' in our comments to distinguish between these
# situations.
#
# The primary aim and secondary aim of the trial will only make use of 2qs
# which were not triggered by the participant. 
#
# Eventually, we might also parse 2qs which were triggered by the participant 
# from list_all_loweffort_log as they may potentially  be useful in 
# sensitivity analysis.

# -----------------------------------------------------------------------------
# Parse responses to 2qs not triggered by the participant
# -----------------------------------------------------------------------------

list_dat_response_to_triggered_2qs <- list()

for(i in 1:length(list_all_loweffort_log)){
  loweffort_log <- list_all_loweffort_log[[i]]
  current_id <- loweffort_log[["mars_id"]][1]
  
  dat_2qs_responses <- loweffort_log %>%
    mutate(responded_to_triggered = grepl(pattern = "Tailor question completed", .data[["V1"]], fixed = TRUE)) %>%
    filter(responded_to_triggered)
  
  if(nrow(dat_2qs_responses) > 0){
    for(this_row in 1:nrow(dat_2qs_responses)){
      this_raw_unix_ts <- unlist(strsplit(x = dat_2qs_responses[["V1"]][this_row], split = ","))[1]
      this_raw_offset <- unlist(strsplit(x = dat_2qs_responses[["V1"]][this_row], split = ","))[2]
      this_raw_response <- unlist(strsplit(x = dat_2qs_responses[["V1"]][this_row], split = "cig"))[2]
      dat_2qs_responses[["raw_unix_ts"]][this_row] <- this_raw_unix_ts
      dat_2qs_responses[["raw_offset"]][this_row] <- this_raw_offset
      dat_2qs_responses[["raw_response"]][this_row] <- paste("cig", substr(x = this_raw_response, start = 1, stop = 36), sep="")
    }
    
    dat_2qs_responses <- dat_2qs_responses %>%
      mutate(raw_unix_ts = as.numeric(raw_unix_ts),
             raw_offset = as.numeric(raw_offset)) %>%
      mutate(ts_responded_2qs_utc = as_datetime(x = raw_unix_ts/1000, tz = "UTC"))  %>%
      mutate(olson = case_when(
        raw_offset/(1000*60*60) == -5 ~ "US/Eastern", 
        raw_offset/(1000*60*60) == -6 ~ "US/Central", 
        raw_offset/(1000*60*60) == -7 ~ "US/Mountain", 
        raw_offset/(1000*60*60) == -8 ~ "US/Pacific",
        .default = NA_character_  # -- the syntax '.default' comes in newer versions of dplyr; make sure to read the documentation for dplyr::case_when
      )) 
    
    dat_2qs_responses[["ts_responded_2qs_mountain"]] <- with_tz(time = dat_2qs_responses[["ts_responded_2qs_utc"]], tzone = "US/Mountain")
    
    dat_2qs_responses <- dat_2qs_responses %>%
      mutate(cig_available = case_when(
        grepl(pattern = "cig=1", .data[["raw_response"]], fixed = TRUE) ~ 1,
        grepl(pattern = "cig=0", .data[["raw_response"]], fixed = TRUE) ~ 0,
        .default = NULL   # -- this syntax comes in newer versions of dplyr; make sure to read the documentation for dplyr::case_when
      )) %>%
      mutate(negative_affect = case_when(
        grepl(pattern = "emotion=1", .data[["raw_response"]], fixed = TRUE) ~ 1,
        grepl(pattern = "emotion=0", .data[["raw_response"]], fixed = TRUE) ~ 0,
        .default = NULL   # -- this syntax comes in newer versions of dplyr; make sure to read the documentation for dplyr::case_when
      ))
    
    dat_2qs_responses <- dat_2qs_responses %>%
      mutate(mars_id = current_id) %>%
      select(mars_id, olson, ts_responded_2qs_mountain,
             cig_available, negative_affect, raw_response)
    
    list_dat_response_to_triggered_2qs <- append(list_dat_response_to_triggered_2qs, list(dat_2qs_responses)) 
  }
}

# -----------------------------------------------------------------------------
# Sanity check: Do a spot check of raw responses versus parsed responses
# -----------------------------------------------------------------------------

dat_response_to_triggered_2qs <- bind_rows(list_dat_response_to_triggered_2qs)

table(dat_response_to_triggered_2qs[["raw_response"]], dat_response_to_triggered_2qs[["cig_available"]])
table(dat_response_to_triggered_2qs[["raw_response"]], dat_response_to_triggered_2qs[["negative_affect"]])
sum(is.na(dat_response_to_triggered_2qs[["raw_response"]]))

list_dat_response_to_triggered_2qs <- lapply(list_dat_response_to_triggered_2qs, 
                                             function(x){
                                               x <- x %>% select(-raw_response)
                                               return(x)
                                             })

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
    group_by(ts_responded_2qs_mountain) %>% 
    summarise(count = n()) %>%
    mutate(mars_id = curr_id) %>%
    select(mars_id, everything())
  
  return(out)
}

table_response_2qs <- lapply(list_dat_response_to_triggered_2qs, FUN = count_events_within_list)
table_response_2qs <- bind_rows(table_response_2qs)
#print(table_response_2qs)  # Uncomment to reveal table

dedup_within_list <- function(x){
  # Args: 
  #   x -- a list element
  #
  # Output:
  #   x -- the list element, with EXACT duplicates removed
  
  x <- x[!duplicated(x),]
  return(x)
}


list_dat_response_to_triggered_2qs <- lapply(list_dat_response_to_triggered_2qs, FUN = dedup_within_list)

# -----------------------------------------------------------------------------
# Save output
# -----------------------------------------------------------------------------

saveRDS(list_dat_response_to_triggered_2qs, file.path(path_manipulated_data, "list_dat_response_to_triggered_2qs.rds"))

