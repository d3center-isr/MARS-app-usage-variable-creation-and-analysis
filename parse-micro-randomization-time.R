source("paths.R")
library(tidyverse)

list_all_system_log <- readRDS(file = file.path(path_manipulated_data, "list_all_system_log.rds"))

# -----------------------------------------------------------------------------
# Extract rows associated with the time when micro-randomization happened
# -----------------------------------------------------------------------------

list_coinflip <- list()

for(i in 1:length(list_all_system_log)){
  # Note: It is safer to make it a habit of specifying fixed = TRUE
  #       whenever you are looking for a 'literal' expression, otherwise
  #       grepl() and related functions may interpret the specified pattern to
  #       be a regular expression. Read R documentation ?regex to learn more.
  #       (That is, unless you really want to leverage regex)
  #
  #       Below, specifying fixed = TRUE allows us to correctly identify those 
  #       rows in column V5 containing the patterns indicated below.
  
  dat_system_log <- list_all_system_log[[i]]
  
  dat_coinflip <- dat_system_log %>%
    filter(grepl(pattern = "BackgroundService/Listen(init)/Scheduler(EMI-RANDOM)", .data[["V5"]], fixed = TRUE)) %>%
    filter(grepl(pattern = "try", .data[["V5"]], fixed = TRUE)) %>%
    filter(grepl(pattern = "trigger", .data[["V5"]], fixed = TRUE)) %>%
    filter(grepl(pattern = "success", .data[["V6"]], fixed = TRUE))
  
  list_coinflip <- append(list_coinflip, list(dat_coinflip))
}

# -----------------------------------------------------------------------------
# Parse block-level information from micro-randomization logs
# -----------------------------------------------------------------------------

list_dat_triggered <- list()

for(i in 1:length(list_coinflip)){
  if(nrow(list_coinflip[[i]]) > 0){
    dat_triggered <- list_coinflip[[i]]
    
    # Unusually, there are a few participants whose V1 is of character type. 
    # In this case, dividing by 1000 will lead to an ERROR message.
    # For the vast majority of participants, their V1 is of numeric type
    if(is.numeric(dat_triggered[["V1"]]) == FALSE){
      dat_triggered <- dat_triggered %>% mutate(V1 = as.numeric(V1))
    }
    
    # Note that participants can come from different time zones
    # and that there is a rare case of at least one participant being located in
    # two different time zones over the study period.
    #
    # Columns in a data frame cannot store multiple time zones;
    # if you try, R will try to convert all rows into the same time zone!
    # To get around this issue, we store a human-readable UTC time zone 
    # and the participant's time zone in the column 'olson'.
    
    # V1 -- contains the timestamp when the coinflip was triggered by the system
    # V3 -- displayed human-readable times are in the participant's LOCAL TIME;
    #       a good sanity check to see if you are converting V1 to 
    #       human-readable time correctly is to match what you get to V3
    dat_triggered <- dat_triggered %>%
      mutate(ts_coinflip_utc = as_datetime(x = V1/1000, tz = "UTC"))  %>%
      mutate(olson = case_when(
        V2/(1000*60*60) == -5 ~ "US/Eastern", 
        V2/(1000*60*60) == -6 ~ "US/Central", 
        V2/(1000*60*60) == -7 ~ "US/Mountain", 
        V2/(1000*60*60) == -8 ~ "US/Pacific",
        .default = NULL
      )) 
    
    # Create a column in mountain time for everyone 
    # even for participants not actually in mountain time, 
    # so that we have a quick way to eye-ball the data when we do quick checks.
    dat_triggered[["ts_coinflip_mountain"]] <- with_tz(time = dat_triggered[["ts_coinflip_utc"]], tzone = "US/Mountain")
    
    # V5 -- contains the begin and end time of blocks;
    #       the displayed human-readable times are in the participant's LOCAL TIME
    #       this column also records the block in which the coinflip was triggered.
    
    dat_parse_block <- strsplit(x = dat_triggered[["V5"]], split = "when")
    block_info <- lapply(dat_parse_block, function(x){x[2]})
    block_info <- unlist(block_info)
    
    dat_triggered <- dat_triggered %>% 
      mutate(block_info = block_info) %>%
      mutate(block_number = case_when(
        grepl(pattern = "EMI-RANDOM-0", x = .data[["block_info"]]) ~ 0,
        grepl(pattern = "EMI-RANDOM-1", x = .data[["block_info"]]) ~ 1,
        grepl(pattern = "EMI-RANDOM-2", x = .data[["block_info"]]) ~ 2,
        grepl(pattern = "EMI-RANDOM-3", x = .data[["block_info"]]) ~ 3,
        grepl(pattern = "EMI-RANDOM-4", x = .data[["block_info"]]) ~ 4,
        grepl(pattern = "EMI-RANDOM-5", x = .data[["block_info"]]) ~ 5,
        .default = NULL
      ))
    
    # Important to remember to set fixed = TRUE since we want an exact match
    # and not a regular expression.
    tmp <- substring(block_info, first = 22)
    tmp2 <- sub(pattern = ")/try[", replacement = "marianatrench", x = tmp, fixed = TRUE)
    tmp3 <- strsplit(x = tmp2, split = "marianatrench")
    block_bounds <- lapply(tmp3, function(x){x[1]})
    block_bounds <- unlist(block_bounds)
    upper_block_bounds <- substring(block_bounds, first = 25, last = 47)
    lower_block_bounds <- substring(block_bounds, first = 1, last = 23)
    
    # Now, begin converting strings to POSIXct objects you can work with.
    olson <- dat_triggered[["olson"]]
    list_ubb <- as.list(upper_block_bounds)
    list_lbb <- as.list(lower_block_bounds)
    
    list_ubb_converted <- list()
    list_lbb_converted <- list()
    list_interval_converted <- list()
    
    # Lists are able to handle elements having different time zones.
    for(j in 1:length(olson)){
      list_ubb_converted[[j]] <- mdy_hms(list_ubb[[j]], tz = olson[j])
      list_lbb_converted[[j]] <- mdy_hms(list_lbb[[j]], tz = olson[j])
    }
    
    # Create a column in mountain time for everyone 
    # even for participants not actually in mountain time.
    for(j in 1:length(olson)){
      list_ubb_converted[[j]] <- with_tz(list_ubb_converted[[j]], tzone = "US/mountain")
      list_lbb_converted[[j]] <- with_tz(list_lbb_converted[[j]], tzone = "US/mountain")
    }
    
    # Now, create an interval, which will be in mountain time.
    for(j in 1:length(olson)){
      list_interval_converted[[j]] <- list_lbb_converted[[j]] %--% list_ubb_converted[[j]]
    }
    
    # Prepare to append this column and extract the calendar date when each
    # block happened.
    #
    # While the code logic to obtain delivery_bounds_mountain for micro-randomizations
    # is the same code logic to obtain block_bounds_mountain for 2QS
    # the interpretation of the resulting value differs.
    # For micro-randomizations, the time interval pertains to the span of time the software 
    # will try to micro-randomize for that block.
    block_mountain <- bind_rows(lapply(list_interval_converted, FUN = as.data.frame))
    block_mountain <- block_mountain$`X[[i]]`
    dat_triggered[["randomization_bounds_mountain"]] <- block_mountain
    
    dat_triggered <- dat_triggered %>%
      select(mars_id, olson, block_number, 
             randomization_bounds_mountain, ts_coinflip_mountain)
    
    list_dat_triggered <- append(list_dat_triggered, list(dat_triggered))
  }
}

list_dat_triggered_coinflip <- list_dat_triggered

# -----------------------------------------------------------------------------
# We discover that there are exact duplicates in the raw data.
# Hence, we need a step where we eliminate exact duplicates.
# -----------------------------------------------------------------------------

count_events_within_list <- function(x){
  # Args: 
  #   x -- a list element
  #
  # Output:
  #   out - a tibble
  
  curr_id <- x[["mars_id"]][1]
  out <- x %>% 
    # Note that we expect each randomization bound to be 
    # associated with exactly one micro-randomization
    # Obtaining these summary statistics are a way to diagnose deviations
    # from this expectation.
    group_by(randomization_bounds_mountain) %>% 
    summarise(b = min(block_number), count = n()) %>%
    mutate(mars_id = curr_id) %>%
    select(mars_id, everything())
  
  return(out)
}

table_coinflip <- lapply(list_dat_triggered_coinflip, FUN = count_events_within_list)
table_coinflip <- bind_rows(table_coinflip)
print(table_coinflip)

dedup_within_list <- function(x){
  # Args: 
  #   x -- a list element
  #
  # Output:
  #   x -- the list element, with EXACT duplicates removed
  
  x <- x[!duplicated(x),]
  return(x)
}

list_dat_triggered_coinflip <- lapply(list_dat_triggered_coinflip, FUN = dedup_within_list)

# -----------------------------------------------------------------------------
# Save output
# -----------------------------------------------------------------------------

saveRDS(list_dat_triggered_coinflip, file = file.path(path_manipulated_data, "list_dat_triggered_coinflip.rds"))

