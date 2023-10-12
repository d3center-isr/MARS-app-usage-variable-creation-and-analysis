rm(list = ls())

source("paths.R")
library(tidyverse)

list_dat_triggered_coinflip <- readRDS(file = file.path(path_manipulated_data, "list_dat_triggered_coinflip.rds"))
list_all_emi <- readRDS(file = file.path(path_manipulated_data, "list_all_emi.rds"))

dat_triggered_coinflip <- bind_rows(list_dat_triggered_coinflip)
dat_all_emi <- bind_rows(list_all_emi)
dat_all_emi <- dat_all_emi %>% arrange(mars_id, ts_recorded_mountain) %>% mutate(raw_tracking_id = 1:nrow(.))

list_all <- list()
all_ids <- unique(dat_triggered_coinflip[["mars_id"]])

for(i in 1:length(all_ids)){
  current_participant <- all_ids[i]
  dat_participant_triggered <- dat_triggered_coinflip %>% filter(mars_id == current_participant)
  dat_participant_emi <- dat_all_emi %>% filter(mars_id == current_participant) %>%
    select(mars_id, ts_recorded_mountain, A, raw_tracking_id) %>%
    mutate(raw_tracking_id_lag1 = lag(raw_tracking_id),
           ts_recorded_mountain_lag1 = lag(ts_recorded_mountain),
           A_lag1 = lag(A))
  
  #############################################################################
  # This is our 'ideal' matching rule
  #############################################################################
  dat_participant_triggered <- left_join(x = dat_participant_triggered,
                                         y = dat_participant_emi,
                                         by = join_by(mars_id == mars_id, 
                                                      closest(ts_coinflip_mountain<=ts_recorded_mountain)
                                                      )
                                         )
  
  dat_participant_triggered[["secs_between"]] <- int_length(dat_participant_triggered[["ts_coinflip_mountain"]] %--% dat_participant_triggered[["ts_recorded_mountain"]])
  
  #############################################################################
  # This calculation is for diagnosing issues in our 'ideal' matching rule
  #############################################################################
  dat_participant_triggered[["secs_between2"]] <- int_length(dat_participant_triggered[["ts_coinflip_mountain"]] %--% dat_participant_triggered[["ts_recorded_mountain_lag1"]])
  
  #############################################################################
  # All done! Create a unique identifier for each micro-randomization
  #############################################################################
  dat_participant_triggered <- dat_participant_triggered %>% 
    mutate(coinflip_id = 1:nrow(.)) %>%
    select(mars_id, olson, coinflip_id, everything())
  
  list_all <- append(list_all, list(dat_participant_triggered))
}

dat_all_matched <- bind_rows(list_all)

#############################################################################
# Perform checks
#############################################################################
dat_for_checks <- dat_all_matched %>% mutate(is_mismatch = if_else(secs_between > 120, 1, 0))
dat_for_checks_no_mismatch <- dat_for_checks %>% filter(is_mismatch == 0)
# There is only one record from only one participant having a mismatch
# In this case, their recorded date-time came a few milliseconds before micro-randomization date-time
# This was manually fixed in the code below
dat_for_checks_with_mismatch <- dat_for_checks %>% filter(is_mismatch == 1)  

#############################################################################
# Implement fix on the rows having mismatch
#############################################################################
dat_all_matched <- dat_all_matched %>%
  mutate(is_mismatch = if_else(secs_between > 120, 1, 0)) %>%
  mutate(A = if_else(is_mismatch == 1, A_lag1, A),
         raw_tracking_id = if_else(is_mismatch == 1, raw_tracking_id_lag1, raw_tracking_id),
         ts_recorded_mountain = if_else(is_mismatch == 1, ts_recorded_mountain_lag1, ts_recorded_mountain),
         secs_between = if_else(is_mismatch == 1, secs_between2, secs_between)) %>%
  select(-raw_tracking_id_lag1, -ts_recorded_mountain_lag1, -A_lag1, -secs_between2)

#############################################################################
# Were there any records in that were not matched? (discarded)
#############################################################################
tracking_orig <- dat_all_emi[["tracking_id"]]
tracking_with_match <- unique(dat_all_matched[["raw_tracking_id"]])
tracking_with_nomatch <- setdiff(x = tracking_orig, y = tracking_with_match)
n_tracking_with_nomatch <- length(tracking_with_nomatch)

#############################################################################
# Sanity check: What are the possible values of block_number that 
# were parsed from the system log file entries corresponding to the
# micro-randomizations?
#############################################################################

#   * We see that the value of block_number is always equal to zero
#     which indicates that although emi_report.csv has block numbers, 
#     the block numbers in emi_report.csv must have been merged from some other
#     data source, using some merging rule (which is unknown).
#   * We remove the column block_number before saving.
#   * In another script, we will develop a rule that identifies the originating
#     block of the micro-randomization.

tbl <- table(dat_all_matched[["block_number"]])
# print(tbl)  # Uncomment to reveal table

#############################################################################
# Prepare for output
#############################################################################
dat_for_output <- dat_all_matched %>% select(-secs_between, -is_mismatch, -raw_tracking_id, -block_number)

