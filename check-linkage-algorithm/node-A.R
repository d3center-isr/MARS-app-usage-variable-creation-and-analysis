rm(list = ls())

source("paths.R")
library(tidyverse)

list_dat_triggered_2qs <- readRDS(file = file.path(path_manipulated_data, "list_dat_triggered_2qs.rds"))
list_status_survey_2qs <- readRDS(file = file.path(path_manipulated_data, "list_status_survey_2qs.rds"))

dat_triggered_2qs <- bind_rows(list_dat_triggered_2qs)
dat_status_survey_2qs <- bind_rows(list_status_survey_2qs)
# Spot checks reveal that it is possible for errors to occur in the 
# system-recorded timezone. Including a suffix in the parsed timezone variables
# will allow us to perform checking later on.
dat_triggered_2qs <- dat_triggered_2qs %>% rename(olson_triggered = olson) %>% arrange(mars_id, ts_2qs_triggered_mountain)
dat_status_survey_2qs <- dat_status_survey_2qs %>% rename(olson_status = olson)
dat_status_survey_2qs <- dat_status_survey_2qs %>% arrange(mars_id, ts_status_survey_2qs_mountain) %>% mutate(raw_tracking_id = 1:nrow(.))

list_all_with_matched_status <- list()
all_ids <- unique(dat_triggered_2qs[["mars_id"]])

for(i in 1:length(all_ids)){
  current_participant <- all_ids[i]
  dat_participant_triggered <- dat_triggered_2qs %>% filter(mars_id == current_participant) %>% arrange(ts_2qs_triggered_mountain) %>% mutate(quick_survey_id = 1:nrow(.))
  dat_participant_status <- dat_status_survey_2qs %>% filter(mars_id == current_participant) %>%
    mutate(raw_tracking_id_lag1 = lag(raw_tracking_id),
           ts_status_survey_2qs_mountain_lag1 = lag(ts_status_survey_2qs_mountain),
           status_survey_2qs_lag1 = lag(status_survey_2qs))
  
  #############################################################################
  # This is our 'ideal' matching rule
  #############################################################################
  dat_participant_triggered <- left_join(x = dat_participant_triggered,
                                         y = dat_participant_status,
                                         by = join_by(mars_id == mars_id, 
                                                      closest(ts_2qs_triggered_mountain <= ts_status_survey_2qs_mountain)
                                                      )
                                         )
  dat_participant_triggered[["secs_between"]] <- int_length(dat_participant_triggered[["ts_2qs_triggered_mountain"]] %--% dat_participant_triggered[["ts_status_survey_2qs_mountain"]])
  
  #############################################################################
  # This calculation is for diagnosing issues in our 'ideal' matching rule
  #############################################################################
  dat_participant_triggered[["is_dup_timestamp"]] <- duplicated(dat_participant_triggered[["ts_status_survey_2qs_mountain"]], fromLast = TRUE)
  dat_participant_triggered[["secs_between2"]] <- int_length(dat_participant_triggered[["ts_2qs_triggered_mountain"]] %--% dat_participant_triggered[["ts_status_survey_2qs_mountain_lag1"]])
  
  list_all_with_matched_status <- append(list_all_with_matched_status, list(dat_participant_triggered))
}

dat_all_with_matched_status <- bind_rows(list_all_with_matched_status)

#############################################################################
# Perform checks
#############################################################################
dat_for_checks <- dat_all_with_matched_status %>% mutate(is_mismatch = if_else(is_dup_timestamp == TRUE, 1, 0))
dat_for_checks_no_mismatch <- dat_for_checks %>% filter(is_mismatch == 0)
dat_for_checks_with_mismatch <- dat_for_checks %>% filter(is_mismatch == 1)  # This was 47 records for which trigger date-time has no corresponding status date-time

#############################################################################
# Implement fix on the rows having mismatch
#############################################################################

# We supply the name "maybe_missed" to say that we aren't certain that this 2qs was "missed"
# in the sense of how the software uses the word "missed". Another possibility is that
# the responses could have gotten lost/went unrecorded for some reason.
# Note that 2qs which we label as "maybe_missed" exist in the tailor_report.csv file
# but it is not obvious from the tailor_report.csv file alone that these 2qs need to be included in the parsing,
# e.g., the status column in the tailor_report.csv file will have no recorded value for status.
# The logic in this script appropriately reveals these 2qs which in fact need to be included in the parsing.

dat_all_with_matched_status <- dat_all_with_matched_status %>%
  mutate(is_mismatch = if_else(is_dup_timestamp == TRUE, 1, 0)) %>%
  mutate(ts_status_survey_2qs_mountain = if_else(is_mismatch == 1, NA, ts_status_survey_2qs_mountain),
         status_survey_2qs = if_else(is_mismatch == 1, "maybe_missed", status_survey_2qs),
         olson_status = if_else(is_mismatch == 1, NA, olson_status),
         raw_tracking_id = if_else(is_mismatch == 1, NA, raw_tracking_id)) %>%
  select(-raw_tracking_id_lag1, -ts_status_survey_2qs_mountain_lag1, -status_survey_2qs_lag1,
         -secs_between, -secs_between2, -is_dup_timestamp, -is_mismatch)

#############################################################################
# Were there any records in that were not matched? (discarded)
# Answer: Since the value of n_tracking_with_nomatch is zero, the answer
# is none
#############################################################################
tracking_orig <- dat_status_survey_2qs[["raw_tracking_id"]]
tracking_with_match <- unique(dat_all_with_matched_status[["raw_tracking_id"]])
tracking_with_nomatch <- setdiff(x = tracking_orig, y = tracking_with_match)
n_tracking_with_nomatch <- length(tracking_with_nomatch)

#############################################################################
# Save and move on to next step
#############################################################################
saveRDS(dat_all_with_matched_status, file = file.path(path_manipulated_data, "brief_survey_linked_trigger_to_status.rds"))

