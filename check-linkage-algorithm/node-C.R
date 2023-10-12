rm(list = ls())

source("paths.R")
library(tidyverse)

list_dat_triggered_ema <- readRDS(file = file.path(path_manipulated_data, "list_dat_triggered_ema.rds"))
list_status_survey_ema <- readRDS(file = file.path(path_manipulated_data, "list_status_survey_ema.rds"))

dat_triggered_ema <- bind_rows(list_dat_triggered_ema)
dat_status_survey_ema <- bind_rows(list_status_survey_ema)

dat_triggered_ema <- dat_triggered_ema %>% rename(olson_triggered = olson) %>% arrange(mars_id, ts_ema_triggered_mountain)
dat_status_survey_ema <- dat_status_survey_ema %>% rename(olson_status = olson)
dat_status_survey_ema <- dat_status_survey_ema %>% arrange(mars_id, ts_status_survey_ema_mountain) %>% mutate(raw_tracking_id = 1:nrow(.))

list_all_with_matched_status <- list()
all_ids <- unique(dat_triggered_ema[["mars_id"]])

for(i in 1:length(all_ids)){
  current_participant <- all_ids[i]
  dat_participant_triggered <- dat_triggered_ema %>% filter(mars_id == current_participant) %>% arrange(ts_ema_triggered_mountain) %>% mutate(ema_survey_id = 1:nrow(.))
  dat_participant_status <- dat_status_survey_ema %>% filter(mars_id == current_participant) %>%
    mutate(raw_tracking_id_lag1 = lag(raw_tracking_id),
           ts_status_survey_ema_mountain_lag1 = lag(ts_status_survey_ema_mountain),
           status_survey_ema_lag1 = lag(status_survey_ema))

  dat_participant_triggered <- left_join(x = dat_participant_triggered,
                                         y = dat_participant_status,
                                         by = join_by(mars_id == mars_id, 
                                                      closest(ts_ema_triggered_mountain <= ts_status_survey_ema_mountain)
                                                      )
                                         )
  dat_participant_triggered[["secs_between"]] <- int_length(dat_participant_triggered[["ts_ema_triggered_mountain"]] %--% dat_participant_triggered[["ts_status_survey_ema_mountain"]])
  
  #############################################################################
  # This calculation is for diagnosing issues in our 'ideal' matching rule
  #############################################################################
  dat_participant_triggered[["is_dup_timestamp"]] <- duplicated(dat_participant_triggered[["ts_status_survey_ema_mountain"]], fromLast = TRUE)
  dat_participant_triggered[["secs_between2"]] <- int_length(dat_participant_triggered[["ts_ema_triggered_mountain"]] %--% dat_participant_triggered[["ts_status_survey_ema_mountain_lag1"]])
  
  list_all_with_matched_status <- append(list_all_with_matched_status, list(dat_participant_triggered))
}

dat_all_with_matched_status <- bind_rows(list_all_with_matched_status)

#############################################################################
# Perform checks
#############################################################################
dat_for_checks <- dat_all_with_matched_status %>% mutate(is_mismatch = if_else(is_dup_timestamp == TRUE, 1, 0))
dat_for_checks_no_mismatch <- dat_for_checks %>% filter(is_mismatch == 0)
dat_for_checks_with_mismatch <- dat_for_checks %>% filter(is_mismatch == 1)  # This was 42 records for which trigger date-time has no corresponding status date-time

#############################################################################
# Implement fix on the rows having mismatch
#############################################################################

# We supply the name "maybe_missed" to say that we aren't certain that this EMA was "missed"
# in the sense of how the software uses the word "missed". Another possibility is that
# the responses could have gotten lost/went unrecorded for some reason.
# Note that EMA which we label as "maybe_missed" exist in the tailor_report.csv file
# but it is not obvious from the ema_report.csv file alone that these EMA need to be included in the parsing,
# e.g., the status column in the ema_report.csv file will have no recorded value for status.
# The logic in this script appropriately reveals these EMA which in fact need to be included in the parsing.

dat_all_with_matched_status <- dat_all_with_matched_status %>%
  mutate(is_mismatch = if_else(is_dup_timestamp == TRUE, 1, 0)) %>%
  mutate(ts_status_survey_ema_mountain = if_else(is_mismatch == 1, NA, ts_status_survey_ema_mountain),
         status_survey_ema = if_else(is_mismatch == 1, "maybe_missed", status_survey_ema),
         olson_status = if_else(is_mismatch == 1, NA, olson_status),
         raw_tracking_id = if_else(is_mismatch == 1, NA, raw_tracking_id)) %>%
  select(-raw_tracking_id_lag1, -ts_status_survey_ema_mountain_lag1, -status_survey_ema_lag1,
         -secs_between, -secs_between2, -is_dup_timestamp, -is_mismatch)

#############################################################################
# Were there any records in that were not matched? (discarded)
# Answer: Since the value of n_tracking_with_nomatch is zero, the answer
# is none
#############################################################################
tracking_orig <- dat_status_survey_ema[["raw_tracking_id"]]
tracking_with_match <- unique(dat_all_with_matched_status[["raw_tracking_id"]])
tracking_with_nomatch <- setdiff(x = tracking_orig, y = tracking_with_match)
n_tracking_with_nomatch <- length(tracking_with_nomatch)

#############################################################################
# Save and move on to next step
#############################################################################
saveRDS(dat_all_with_matched_status, file = file.path(path_manipulated_data, "ema_linked_trigger_to_status.rds"))

