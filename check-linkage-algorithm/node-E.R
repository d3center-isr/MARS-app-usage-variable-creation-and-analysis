rm(list = ls())

source("paths.R")
library(tidyverse)

dat_all_with_matched_status <- readRDS(file = file.path(path_manipulated_data, "ema_linked_trigger_to_status.rds"))
orig_colnames <- setdiff(x = colnames(dat_all_with_matched_status), y = c("olson_triggered","olson_status","raw_tracking_id"))

dat_conventional_long_format_ema_responses <- readRDS(file = file.path(path_manipulated_data, "dat_conventional_long_format_ema_responses.rds"))
dat_response_to_triggered_ema <- dat_conventional_long_format_ema_responses %>% rename(olson_response = olson) %>% arrange(mars_id, ts_start_time_mountain) %>% mutate(raw_tracking_id_response = 1:nrow(.))

list_all_with_matched_status <- list()
all_ids <- unique(dat_all_with_matched_status[["mars_id"]])

for(i in 1:length(all_ids)){
  current_participant <- all_ids[i]
  dat_participant_triggered <- dat_all_with_matched_status %>% filter(mars_id == current_participant)
  dat_participant_response <- dat_response_to_triggered_ema %>% filter(mars_id == current_participant)
  
  # NOTE: We are using a RIGHT JOIN here!
  dat_participant_triggered <- right_join(x = dat_participant_response,
                                          y = dat_participant_triggered,
                                          by = join_by(mars_id == mars_id,
                                                       between(x = ts_start_time_mountain,
                                                               y_lower = ts_ema_triggered_mountain,
                                                               y_upper = ts_status_survey_ema_mountain,
                                                               bounds = "[]")
                                                       )
                                          )
  
  # Create a tracker to detect many-to-one matches resulting from the right join operation above
  dat_participant_triggered <- dat_participant_triggered %>% arrange(ts_ema_triggered_mountain)
  # Rows in tmp1 represent EMA which were triggered but no response was matched
  tmp1 <- dat_participant_triggered %>% filter(is.na(response_ema_id))
  # Rows in tmp2 represent EMA which were triggered and a response was matched
  tmp2 <- dat_participant_triggered %>% filter(!is.na(response_ema_id))
  list_tmp <- list()
  if(nrow(tmp1) > 0){
    tmp1[["is_multiple_ema_response"]] <- NA
    list_tmp <- append(list_tmp, list(tmp1))
  }
  if(nrow(tmp2) > 0){
    tmp2[["is_multiple_ema_response"]] <- duplicated(tmp2[["response_ema_id"]])
    list_tmp <- append(list_tmp, list(tmp2))
  }
  dat_participant_triggered <- bind_rows(list_tmp)
  dat_participant_triggered <- dat_participant_triggered %>% arrange(ema_survey_id)
  dat_participant_triggered[["is_multiple_ema_trigger_time"]] = duplicated(dat_participant_triggered[["ema_survey_id"]])
  dat_participant_triggered <- dat_participant_triggered %>% select(is_multiple_ema_trigger_time, everything())
  
  list_all_with_matched_status <- append(list_all_with_matched_status, list(dat_participant_triggered))
}

dat_merged <- bind_rows(list_all_with_matched_status)

#############################################################################
# Were there instances of many-to-one matches?
# Answer: None
#############################################################################

if(FALSE){  # change to TRUE to execute next line
  # This sum is equal to zero: there were no instances of many-to-one matches 
  # resulting from the right join operation above.
  sum(dat_merged[["is_multiple_ema_response"]], na.rm = TRUE)
}

#############################################################################
# Were there instances where one trigger time could be matched to
# two response times?
# Answer: zero instances
#############################################################################

if(FALSE){  # change to TRUE to execute next line
  # This sum is equal to 5: there were instances of one-to-many matches 
  # resulting from the right join operation above.
  sum(dat_merged[["is_multiple_ema_trigger_time"]], na.rm = TRUE)
}

if(FALSE){
  table(dat_merged[["is_multiple_ema_response"]], dat_merged[["is_multiple_ema_trigger_time"]])
}

dat_merged <- dat_merged %>% select(-is_multiple_ema_response, -is_multiple_ema_trigger_time)

#############################################################################
# Were there any records in that were not matched? (discarded)
#############################################################################
tracking_orig <- dat_response_to_triggered_ema[["raw_tracking_id_response"]]
tracking_with_match <- unique(dat_merged[["raw_tracking_id_response"]])
tracking_with_nomatch <- setdiff(x = tracking_orig, y = tracking_with_match)
n_tracking_with_nomatch <- length(tracking_with_nomatch)   # There is 1 row in dat_response_to_triggered_ema which have not been linked!

dat_notmatched <- dat_response_to_triggered_ema %>% filter(raw_tracking_id_response %in% tracking_with_nomatch)
participants_having_notmatched <- unique(dat_notmatched[["mars_id"]])
n_participants_having_notmatched <- length(participants_having_notmatched)

# There is only one row in the data frame dat_response_to_triggered_ema
# that was not linked. We may manually inspect this case: 
# it turns out that the EMA trigger time it was supposed to be linked to 
# did not have a corresponding date time for status. 
# In the code below, we carry out linkage  "manually" and only use
# two date times to carry out the linkage,
# rather then three date times as in the loop above.
current_participant <- participants_having_notmatched  # Note that the right hand side is an array of length 1
this_response_ema_id <- dat_notmatched[["response_ema_id"]]
this_ema_survey_id <- 17
dat_participant_triggered <- dat_all_with_matched_status %>% filter(mars_id == current_participant) %>% filter(ema_survey_id == this_ema_survey_id)
dat_participant_response <- dat_response_to_triggered_ema %>% filter(mars_id == current_participant) %>% filter(response_ema_id == this_response_ema_id)
# Note that the left hand side will just be a data frame with one row
dat_participant_triggered <- right_join(x = dat_participant_response,
                                        y = dat_participant_triggered,
                                        by = join_by(mars_id == mars_id,
                                                     closest(ts_start_time_mountain > ts_ema_triggered_mountain)
                                                     )
                                        )
# Manually update status: change from "maybe_missed" to "completed"
# This is an example of a case where "maybe_missed" did not necessarily mean that the participant did not complete the EMA
# since we were eventually able to locate their responses and manually perform the linkage in this case.
dat_participant_triggered <- dat_participant_triggered %>% mutate(status_survey_ema = replace(status_survey_ema, ema_survey_id == this_ema_survey_id, "completed"))
# Now we can update the data
dat_merged <- dat_merged %>% filter(!((mars_id == current_participant) & (ema_survey_id == this_ema_survey_id)))
dat_merged <- rbind(dat_merged, dat_participant_triggered)
dat_merged <- dat_merged %>% arrange(mars_id, ema_survey_id)

#############################################################################
# Sanity check: perform this check again
# Were there any records in that were not matched? (discarded)
#############################################################################
tracking_orig <- dat_response_to_triggered_ema[["raw_tracking_id_response"]]
tracking_with_match <- unique(dat_merged[["raw_tracking_id_response"]])
tracking_with_nomatch <- setdiff(x = tracking_orig, y = tracking_with_match)
n_tracking_with_nomatch <- length(tracking_with_nomatch)  # There are no rows in dat_response_to_triggered_ema which have not been linked!

#############################################################################
# It appears that the timezone associated with status, i.e., olson_status
# is the most reliably correct one. It appears as though the software
# experiences a very very brief delay (i.e., incorrect timezone for trigger time
# is remedied as soon as status is assessed by the system) 
# in updating the recorded timezone when the phone is newly switched on 
# after crossing timezones, such as when the
# phone was delivered to remote-enrolled participants.
#############################################################################
dat_merged <- dat_merged %>% 
  mutate(is_identical_olson = case_when(
    is.na(olson_status) ~ 0,
    (!is.na(olson_status)) & (olson_triggered == olson_status) ~ 1,
    (!is.na(olson_status)) & (olson_triggered != olson_status) ~ 0,
    .default = NULL
  ))

dat_merged <- dat_merged %>%
  mutate(olson = olson_triggered) %>%
  mutate(olson = case_when(
    is.na(olson_status) ~ olson_triggered,
    (!is.na(olson_status)) & (olson_triggered == olson_status) ~ olson_triggered,
    (!is.na(olson_status)) & (olson_triggered != olson_status) ~ olson_status,  # Update only if olson_status exists
    .default = NULL
  ))

#############################################################################
# There was one participant for which 4 of their administered EMAs were
# designated as completed even though no response was provided for Q1, Q2, Q3
#############################################################################
dat_merged <- dat_merged %>%
  mutate(is_inc = if_else((!is.na(response_ema_id)) & status_survey_ema == "completed" & is.na(Q1_ts_finish_time_mountain) & is.na(Q2_ts_finish_time_mountain) & is.na(Q3_ts_finish_time_mountain), 1, 0)) %>%
  mutate(status_survey_ema = if_else(is_inc == 1, "incomplete", status_survey_ema)) %>%
  select(-is_inc)

#############################################################################
# Save and move on to next step
#############################################################################
dat_output <- dat_merged %>%
  select(-raw_tracking_id, -raw_tracking_id_response,
         -is_identical_olson, -olson_triggered, -olson_response, -olson_status) %>%
  select(mars_id, olson, all_of(orig_colnames), everything())

