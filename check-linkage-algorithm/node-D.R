rm(list = ls())

source("paths.R")
library(tidyverse)

dat_all_with_matched_status <- readRDS(file = file.path(path_manipulated_data, "brief_survey_linked_trigger_to_status.rds"))
list_dat_response_to_triggered_2qs <- readRDS(file = file.path(path_manipulated_data, "list_dat_response_to_triggered_2qs.rds"))
dat_response_to_triggered_2qs <- bind_rows(list_dat_response_to_triggered_2qs)
dat_response_to_triggered_2qs <- dat_response_to_triggered_2qs %>% rename(olson_response = olson) %>% arrange(mars_id, ts_responded_2qs_mountain) %>% mutate(raw_tracking_id_response = 1:nrow(.))

list_all_with_matched_status <- list()
all_ids <- unique(dat_all_with_matched_status[["mars_id"]])

for(i in 1:length(all_ids)){
  current_participant <- all_ids[i]
  dat_participant_triggered <- dat_all_with_matched_status %>% filter(mars_id == current_participant) 
  dat_participant_response <- dat_response_to_triggered_2qs %>% filter(mars_id == current_participant)
  
  # NOTE: We are using a RIGHT JOIN here!
  dat_participant_triggered <- right_join(x = dat_participant_response,
                                          y = dat_participant_triggered,
                                          by = join_by(mars_id == mars_id,
                                                       between(x = ts_responded_2qs_mountain,
                                                               y_lower = ts_2qs_triggered_mountain,
                                                               y_upper = ts_status_survey_2qs_mountain,
                                                               bounds = "[]")
                                                       ))
  
  # Create a tracker to detect many-to-one matches resulting from the right join operation above
  dat_participant_triggered <- dat_participant_triggered %>% arrange(ts_2qs_triggered_mountain)
  # Rows in tmp1 represent 2QS which were triggered but no response was matched
  tmp1 <- dat_participant_triggered %>% filter(is.na(raw_tracking_id_response))
  # Rows in tmp2 represent 2QS which were triggered and a response was matched
  tmp2 <- dat_participant_triggered %>% filter(!is.na(raw_tracking_id_response))
  list_tmp <- list()
  if(nrow(tmp1) > 0){
    tmp1[["is_multiple_2qs_response"]] <- NA
    list_tmp <- append(list_tmp, list(tmp1))
  }
  if(nrow(tmp2) > 0){
    tmp2[["is_multiple_2qs_response"]] <- duplicated(tmp2[["raw_tracking_id_response"]])
    list_tmp <- append(list_tmp, list(tmp2))
  }
  dat_participant_triggered <- bind_rows(list_tmp)
  dat_participant_triggered <- dat_participant_triggered %>% arrange(quick_survey_id)
  dat_participant_triggered[["is_multiple_2qs_trigger_time"]] = duplicated(dat_participant_triggered[["quick_survey_id"]])
  dat_participant_triggered <- dat_participant_triggered %>% select(is_multiple_2qs_trigger_time, everything())
  
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
  sum(dat_merged[["is_multiple_2qs_response"]], na.rm = TRUE)
}

#############################################################################
# Were there instances where one trigger time could be matched to
# two response times?
# Answer: 5 instances
#############################################################################

if(FALSE){  # change to TRUE to execute next line
  # This sum is equal to 5: there were instances of one-to-many matches 
  # resulting from the right join operation above.
  sum(dat_merged[["is_multiple_2qs_trigger_time"]], na.rm = TRUE)
}

if(FALSE){
  table(dat_merged[["is_multiple_2qs_response"]], dat_merged[["is_multiple_2qs_trigger_time"]])
}

dat_diagnose <- dat_merged %>% filter(is_multiple_2qs_trigger_time == 1)
participants_having_one_to_many_match <- unique(dat_diagnose[["mars_id"]])
n_participants_having_one_to_many_match <- length(participants_having_one_to_many_match)

dat_diagnose[["paired_raw_tracking_id_response"]] <- NA_real_
dat_diagnose[["comment"]] <- NA_character_

i <- 1
current_participant <- participants_having_one_to_many_match[i]
dat_diagnose_for_inspection <- dat_diagnose %>% filter(mars_id == current_participant)
dat_merged_for_inspection <- dat_merged %>% filter(mars_id == current_participant) %>% filter(raw_tracking_id_response == 520)
this_raw_tracking_id_response <- dat_merged_for_inspection[["raw_tracking_id_response"]]
# In both records, the value of cig_available and negative_affect were identical
# One trigger date time could be matched to two response date times (within seconds of each other)
# Regard these rows as representing duplicate values of cig_available and negative_affect
# Take the earlier response date time as the match
dat_diagnose <- dat_diagnose %>%
  mutate(paired_raw_tracking_id_response = replace(paired_raw_tracking_id_response, mars_id == current_participant, this_raw_tracking_id_response - 1)) %>%
  mutate(comment = replace(comment, mars_id == current_participant, "Regard these rows as representing duplicate values of cig_available and negative_affect"))

i <- 2
current_participant <- participants_having_one_to_many_match[i]
dat_diagnose_for_inspection <- dat_diagnose %>% filter(mars_id == current_participant)
dat_merged_for_inspection <- dat_merged %>% filter(mars_id == current_participant) %>% filter(raw_tracking_id_response == 1113)
this_raw_tracking_id_response <- dat_merged_for_inspection[["raw_tracking_id_response"]]
# In both records, the value of cig_available and negative_affect were identical
# One trigger date time could be matched to two response date times (within seconds of each other)
# Regard these rows as representing duplicate values of cig_available and negative_affect
# Take the earlier response date time as the match
dat_diagnose <- dat_diagnose %>%
  mutate(paired_raw_tracking_id_response = replace(paired_raw_tracking_id_response, mars_id == current_participant, this_raw_tracking_id_response - 1)) %>%
  mutate(comment = replace(comment, mars_id == current_participant, "Regard these rows as representing duplicate values of cig_available and negative_affect"))

i <- 3
current_participant <- participants_having_one_to_many_match[i]
dat_diagnose_for_inspection <- dat_diagnose %>% filter(mars_id == current_participant)
dat_merged_for_inspection <- dat_merged %>% filter(mars_id == current_participant) %>% filter(raw_tracking_id_response == 1230)
this_raw_tracking_id_response <- dat_merged_for_inspection[["raw_tracking_id_response"]]
# In both records, the value of cig_available and negative_affect were identical
# One trigger date time could be matched to two response date times (within seconds of each other)
# Regard these rows as representing duplicate values of cig_available and negative_affect
# Take the earlier response date time as the match
dat_diagnose <- dat_diagnose %>%
  mutate(paired_raw_tracking_id_response = replace(paired_raw_tracking_id_response, mars_id == current_participant, this_raw_tracking_id_response - 1)) %>%
  mutate(comment = replace(comment, mars_id == current_participant, "Regard these rows as representing duplicate values of cig_available and negative_affect"))

i <- 4
current_participant <- participants_having_one_to_many_match[i]
dat_diagnose_for_inspection <- dat_diagnose %>% filter(mars_id == current_participant)
dat_merged_for_inspection <- dat_merged %>% filter(mars_id == current_participant) %>% filter(raw_tracking_id_response == 1360)
this_raw_tracking_id_response <- dat_merged_for_inspection[["raw_tracking_id_response"]]
# In both records, the value of cig_available and negative_affect were identical
# One trigger date time could be matched to two response date times (within seconds of each other)
# Regard these rows as representing duplicate values of cig_available and negative_affect
# Take the earlier response date time as the match
dat_diagnose <- dat_diagnose %>%
  mutate(paired_raw_tracking_id_response = replace(paired_raw_tracking_id_response, mars_id == current_participant, this_raw_tracking_id_response - 1)) %>%
  mutate(comment = replace(comment, mars_id == current_participant, "Regard these rows as representing duplicate values of cig_available and negative_affect"))

i <- 5
current_participant <- participants_having_one_to_many_match[i]
dat_diagnose_for_inspection <- dat_diagnose %>% filter(mars_id == current_participant)
dat_merged_for_inspection <- dat_merged %>% filter(mars_id == current_participant) %>% filter(raw_tracking_id_response == 2664)
this_raw_tracking_id_response <- dat_merged_for_inspection[["raw_tracking_id_response"]]
# In both records, the value of cig_available and negative_affect were identical
# One trigger date time could be matched to two response date times (within seconds of each other)
# Regard these rows as representing duplicate values of cig_available and negative_affect
# Take the earlier response date time as the match
dat_diagnose <- dat_diagnose %>%
  mutate(paired_raw_tracking_id_response = replace(paired_raw_tracking_id_response, mars_id == current_participant, this_raw_tracking_id_response - 1)) %>%
  mutate(comment = replace(comment, mars_id == current_participant, "Regard these rows as representing duplicate values of cig_available and negative_affect"))

dat_diagnose <- dat_diagnose %>% select(mars_id, raw_tracking_id_response, paired_raw_tracking_id_response, comment)

dat_merged <- dat_merged %>%
  filter(is_multiple_2qs_trigger_time == FALSE) %>%
  select(-is_multiple_2qs_response, -is_multiple_2qs_trigger_time)

#############################################################################
# Were there any records in that were not matched? (discarded)
#############################################################################
discarded_tracking_ids <- dat_diagnose[["raw_tracking_id_response"]]
tracking_orig <- setdiff(x = dat_response_to_triggered_2qs[["raw_tracking_id_response"]], y = discarded_tracking_ids)
tracking_with_match <- unique(dat_merged[["raw_tracking_id_response"]])
tracking_with_nomatch <- setdiff(x = tracking_orig, y = tracking_with_match)
n_tracking_with_nomatch <- length(tracking_with_nomatch)   

dat_notmatched <- dat_response_to_triggered_2qs %>% filter(raw_tracking_id_response %in% tracking_with_nomatch)
participants_having_notmatched <- unique(dat_notmatched[["mars_id"]])
n_participants_having_notmatched <- length(participants_having_notmatched)  

if(FALSE){
  print(n_tracking_with_nomatch)  # There are 6 rows in dat_response_to_triggered_2qs which have not been linked!
  print(n_participants_having_notmatched)  # There are 6 participants -- thus we have one row per participant which has not been matched
}

dat_notmatched[["paired_raw_tracking_id_response"]] <- NA_real_
dat_notmatched[["is_dup"]] <- NA_real_
dat_notmatched[["comment"]] <- NA_character_

# Since there are only 6 rows, we can manually inspect each case and see how to
# handle each one
i <- 1
current_participant <- participants_having_notmatched[i]
dat_notmatched_for_inspection <- dat_notmatched %>% filter(mars_id == current_participant)
dat_merged_for_inspection <- dat_merged %>% filter(mars_id == current_participant) %>% filter(quick_survey_id == 23)
this_raw_tracking_id_response <- dat_merged_for_inspection[["raw_tracking_id_response"]]
# In both records, the value of cig_available and negative_affect were identical
# One trigger date time could be matched to two response date times (within seconds of each other)
# The reason why the record in the data frame dat_notmatched_for_inspection 
# was not matched in the for loop above is because the response date time was slightly after the status date time;
# by contrast, the record in dat_diagnose actually had their response date times lie before the status date time.
# Regard these rows as representing duplicate values of cig_available and negative_affect.
# The earlier response date time is taken as the match and actually has already been linked via the for loop above.
dat_notmatched <- dat_notmatched %>%
  mutate(paired_raw_tracking_id_response = replace(paired_raw_tracking_id_response, mars_id == current_participant, this_raw_tracking_id_response)) %>%
  mutate(is_dup = replace(is_dup, mars_id == current_participant, 1)) %>%
  mutate(comment = replace(comment, mars_id == current_participant, "Regard these rows as representing duplicate values of cig_available and negative_affect"))

i <- 2
current_participant <- participants_having_notmatched[i]
dat_notmatched_for_inspection <- dat_notmatched %>% filter(mars_id == current_participant)
dat_merged_for_inspection <- dat_merged %>% filter(mars_id == current_participant) %>% filter(quick_survey_id == 42)
this_raw_tracking_id_response <- dat_merged_for_inspection[["raw_tracking_id_response"]]
# In both records, the value of cig_available and negative_affect were identical
# One trigger date time could be matched to two response date times (within seconds of each other)
# The reason why the record in the data frame dat_notmatched_for_inspection 
# was not matched in the for loop above is because the response date time was slightly after the status date time;
# by contrast, the record in dat_diagnose actually had their response date times lie before the status date time.
# Regard these rows as representing duplicate values of cig_available and negative_affect.
# The earlier response date time is taken as the match and actually has already been linked via the for loop above.
dat_notmatched <- dat_notmatched %>%
  mutate(paired_raw_tracking_id_response = replace(paired_raw_tracking_id_response, mars_id == current_participant, this_raw_tracking_id_response)) %>%
  mutate(is_dup = replace(is_dup, mars_id == current_participant, 1)) %>%
  mutate(comment = replace(comment, mars_id == current_participant, "Regard these rows as representing duplicate values of cig_available and negative_affect"))

i <- 3
current_participant <- participants_having_notmatched[i]
dat_notmatched_for_inspection <- dat_notmatched %>% filter(mars_id == current_participant)
dat_merged_for_inspection <- dat_merged %>% filter(mars_id == current_participant) %>% filter(quick_survey_id == 45)
this_raw_tracking_id_response <- dat_merged_for_inspection[["raw_tracking_id_response"]]
# In both records, the value of cig_available and negative_affect were identical
# One trigger date time could be matched to two response date times (within seconds of each other)
# The reason why the record in the data frame dat_notmatched_for_inspection 
# was not matched in the for loop above is because the response date time was slightly after the status date time;
# by contrast, the record in dat_diagnose actually had their response date times lie before the status date time.
# Regard these rows as representing duplicate values of cig_available and negative_affect.
# The earlier response date time is taken as the match and actually has already been linked via the for loop above.
dat_notmatched <- dat_notmatched %>%
  mutate(paired_raw_tracking_id_response = replace(paired_raw_tracking_id_response, mars_id == current_participant, this_raw_tracking_id_response)) %>%
  mutate(is_dup = replace(is_dup, mars_id == current_participant, 1)) %>%
  mutate(comment = replace(comment, mars_id == current_participant, "Regard these rows as representing duplicate values of cig_available and negative_affect"))

i <- 4
current_participant <- participants_having_notmatched[i]
dat_notmatched_for_inspection <- dat_notmatched %>% filter(mars_id == current_participant)
dat_merged_for_inspection <- dat_merged %>% filter(mars_id == current_participant) %>% filter(quick_survey_id == 13)
this_raw_tracking_id_response <- dat_merged_for_inspection[["raw_tracking_id_response"]]
# In both records, the value of cig_available and negative_affect were identical
# One trigger date time could be matched to two response date times (within seconds of each other)
# The reason why the record in the data frame dat_notmatched_for_inspection 
# was not matched in the for loop above is because the response date time was slightly after the status date time;
# by contrast, the record in dat_diagnose actually had their response date times lie before the status date time.
# Regard these rows as representing duplicate values of cig_available and negative_affect.
# The earlier response date time is taken as the match and actually has already been linked via the for loop above.
dat_notmatched <- dat_notmatched %>%
  mutate(paired_raw_tracking_id_response = replace(paired_raw_tracking_id_response, mars_id == current_participant, this_raw_tracking_id_response)) %>%
  mutate(is_dup = replace(is_dup, mars_id == current_participant, 1)) %>%
  mutate(comment = replace(comment, mars_id == current_participant, "Regard these rows as representing duplicate values of cig_available and negative_affect"))

i <- 5
current_participant <- participants_having_notmatched[i]
dat_notmatched_for_inspection <- dat_notmatched %>% filter(mars_id == current_participant)
dat_merged_for_inspection <- dat_merged %>% filter(mars_id == current_participant) %>% filter(quick_survey_id == 44)
this_raw_tracking_id_response <- dat_merged_for_inspection[["raw_tracking_id_response"]]
# In both records, the value of cig_available and negative_affect were identical
# One trigger date time could be matched to two response date times (within seconds of each other)
# The reason why the record in the data frame dat_notmatched_for_inspection 
# was not matched in the for loop above is because the response date time was slightly after the status date time;
# by contrast, the record in dat_diagnose actually had their response date times lie before the status date time.
# Regard these rows as representing duplicate values of cig_available and negative_affect.
# The earlier response date time is taken as the match and actually has already been linked via the for loop above.
dat_notmatched <- dat_notmatched %>%
  mutate(paired_raw_tracking_id_response = replace(paired_raw_tracking_id_response, mars_id == current_participant, this_raw_tracking_id_response)) %>%
  mutate(is_dup = replace(is_dup, mars_id == current_participant, 1)) %>%
  mutate(comment = replace(comment, mars_id == current_participant, "Regard these rows as representing duplicate values of cig_available and negative_affect"))

i <- 6
# This case is not an instance of a duplicate value of cig_available and negative_affect
# It turns out that the 2QS trigger time it was supposed to be linked to 
# did not have a corresponding date time for status. 
# In the code below, we carry out linkage  "manually" and only use
# two date times to carry out the linkage,
# rather then three date times as in the loop above.
current_participant <- participants_having_notmatched[i]
dat_notmatched_for_inspection <- dat_notmatched %>% filter(mars_id == current_participant)
dat_merged_for_inspection <- dat_merged %>% filter(mars_id == current_participant) %>% filter(quick_survey_id == 2)
dat_notmatched <- dat_notmatched %>%
  mutate(paired_raw_tracking_id_response = replace(paired_raw_tracking_id_response, mars_id == current_participant, -999)) %>%
  mutate(is_dup = replace(is_dup, mars_id == current_participant, 0)) %>%
  mutate(comment = replace(comment, mars_id == current_participant, "This row should have been matched to a triggered 2QS."))

# Carry out the linkage "manually" for this one case
this_quick_survey_id <- 2
this_raw_tracking_id_response <- 2036
dat_participant_triggered <- dat_all_with_matched_status %>% filter(mars_id == current_participant) %>% filter(quick_survey_id == this_quick_survey_id)
dat_participant_response <- dat_response_to_triggered_2qs %>% filter(mars_id == current_participant) %>% filter(raw_tracking_id_response == this_raw_tracking_id_response)

# NOTE: We are using a RIGHT JOIN here!
dat_participant_triggered <- right_join(x = dat_participant_response,
                                        y = dat_participant_triggered,
                                        by = join_by(mars_id == mars_id,
                                                     closest(ts_responded_2qs_mountain > ts_2qs_triggered_mountain)
                                                     )
                                        )

# Manually update status: change from "maybe_missed" to "completed"
# This is an example of a case where "maybe_missed" did not necessarily mean that the participant did not complete the 2QS
# since we were eventually able to locate their responses and manually perform the linkage in this case.
dat_participant_triggered <- dat_participant_triggered %>% mutate(status_survey_2qs = replace(status_survey_2qs, quick_survey_id == this_quick_survey_id, "completed"))
# Now we can update the data
dat_merged <- dat_merged %>% filter(!((mars_id == current_participant) & (quick_survey_id == this_quick_survey_id)))
dat_merged <- rbind(dat_merged, dat_participant_triggered)
dat_merged <- dat_merged %>% arrange(mars_id, quick_survey_id)

#############################################################################
# Sanity check: perform this check again
# Were there any records in that were not matched? (discarded)
#############################################################################
tracking_ids_of_dups <- dat_notmatched %>% filter(is_dup == 1) %>% .[["raw_tracking_id_response"]]

tracking_orig <- dat_response_to_triggered_2qs[["raw_tracking_id_response"]]
tracking_orig <- setdiff(x = tracking_orig, y = tracking_ids_of_dups)  # remove tracking ID of rows which we regarded as representing duplicate values of cig_available and negative_affect

tracking_with_match <- unique(dat_merged[["raw_tracking_id_response"]])
tracking_with_nomatch <- setdiff(x = tracking_orig, y = tracking_with_match)
n_tracking_with_nomatch <- length(tracking_with_nomatch)   # There are no rows in dat_response_to_triggered_ema which have not been linked!

dat_notmatched <- dat_notmatched %>% filter(is_dup==1) %>% select(mars_id, raw_tracking_id_response, paired_raw_tracking_id_response, comment)
dat_designated_dups  <- rbind(dat_diagnose, dat_notmatched)
dat_designated_dups[["comment2"]] <- "keep paired_raw_tracking_id_response and discard raw_tracking_id_response"

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
# Save and move on to next step
#############################################################################
dat_decisions_2qs <- dat_designated_dups

dat_output <- dat_merged %>%
  select(-raw_tracking_id, -raw_tracking_id_response,
         -is_identical_olson, -olson_triggered, -olson_response, -olson_status) %>%
  select(mars_id, olson, block_number, block_bounds_mountain, quick_survey_id, ts_2qs_triggered_mountain,
         ts_status_survey_2qs_mountain, status_survey_2qs, ts_responded_2qs_mountain, cig_available, negative_affect, everything())


