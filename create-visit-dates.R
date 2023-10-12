rm(list = ls())

###############################################################################
# Create V1 start date
###############################################################################

source("paths.R")
library(tidyverse)

# The event log file contains study staff note keeping on 
# participant milestones within the study, including completion of activities
# associated with each study visit
event_log <- readRDS(file = file.path(path_visit_data, "event_log.rds"))

# Read in parsed Grafana ID's
list_ids <- readRDS(file = file.path(path_manipulated_data, "list_ids.rds"))
# Read in crosswalk between RSR ID's and Grafana ID's
crosswalk_grafana_rsr <- readRDS(file.path(path_manipulated_data, "crosswalk_grafana_rsr.rds"))

arr_ids <- unlist(list_ids)

# Grab dates corresponding to the following three event types:
#      "Consent-rcvd"
#      "V1 Metrics-complete"
#      "V1 Brief Quitting Advice"
# And then create a separate data frame for each event type.
v1_useful_events <- event_log %>%
  rename(event_type = EventType) %>%
  mutate(include_row = case_when(
    event_type == "Consent-rcvd" ~ 1,
    event_type == "V1 Metrics-complete" ~ 1,
    event_type == "V1 Brief Quitting Advice" ~ 1,
    .default = 0
  )) %>%
  filter(include_row == 1) %>%
  select(-include_row)

# Dates are in the UTC time zone, but they really 
# pertain to local time in US/Mountain
# Hence, before we may use dates/date-times correctly, we call force_tz() 
# and set the argument tz to be "US/Mountain"
v1_useful_events <- v1_useful_events %>%
  mutate(event_start_date = date(StartDate)) %>%
  mutate(event_start_date = force_tz(event_start_date, tz = "US/Mountain"))

wide1 <- v1_useful_events %>% filter(event_type == "Consent-rcvd") %>% select(EventID, SubjectID, v1_consent_rcvd = event_start_date)
wide2 <- v1_useful_events %>% filter(event_type == "V1 Metrics-complete") %>% select(EventID, SubjectID, v1_metrics_complete = event_start_date)
wide3 <- v1_useful_events %>% filter(event_type == "V1 Brief Quitting Advice") %>% select(EventID, SubjectID, v1_bqa = event_start_date)

###############################################################################
# Perform a sanity check: each participant is expected to have only 1 date
# per event type; do we see that this is the case in the data?
###############################################################################

wide1 <- wide1 %>% arrange(EventID)
wide2 <- wide2 %>% arrange(EventID)
wide3 <- wide3 %>% arrange(EventID)

wide1[["is_dup"]] <- duplicated(wide1[["SubjectID"]])
wide2[["is_dup"]] <- duplicated(wide2[["SubjectID"]])
wide3[["is_dup"]] <- duplicated(wide3[["SubjectID"]])

v1_count_dat_dup <- data.frame(`consent_rcvd` = sum(1*wide1[["is_dup"]]), 
                               `v1_metrics_complete` = sum(1*wide2[["is_dup"]]), 
                               `v1_bqa` = sum(1*wide3[["is_dup"]]))

# Duplicate found here
print(v1_count_dat_dup)

###############################################################################
# Implement case-by-case decision to any duplicates found
###############################################################################

subj_ids_with_dup_1 <- wide1 %>% filter(is_dup == TRUE) %>% .[["SubjectID"]]
subj_ids_with_dup_2 <- wide2 %>% filter(is_dup == TRUE) %>% .[["SubjectID"]]
subj_ids_with_dup_3 <- wide3 %>% filter(is_dup == TRUE) %>% .[["SubjectID"]]

subj_ids_with_dup <- unique(c(subj_ids_with_dup_1, subj_ids_with_dup_2, subj_ids_with_dup_3))

# Inspect event_log
v1_dup_cases_event_log <- event_log %>% 
  filter(SubjectID %in% subj_ids_with_dup) %>%
  filter(EventType %in% c("Consent-rcvd","V1 Metrics-complete","V1 Brief Quitting Advice")) %>%
  arrange(SubjectID, EventID, EventType, StartDate) %>%
  select(EventID, Study, EventType, SubjectID, StartDate, EndDate, LastEditDate)

print(v1_dup_cases_event_log)

# Scenario: There is 1 participant who had two recorded dates for the event
# type `consent_rcvd`. The earlier recorded date corresponds to the date when
# informed consent was received; the later recorded date corresponds to the
# date when an administraSince the later recorded date merely corresponds to
# an administrative update and the earlier date actually corresponded to
# the date when study staff received the participant's consent,
# we use the StartDate corresponding to the earlier EventID
wide1 <- wide1 %>% arrange(EventID) %>% filter(is_dup == FALSE)

###############################################################################
# Create wide format dataset with dates for each event type side-by-side
###############################################################################

wide1 <- wide1 %>% select(-is_dup) %>% rename(EventID_v1_consent_rcvd = EventID)
wide2 <- wide2 %>% select(-is_dup) %>% rename(EventID_v1_metrics_complete = EventID)
wide3 <- wide3 %>% select(-is_dup) %>% rename(EventID_v1_bqa = EventID)

wide_v1_useful_events <- left_join(x = wide1, y = wide2, by = "SubjectID")
wide_v1_useful_events <- left_join(x = wide_v1_useful_events, y = wide3, by = "SubjectID")
wide_v1_useful_events <- wide_v1_useful_events %>% rename(rsr_id = SubjectID)
wide_v1_useful_events <- left_join(x = crosswalk_grafana_rsr, y = wide_v1_useful_events, by = "rsr_id")

###############################################################################
# Perform checks on the RSR dates
###############################################################################

wide_v1_useful_events <- wide_v1_useful_events %>%
  mutate(all_v1_exist = if_else((!is.na(v1_consent_rcvd)) & (!is.na(v1_metrics_complete)) & (!is.na(v1_bqa)), 1, 0)) %>%
  mutate(are_any_rsr_v1_dates_missing = is.na(v1_consent_rcvd) | is.na(v1_metrics_complete) | is.na(v1_bqa)) %>%
  mutate(are_rsr_v1_dates_identical = case_when(
    (are_any_rsr_v1_dates_missing == FALSE) & (v1_consent_rcvd == v1_metrics_complete) & (v1_metrics_complete == v1_bqa) ~ TRUE,
    (are_any_rsr_v1_dates_missing == FALSE) & ((v1_consent_rcvd != v1_metrics_complete) | (v1_metrics_complete != v1_bqa) | (v1_consent_rcvd != v1_bqa)) ~ FALSE,
    .default = NULL
  ))

# Tabulate number of instances where dates are not identical
wide_v1_useful_events %>% 
  group_by(are_rsr_v1_dates_identical, are_any_rsr_v1_dates_missing, .groups = "keep") %>% 
  summarise(n())

# Since there are ZERO instances where the three dates are not identical, 
# we can simply set v1_date_began to v1_metrics_complete
wide_v1_useful_events <- wide_v1_useful_events %>%
  mutate(v1_date_began = v1_metrics_complete)

###############################################################################
# Prepare to output
###############################################################################
dat_visit_dates <- wide_v1_useful_events

dat_visit_dates[["pilot_participant"]] <- if_else(dat_visit_dates[["mars_id"]] %in% c("mars_1","mars_2","mars_3"), TRUE, FALSE)

dat_visit_dates <- dat_visit_dates %>%
  select(rsr_id, mars_id, pilot_participant, Remote_Enrolled, md2k_directory_exist, v1_date_began, everything()) %>% 
  arrange(desc(md2k_directory_exist)) %>%
  rename(remote_enrolled = Remote_Enrolled)

dat_visit_dates_v1_only <- dat_visit_dates %>%
  select(rsr_id, mars_id, pilot_participant, remote_enrolled, md2k_directory_exist, v1_date_began)

###############################################################################
# Save output
###############################################################################

saveRDS(dat_visit_dates, file = file.path(path_manipulated_data, "dat_visit_dates.rds"))
saveRDS(dat_visit_dates_v1_only, file = file.path(path_manipulated_data, "dat_visit_dates_V1_only.rds"))
