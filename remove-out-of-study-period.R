rm(list = ls())

source("paths.R")
library(tidyverse)

dat_visit_dates <- readRDS(file = file.path(path_manipulated_data, "dat_visit_dates_V1_only.rds"))
dat_cleaned_burst <- readRDS(file = file.path(path_manipulated_data, "dat_cleaned_burst.rds"))

###############################################################################
# First, remove observations outside of study period
###############################################################################
dat_study_bounds <- dat_visit_dates %>% 
  select(mars_id, v1_date_began) %>%
  mutate(v1_date_began = v1_date_began + seconds(1)) %>%  # -- This will help serve as a visual cue when performing spot checks that v1_date_began is in fact a variable of type date-time
  mutate(v1_date_began_plus_nine = v1_date_began + days(9) + hours(23) + minutes(59) + seconds(58))  # -- This will help serve as a visual cue when performing spot checks that v1_date_began_plus_nine is in fact a variable of type date-time

all_ids <- unique(dat_cleaned_burst[["mars_id"]])
list_all_burst <- list()

for(i in 1:length(all_ids)){
  current_participant <- all_ids[i]
  dat_current_burst <- dat_cleaned_burst %>% filter(mars_id == current_participant)
  dat_current_study_bounds <- dat_study_bounds %>% filter(mars_id == current_participant)
  
  # Note that these dates are in mountain time in the raw data.
  # These are arrays of length 1
  v1 <- dat_current_study_bounds[["v1_date_began"]]
  v1_plus_nine <- dat_current_study_bounds[["v1_date_began_plus_nine"]]
  
  # Using force_tz and setting the tzone argument to UTC is a trick to work with 
  # just a single column that could possibly represent date times from multiple time zones
  dat_current_burst <- dat_current_burst %>%
    mutate(block_start_mountain = int_start(block_bounds_mountain)) %>%
    mutate(block_start_pacific = with_tz(time = block_start_mountain, tzone = "US/Pacific"),
           block_start_central = with_tz(time = block_start_mountain, tzone = "US/Central"),
           block_start_eastern = with_tz(time = block_start_mountain, tzone = "US/Eastern")) %>%
    mutate(block_start_local = case_when(
      olson == "US/Mountain" ~ force_tz(time = block_start_mountain, tzone = "UTC"),
      olson == "US/Pacific" ~ force_tz(time = block_start_pacific, tzone = "UTC"),
      olson == "US/Central" ~ force_tz(time = block_start_central, tzone = "UTC"),
      olson == "US/Eastern" ~ force_tz(time = block_start_eastern, tzone = "UTC"),
      .default = NULL
    ))
  
  dat_current_burst <- dat_current_burst %>%
    mutate(v1_date_began_mountain = v1) %>%
    mutate(v1_date_began_local = force_tz(time = v1_date_began_mountain, tzone = "UTC")) %>%
    mutate(v1_date_began_plus_nine_mountain = v1_plus_nine) %>% 
    mutate(v1_date_began_plus_nine_local = force_tz(time = v1_date_began_plus_nine_mountain, tzone = "UTC"))
  
  dat_current_burst <- dat_current_burst %>%
    mutate(is_outside_study = if_else((block_start_local < v1_date_began_local) | (block_start_local > v1_date_began_plus_nine_local), 1, 0))
  
  list_all_burst <- append(list_all_burst, list(dat_current_burst))
}

dat_cleaned_burst_with_outside_study_indicator <- bind_rows(list_all_burst)

###############################################################################
# First, remove observations outside of study period
###############################################################################
dat_within_study <- dat_cleaned_burst_with_outside_study_indicator %>% filter(is_outside_study == 0) %>% select(-is_outside_study)
dat_outside_study <- dat_cleaned_burst_with_outside_study_indicator %>% filter(is_outside_study == 1) %>% select(-is_outside_study)

###############################################################################
# Were there any participants removed as a result of this step?
###############################################################################
ids_present_raw_data <- unique(dat_cleaned_burst[["mars_id"]])
ids_present_after_dropping_out_of_mrt_period_rows <- unique(dat_within_study[["mars_id"]])
ids_absent_from_within_mrt_period_rows <- setdiff(x = ids_present_raw_data, y = ids_present_after_dropping_out_of_mrt_period_rows)  # There is only one participant who did not make the cut

###############################################################################
# For mars_53, there is one day for which block 4 and 5 came before block 1
# (block numbers indexed by zero). We note that there is no two-question survey
# associated with blocks 0, 2, 3.
# This phenomenon is likely due to an adjustment of wake up time that happened 
# on that day.
# Decision: Discard block 4 and 5 data but keep block 1 data.
###############################################################################
dat_within_study <- dat_within_study %>%
  mutate(drop_due_to_changed_wakeup = if_else((mars_id == "mars_53") & ((quick_survey_id == 1) | (quick_survey_id == 2)), 1, 0))

dat_within_study_copy <- dat_within_study %>% filter(drop_due_to_changed_wakeup == 0) %>% select(-drop_due_to_changed_wakeup)

###############################################################################
# Next, match each sequence to decision points
###############################################################################
dat_within_study_copy <- dat_within_study_copy %>% 
  mutate(decision_point = (6 * floor(int_length(v1_date_began_local %--% block_start_local)/(60*60*24))) + (block_number + 1)) %>%
  select(mars_id, olson, decision_point, everything())

###############################################################################
# Fill in the gaps for those decision points not represented in the raw data
###############################################################################
all_ids_within_study <- unique(dat_within_study_copy[["mars_id"]])

skeleton <- data.frame(mars_id = rep(x = all_ids_within_study, each = 60),
                       decision_point = rep(x = 1:60, times = length(all_ids_within_study)))

dat_matched_to_decision_points <- left_join(x = skeleton, y = dat_within_study_copy, by = join_by(mars_id == mars_id, decision_point == decision_point))
dat_matched_to_decision_points <- dat_matched_to_decision_points %>% mutate(cluster_id = ceiling(decision_point/6)) %>% select(mars_id, olson, cluster_id, decision_point, everything())

###############################################################################
# Save output
###############################################################################
saveRDS(ids_absent_from_within_mrt_period_rows, file = file.path(path_manipulated_data, "mars_ids_did_not_have_mhealth_data_within_mrt_period.rds"))
saveRDS(dat_within_study, file = file.path(path_manipulated_data, "dat_within_study.rds"))
saveRDS(dat_within_study_copy, file = file.path(path_manipulated_data, "dat_within_study_after_dropped_due_to_changed_wakeup.rds"))
saveRDS(dat_outside_study, file = file.path(path_manipulated_data, "dat_outside_study.rds"))
saveRDS(dat_cleaned_burst_with_outside_study_indicator, file = file.path(path_manipulated_data, "dat_cleaned_burst_with_outside_study_indicator.rds"))
saveRDS(dat_matched_to_decision_points, file = file.path(path_manipulated_data, "dat_matched_to_decision_points.rds"))

