###############################################################################
# Record when pipeline begins
###############################################################################

.start_time_pipe <- Sys.time()

###############################################################################
# Extract log files and ID's
###############################################################################

# Input: 
#   None
# Output: 
#   list_ids.rds
source("parse-grafana-ids.R")
rm(list = ls())

# Input: 
#   list_ids.rds
# Output: 
#   list_all_system_log.rds
#   list_all_loweffort_log.rds
source("unzip-files.R")
rm(list = ls())

###############################################################################
# Linking data and metadata elements: micro-randomization
###############################################################################

# Input: 
#   list_all_system_log.rds
# Output: 
#   list_dat_triggered_coinflip.rds
source("parse-micro-randomization-time.R")
rm(list = ls())

# Input: 
#   list_all_system_log.rds
# Output: 
#   list_all_emi.rds
source("parse-treatment-assignments.R")
rm(list = ls())

# Input: 
#   list_dat_triggered_coinflip.rds
#   list_all_emi.rds
# Output:  
#   dat_all_with_matched_treatment_assignments.rds
source("match-treatment-assignments.R")
rm(list = ls())

###############################################################################
# Linking data and metadata elements: brief survey
###############################################################################

# Input:
#   list_all_system_log.rds
# Output:
#   list_dat_triggered_2qs.rds
#   list_dat_triggered_ema.rds 
source("parse-survey-trigger-time.R")
rm(list = ls())

# Input:
#   list_all_loweffort_log.rds
# Output:
#   list_dat_response_to_triggered_2qs.rds
source("parse-responses-2qs.R")
rm(list = ls())

# Input:
#   list_all_system_log.rds
# Output:
#   list_status_survey_2qs.rds
#   list_status_survey_EMA.rds
source("parse-status-surveys.R")
rm(list = ls())

# Input:
#   brief_survey_linked_trigger_to_status.rds
#   list_dat_response_to_triggered_2qs.rds
# Output:
#   dat_all_with_matched_status_2qs.rds
source("match-2qs.R")
rm(list = ls())

###############################################################################
# Linking data and metadata elements: EMA
###############################################################################

# Input:
#   list_ids.rds
# Output:
#   dat_long_ema_responses.rds
source("parse-responses-ema.R")
rm(list = ls())

# Input:
#   dat_long_ema_responses.rds
# Output:
#   dat_master_ema_questions.rds
#   dat_conventional_long_format_ema_responses.rds
#   dat_primary_outcome.rds
source("reshape-parsed-ema.R")
rm(list = ls())

# Input:
#   ema_linked_trigger_to_status.rds
#   dat_conventional_long_format_ema_responses.rds
# Output:
#   dat_all_with_matched_status_ema.rds
source("match-ema.R")
rm(list = ls())

###############################################################################
# Linking data and metadata elements: 2QS, micro-randomization, EMA
###############################################################################

# Input:
#   dat_all_with_matched_status_2qs.rds
#   dat_all_with_matched_treatment_assignments.rds
#   dat_all_with_matched_status_ema.rds
# Output:
#   dat_all_burst.rds
#   dat_decisions_atypical_sequence.rds
source("stitch-together-block-level-data.R")
rm(list = ls())

# Input:
#   dat_all_burst.rds
#   dat_tracking_atypical_sequence.rds
# Output:
#   dat_cleaned_burst.rds
source("clean-up-atypical-sequences.R")
rm(list = ls())

###############################################################################
# Create V1 date variable and crosswalk between two types of ID's
###############################################################################

# Input:
#   list_ids.rds
#   Visit1_DataChecklist.rds
# Output:
#   crosswalk_grafana_rsr.rds
source("create-crosswalk-with-visit-data-ids.R")
rm(list = ls())

# Input:
#   list_ids.rds
#   crosswalk_grafana_rsr.rds
#   event_log.rds
# Output:
#   dat_visit_dates.rds
#   dat_visit_dates_V1_only.rds
source("create-visit-dates.R")
rm(list = ls())

###############################################################################
# Consolidate demographic variables from raw data
###############################################################################

# Input:
#   dat_visit_dates.rds
#   redcap_crosswalk.rds
#   redcap_metadata.rds
#   redcap_data.rds
# Output:
#   dat_demogs.rds
source("derive-demogs-from-raw-data.R")
rm(list = ls())

###############################################################################
# Preparatory steps for creating the analytical datasets
###############################################################################

# Input:
#   dat_visit_dates_V1_only.rds
#   dat_cleaned_burst.rds
# Output:
#   dat_cleaned_burst_with_outside_study_indicator.rds
#   dat_within_study.rds
#   dat_outside_study.rds
#   dat_matched_to_decision_points.rds
source("prepare-analytic-datasets/remove-out-of-study-period.R")
rm(list = ls())

# Input:
#   dat_matched_to_decision_points.rds
# Output:
#   mars_ids_pilot.rds
#   mars_ids_did_not_meet_ema_completion_criteria.rds
#   mars_ids_excluded_from_all_analytic_datasets.rds
source("prepare-analytic-datasets/screen-for-analytical-dataset-inclusion.R")
rm(list = ls())

###############################################################################
# Derive analytical datasets
# 
# Note:
#   The rows of all the datasets listed under 'Output' are subsets 
#   of the rows in dat_matched_to_decision_points.rds
#   since rows belonging to participants who were not in the data file
#   mars_ids_excluded_from_all_analytic_datasets.rds
#   were excluded.
###############################################################################

# Input:
#   dat_matched_to_decision_points.rds
#   mars_ids_excluded_from_all_analytic_datasets.rds
#   mars_ids_pilot.rds
# Output:
#   dat_mars_for_ema_assessed_proximal_outcome.rds
#   dat_mars_for_nonema_assessed_proximal_outcome.rds
source("prepare-analytic-datasets/create-basic-analysis-dataset.R")
rm(list = ls())

###############################################################################
# Record when pipeline is completed
###############################################################################
.end_time_pipe <- Sys.time()

.total_time_pipe <- difftime(time1 = .end_time_pipe, time2 = .start_time_pipe, units = "mins")

print(.total_time_pipe)

