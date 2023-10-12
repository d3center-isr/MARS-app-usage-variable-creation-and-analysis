rm(list = ls())

source("paths.R")
library(tidyverse)

dat_matched_to_decision_points <- readRDS(file = file.path(path_manipulated_data, "dat_matched_to_decision_points.rds"))  # Note that this data frame already does NOT include any of the participants in mars_ids_did_not_have_mhealth_data_within_mrt_period
mars_ids_did_not_have_mhealth_data_within_mrt_period <- readRDS(file = file.path(path_manipulated_data, "mars_ids_did_not_have_mhealth_data_within_mrt_period.rds"))
mars_ids_pilot <- c("mars_1","mars_2","mars_3")  # These are the pilot participants


dat_summary <- dat_matched_to_decision_points %>%
  filter(!(mars_id %in% mars_ids_pilot)) %>%
  filter((decision_point >= 7) & (decision_point <= 54)) %>%
  group_by(mars_id) %>%
  summarise(total_ema_administered = sum(!is.na(ts_ema_triggered_mountain)),
            total_ema_completed = sum(status_survey_ema == "completed", na.rm = TRUE),
            total_micro_randomizations = sum(!is.na(A))) %>%
  arrange(total_ema_completed, total_micro_randomizations)

dat_summary_for_exclusion <- dat_summary %>% filter(total_ema_completed < 3)
mars_ids_did_not_meet_ema_completion_criteria <- dat_summary_for_exclusion %>% .[["mars_id"]]

mars_ids_excluded_from_all_analytic_datasets <- c(mars_ids_pilot, 
                                                  mars_ids_did_not_have_mhealth_data_within_mrt_period,
                                                  mars_ids_did_not_meet_ema_completion_criteria)

saveRDS(mars_ids_pilot, file = file.path(path_manipulated_data, "mars_ids_pilot.rds"))
saveRDS(mars_ids_did_not_meet_ema_completion_criteria, file = file.path(path_manipulated_data, "mars_ids_did_not_meet_ema_completion_criteria.rds"))
saveRDS(mars_ids_excluded_from_all_analytic_datasets, file = file.path(path_manipulated_data, "mars_ids_excluded_from_all_analytic_datasets.rds"))

