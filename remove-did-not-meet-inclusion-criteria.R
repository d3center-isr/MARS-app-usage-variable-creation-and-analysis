source("paths.R")
library(tidyverse)

################################################################################
# Load datasets
################################################################################
# This dataset contains data from only those sequences which began
# within the 10-day MRT period.
dat_matched_to_decision_points <- readRDS(file = file.path(path_manipulated_data, "dat_matched_to_decision_points.rds"))

################################################################################
# The following participants were excluded from analyses:
# - Pilot participants
# - Among participants who have any data from sequences which began within the
#   10-day MRT period, those participants who did not complete
#   at least 3 EMA between the 2nd and 9th day inclusive.
#
# Note:  This subset of the data may be used if the proximal outcome of
#        interest was assessed via EMAs.
################################################################################
mars_ids_excluded_from_all_analytic_datasets <- readRDS(file = file.path(path_manipulated_data, "mars_ids_excluded_from_all_analytic_datasets.rds"))

dat_mars_for_ema_assessed_proximal_outcome <- dat_matched_to_decision_points %>% filter(!(mars_id %in% mars_ids_excluded_from_all_analytic_datasets))

saveRDS(dat_mars_for_ema_assessed_proximal_outcome, file = file.path(path_manipulated_data, "dat_mars_for_ema_assessed_proximal_outcome.rds"))

################################################################################
# The following participants were excluded from analyses:
# - Pilot participants
#
# Note: This subset of the data may be used if the proximal outcome of
#       interest was NOT assessed via EMAs, e.g., app usage.
################################################################################
mars_ids_pilot <- readRDS(file = file.path(path_manipulated_data, "mars_ids_pilot.rds"))

dat_mars_for_nonema_assessed_proximal_outcome <- dat_matched_to_decision_points %>% filter(!(mars_id %in% mars_ids_pilot))

saveRDS(dat_mars_for_nonema_assessed_proximal_outcome, file = file.path(path_manipulated_data, "dat_mars_for_nonema_assessed_proximal_outcome.rds"))

