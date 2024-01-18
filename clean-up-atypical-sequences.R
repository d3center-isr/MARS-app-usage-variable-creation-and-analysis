source("paths.R") 
library(tidyverse)

source("check-linkage-algorithm/node-G.R")

###############################################################################
# Handle participants having what appears to be a glitch in the recording of
# their timezone (i.e., handling the 'olson' variable)
###############################################################################
dat_cleaned_burst <- dat_output

dat_cleaned_burst <- dat_cleaned_burst %>%
  mutate(olson = replace(olson, mars_id == "mars_147", "US/Pacific")) %>%
  mutate(olson = replace(olson, mars_id == "mars_152", "US/Pacific")) %>%
  mutate(shifted_forward_block_bounds_mountain = (int_start(block_bounds_mountain) + hours(1)) %--% (int_end(block_bounds_mountain) + hours(1))) %>%
  select(mars_id, olson, block_number, block_bounds_mountain, shifted_forward_block_bounds_mountain, everything())

dat_cleaned_burst <- dat_cleaned_burst %>%
  mutate(block_bounds_mountain = if_else(mars_id == "mars_147", shifted_forward_block_bounds_mountain, block_bounds_mountain)) %>%
  mutate(block_bounds_mountain = if_else(mars_id == "mars_152", shifted_forward_block_bounds_mountain, block_bounds_mountain))

dat_cleaned_burst <- dat_cleaned_burst %>% select(-shifted_forward_block_bounds_mountain)

###############################################################################
# Save clean output
###############################################################################
saveRDS(dat_cleaned_burst, file = file.path(path_manipulated_data, "dat_cleaned_burst.rds"))
