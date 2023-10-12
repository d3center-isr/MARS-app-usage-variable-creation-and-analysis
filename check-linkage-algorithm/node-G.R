rm(list = ls())

source("paths.R") 
library(tidyverse)

dat_all_burst <- readRDS(file = file.path(path_manipulated_data, "dat_all_burst.rds"))
dat_decisions_atypical_sequences <- readRDS(file = file.path(path_manipulated_data, "dat_decisions_atypical_sequences.rds"))

drop_these_sequences <- dat_decisions_atypical_sequences[["paired_with_sequence_id"]]
dat_cleaned_burst <- dat_all_burst %>% filter(!(stitched_sequence_id %in% drop_these_sequences))

dat_output <- dat_cleaned_burst %>% 
  select(-stitched_sequence_id, -is_many_coinflip, -is_many_ema, -group_id, -group_id_lead1, -is_paired, -ts_now_mountain, -ts_ahead_mountain) %>%
  select(mars_id, olson, everything())

