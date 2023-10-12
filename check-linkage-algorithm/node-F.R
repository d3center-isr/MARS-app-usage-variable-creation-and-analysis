rm(list = ls())

source("paths.R") 
library(tidyverse)

dat_all_with_matched_status_2qs <- readRDS(file = file.path(path_manipulated_data, "dat_all_with_matched_status_2qs.rds"))
orig_colnames <- colnames(dat_all_with_matched_status_2qs)

dat_all_with_matched_treatment_assignments <- readRDS(file = file.path(path_manipulated_data, "dat_all_with_matched_treatment_assignments.rds"))
trt_colnames <- colnames(dat_all_with_matched_treatment_assignments)

dat_all_with_matched_status_ema <- readRDS(file = file.path(path_manipulated_data, "dat_all_with_matched_status_ema.rds"))
ema_colnames <- colnames(dat_all_with_matched_status_ema)

dat_all_with_matched_treatment_assignments[["tracking_id_treatment_assignments"]] <- 1:nrow(dat_all_with_matched_treatment_assignments)
dat_all_with_matched_status_ema[["tracking_id_ema"]] <- 1:nrow(dat_all_with_matched_status_ema)

all_ids <- unique(dat_all_with_matched_status_2qs[["mars_id"]])

###############################################################################
# Match micro-randomization data and metadata to the block they originated from
###############################################################################
list_dat_all_burst <- list()

for(i in 1:length(all_ids)){
  current_participant <- all_ids[i]
  dat_current_2qs <- dat_all_with_matched_status_2qs %>% filter(mars_id == current_participant)
  dat_current_coinflip <- dat_all_with_matched_treatment_assignments %>% filter(mars_id == current_participant)
  
  dat_current_2qs <- dat_current_2qs %>%
    arrange(ts_2qs_triggered_mountain) %>%
    mutate(ts_now_mountain = ts_2qs_triggered_mountain) %>%
    mutate(ts_ahead_mountain = lead(ts_2qs_triggered_mountain))
  
  # This line deals with the very last 2QS triggered which will not have any 2QS triggered after it
  dat_current_2qs[["ts_ahead_mountain"]][nrow(dat_current_2qs)] <- dat_current_2qs[["ts_now_mountain"]][nrow(dat_current_2qs)] + minutes(140)
  
  # Determine which micro-randomization falls in between 2 consecutive 2QS
  # NOTE: We are using a RIGHT JOIN here!
  dat_current_2qs <- right_join(x = dat_current_coinflip %>% select(-olson),
                                y = dat_current_2qs %>% rename(block_number_2qs = block_number),
                                by = join_by(mars_id == mars_id,
                                             between(x = ts_coinflip_mountain,
                                                     y_lower = ts_now_mountain,
                                                     y_upper = ts_ahead_mountain,
                                                     bounds = "[)")
                                             )
                                )
  
  list_dat_all_burst <- append(list_dat_all_burst, list(dat_current_2qs))
}

dat_all_burst <- bind_rows(list_dat_all_burst)

###############################################################################
# Match EMA data and metadata to the block they originated from
###############################################################################
list_dat_all_burst <- list()

for(i in 1:length(all_ids)){
  current_participant <- all_ids[i]
  dat_current_2qs <- dat_all_burst %>% filter(mars_id == current_participant)
  dat_current_ema <- dat_all_with_matched_status_ema %>% filter(mars_id == current_participant)
  
  # Determine which EMA falls in between 2 consecutive 2QS
  # NOTE: We are using a RIGHT JOIN here!
  dat_current_2qs <- right_join(x = dat_current_ema %>% select(-olson) %>% rename(block_number_ema = block_number),
                                y = dat_current_2qs,
                                by = join_by(mars_id == mars_id,
                                             between(x = ts_ema_triggered_mountain,
                                                     y_lower = ts_now_mountain,
                                                     y_upper = ts_ahead_mountain,
                                                     bounds = "[)")
                                             )
                                )
  
  # Detect many-to-one matches: micro-randomization
  dat_current_2qs <- dat_current_2qs %>% arrange(ts_2qs_triggered_mountain)
  tmp1 <- dat_current_2qs %>% filter(is.na(coinflip_id))
  tmp2 <- dat_current_2qs %>% filter(!is.na(coinflip_id))
  list_tmp <- list()
  if(nrow(tmp1) > 0){
    tmp1[["is_many_coinflip"]] <- NA
    list_tmp <- append(list_tmp, list(tmp1))
  }
  if(nrow(tmp2) > 0){
    tmp2[["is_many_coinflip"]] <- duplicated(tmp2[["coinflip_id"]])
    list_tmp <- append(list_tmp, list(tmp2))
  }
  dat_current_2qs <- bind_rows(list_tmp)
  
  # Detect many-to-one matches: EMA
  dat_current_2qs <- dat_current_2qs %>% arrange(ts_2qs_triggered_mountain)
  tmp1 <- dat_current_2qs %>% filter(is.na(ema_survey_id))
  tmp2 <- dat_current_2qs %>% filter(!is.na(ema_survey_id))
  list_tmp <- list()
  if(nrow(tmp1) > 0){
    tmp1[["is_many_ema"]] <- NA
    list_tmp <- append(list_tmp, list(tmp1))
  }
  if(nrow(tmp2) > 0){
    tmp2[["is_many_ema"]] <- duplicated(tmp2[["ema_survey_id"]])
    list_tmp <- append(list_tmp, list(tmp2))
  }
  dat_current_2qs <- bind_rows(list_tmp)
  dat_current_2qs <- dat_current_2qs %>% arrange(quick_survey_id)
  
  list_dat_all_burst <- append(list_dat_all_burst, list(dat_current_2qs))
}

dat_all_burst <- bind_rows(list_dat_all_burst)

#############################################################################
# Sanity check: do block numbers from two data sources match up?
# Answer: All match up!
#############################################################################
if(FALSE){  # Change to TRUE to execute this line
  sum(dat_all_burst[["block_number_2qs"]] != dat_all_burst[["block_number_ema"]], na.rm=TRUE)
}

dat_all_burst <- dat_all_burst %>%
  mutate(block_number = block_number_2qs) %>%
  select(-block_number_2qs, -block_number_ema)

#############################################################################
# Reorder columns and rows
# Important: Do not chance this code snippet because Check 1 and Check 2 rely
# on this ordering of rows
#############################################################################
dat_all_burst <- dat_all_burst %>% 
  arrange(mars_id, quick_survey_id, coinflip_id, ema_survey_id) %>%
  mutate(stitched_sequence_id = 1:nrow(.))

dat_all_burst <- dat_all_burst %>% 
  select(stitched_sequence_id,
         is_many_coinflip, is_many_ema, 
         tracking_id_treatment_assignments, tracking_id_ema,
         all_of(orig_colnames), all_of(trt_colnames), all_of(ema_colnames),
         everything())

#############################################################################
# Check 1: Are there any atypical sequences?
# Answer: Yes
#############################################################################
if(FALSE){  # Change to TRUE to execute this line
  n_atypical_sequences <- dat_all_burst %>%
    group_by(is_many_coinflip, is_many_ema) %>%
    summarise(count = n()) %>%
    filter((is_many_coinflip == TRUE) | (is_many_ema == TRUE)) %>%
    ungroup(.) %>%
    summarise(total = sum(count)) %>%
    unlist(.)
  
  # Since there are only 5 cases, we can manually inspect each one
  print(n_atypical_sequences)
  
  tmp <- dat_all_burst %>%
    filter((is_many_coinflip == TRUE) | (is_many_ema == TRUE))
}

dat_document_atypical_sequence1 <- data.frame(
  stitched_sequence_id = c(341, 548, 554, 2060, 5325),
  paired_with_sequence_id = c(341-1, 548-1, 554-1, 2060-1, 5325-1),
  sequence_type = c("atypical sequence: short survey administered (just once), micro-randomized (just once), EMA administered (1st time), EMA administered (2nd time)",  # check
                    "atypical sequence: short survey administered (just once), micro-randomized (1st time), micro-randomized (2nd time), EMA administered (just once)",  # check
                    "atypical sequence: short survey administered (just once), micro-randomized (1st time), micro-randomized (2nd time), EMA administered (just once)",  # check
                    "atypical sequence: short survey administered (just once), micro-randomized (1st time), micro-randomized (2nd time), EMA administered (just once)",  # check
                    "atypical sequence: short survey administered (just once), micro-randomized (1st time), micro-randomized (2nd time), EMA administered (just once)"), # check
  
  comment = c("EMA administered (1st time) -- status was 'maybe_missed'; moreover, no response to any item was recorded, EMA administered (2nd time) -- status was 'completed'",  # check
              "micro-randomized (1st time) and micro-randomized (2nd time) resulted in two DIFFERENT treatment assignments",   # check
              "micro-randomized (1st time) and micro-randomized (2nd time) resulted in two identical treatment assignments",   # check
              "micro-randomized (1st time) and micro-randomized (2nd time) resulted in two identical treatment assignments",   # check
              "micro-randomized (1st time) and micro-randomized (2nd time) resulted in two identical treatment assignments"))  # check

affected_sequence_ids <- c(dat_document_atypical_sequence1[["stitched_sequence_id"]], dat_document_atypical_sequence1[["paired_with_sequence_id"]])

#############################################################################
# Check 2: Are there more atypical sequences?
# Answer: Yes
#############################################################################
dat_diagnose <- dat_all_burst %>%
  mutate(block_start_datetime_mountain = int_start(block_bounds_mountain)) %>%
  mutate(block_start_date_mountain = date(block_start_datetime_mountain)) %>%
  filter(!(stitched_sequence_id %in% affected_sequence_ids)) %>%
  group_by(mars_id, block_start_date_mountain, block_number) %>%
  summarise(count = n(),
            first_time = min(stitched_sequence_id),
            last_time = max(stitched_sequence_id)) %>%
  mutate(gap = last_time - first_time) %>%
  filter(count > 1) %>%
  arrange(gap, mars_id)

if(FALSE){
  # Since there are only 10 such cases, we can manually inspect each one
  # After inspecting each case visually, we find that the last two cases
  # actually represent typical sequences (just convert block bounds from
  # mountain time to local time in the olson column to see this).
  # The other eight cases do actually represent atypical sequences.
  print(nrow(dat_diagnose))
}

dat_document_atypical_sequence2 <- data.frame(stitched_sequence_id = c(1170, 1231, 1453, 2178, 2819, 2867, 4932, 5169),
                                              paired_with_sequence_id = c(1170-1, 1231-1, 1453-1, 2178-1, 2819-1, 2867-1, 4932-1, 5169-1),
                                              sequence_type = c("atypical sequence: short survey administered (1st time), short survey administered (2nd time), micro-randomized (just once), no EMA administered",  # check
                                                                "atypical sequence: short survey administered (1st time), short survey administered (2nd time), micro-randomized (just once), EMA administered (just once)",  # check
                                                                "atypical sequence: short survey administered (1st time), short survey administered (2nd time), micro-randomized (just once), EMA administered (just once)",  # check
                                                                "atypical sequence: short survey administered (1st time), short survey administered (2nd time), no micro-randomization, no EMA administered",  # check
                                                                "atypical sequence: short survey administered (1st time), short survey administered (2nd time), micro-randomized (just once), no EMA administered",  # check
                                                                "atypical sequence: short survey administered (1st time), micro-randomized (1st time), short survey administered (2nd time), micro-randomized (2nd time), EMA administered (just once)",  # check
                                                                "atypical sequence: short survey administered (1st time), short survey administered (2nd time), no micro-randomization, no EMA administered",  # check
                                                                "atypical sequence: short survey administered (1st time), micro-randomized (1st time), short survey administered (2nd time), micro-randomized (2nd time), no EMA administered"),
                                              comment = c(NA_character_,  # check
                                                          NA_character_,  # check
                                                          NA_character_,  # check
                                                          NA_character_,  # check
                                                          NA_character_,  # check
                                                          "micro-randomized (1st time) and micro-randomized (2nd time) resulted in two different treatment assignments",  # check
                                                          NA_character_,  # check
                                                          "micro-randomized (1st time) and micro-randomized (2nd time) resulted in two identical treatment assignments"))  # check

dat_tracking_atypical_sequence <- rbind(dat_document_atypical_sequence1, dat_document_atypical_sequence2)
n_atypical_sequence <- nrow(dat_tracking_atypical_sequence)

#############################################################################
# Were there any records in that were not matched? (discarded)
#############################################################################
tracking_orig_treatment_assignments <- dat_all_with_matched_treatment_assignments[["tracking_id_treatment_assignments"]]
tracking_with_match_treatment_assignments <- unique(dat_all_burst[["tracking_id_treatment_assignments"]])
tracking_with_nomatch_treatment_assignments <- setdiff(x = tracking_orig_treatment_assignments, y = tracking_with_match_treatment_assignments)
n_tracking_with_nomatch_treatment_assignments <- length(tracking_with_nomatch_treatment_assignments)

tracking_orig_ema <- dat_all_with_matched_status_ema[["tracking_id_ema"]]
tracking_with_match_ema <- unique(dat_all_burst[["tracking_id_ema"]])
tracking_with_nomatch_ema <- setdiff(x = tracking_orig_ema, y = tracking_with_match_ema)
n_tracking_with_nomatch_ema <- length(tracking_with_nomatch_ema)

#############################################################################
# If two rows represent the same block, this is where they get assigned the
# same identifier
#############################################################################

list_dat_all_burst <- list()
ids_paired <- dat_tracking_atypical_sequence[["paired_with_sequence_id"]]

for(i in 1:length(all_ids)){
  current_participant <- all_ids[i]
  dat_current <- dat_all_burst %>% filter(mars_id == current_participant) %>% arrange(stitched_sequence_id)
  dat_current <- dat_current %>% mutate(is_paired = if_else(stitched_sequence_id %in% ids_paired, 1, 0))
  any_paired <- sum(dat_current[["is_paired"]])
  if(any_paired > 0){
    dat_current_no <- dat_current %>% filter(is_paired == 0) %>% mutate(group_id = 1:nrow(.))
    dat_current_yes <- dat_current %>% filter(is_paired == 1) %>% mutate(group_id = NA)
    dat_current <- rbind(dat_current_yes, dat_current_no)
    dat_current <- dat_current %>% 
      arrange(stitched_sequence_id) %>% 
      mutate(group_id_lead1 = lead(group_id)) %>%
      mutate(group_id = if_else(is_paired == 1, group_id_lead1, group_id))
  }else{
    dat_current <- dat_current %>% mutate(group_id = 1:nrow(.))
  }
  
  list_dat_all_burst <- append(list_dat_all_burst, list(dat_current))
}

dat_all_burst <- bind_rows(list_dat_all_burst)

#############################################################################
# Prepare output
#############################################################################
dat_decisions_atypical_sequences <- dat_tracking_atypical_sequence
dat_decisions_atypical_sequences[["comment2"]] <- "discard paired_with_sequence_id and keep stitched_sequence_id"

dat_decisions_atypical_sequences[["sequence_cat"]] <- NA_real_

dat_decisions_atypical_sequences[["sequence_cat"]] <- if_else(grepl(pattern = "atypical sequence: short survey administered (just once), micro-randomized (just once), EMA administered (1st time), EMA administered (2nd time)", 
                                                                    x = dat_decisions_atypical_sequences[["sequence_type"]], 
                                                                    fixed = TRUE), 1, dat_decisions_atypical_sequences[["sequence_cat"]])

dat_decisions_atypical_sequences[["sequence_cat"]] <- if_else(grepl(pattern = "atypical sequence: short survey administered (just once), micro-randomized (1st time), micro-randomized (2nd time), EMA administered (just once)", 
                                                                    x = dat_decisions_atypical_sequences[["sequence_type"]], 
                                                                    fixed = TRUE), 2, dat_decisions_atypical_sequences[["sequence_cat"]])

dat_decisions_atypical_sequences[["sequence_cat"]] <- if_else(grepl(pattern = "atypical sequence: short survey administered (1st time), short survey administered (2nd time), micro-randomized (just once), EMA administered (just once)", 
                                                                    x = dat_decisions_atypical_sequences[["sequence_type"]], 
                                                                    fixed = TRUE), 3, dat_decisions_atypical_sequences[["sequence_cat"]])

dat_decisions_atypical_sequences[["sequence_cat"]] <- if_else(grepl(pattern = "atypical sequence: short survey administered (1st time), short survey administered (2nd time), no micro-randomization, no EMA administered", 
                                                                    x = dat_decisions_atypical_sequences[["sequence_type"]], 
                                                                    fixed = TRUE), 4, dat_decisions_atypical_sequences[["sequence_cat"]])

dat_decisions_atypical_sequences[["sequence_cat"]] <- if_else(grepl(pattern = "atypical sequence: short survey administered (1st time), short survey administered (2nd time), micro-randomized (just once), no EMA administered", 
                                                                    x = dat_decisions_atypical_sequences[["sequence_type"]], 
                                                                    fixed = TRUE), 5, dat_decisions_atypical_sequences[["sequence_cat"]])

dat_decisions_atypical_sequences[["sequence_cat"]] <- if_else(grepl(pattern = "atypical sequence: short survey administered (1st time), micro-randomized (1st time), short survey administered (2nd time), micro-randomized (2nd time), EMA administered (just once)", 
                                                                    x = dat_decisions_atypical_sequences[["sequence_type"]], 
                                                                    fixed = TRUE), 6, dat_decisions_atypical_sequences[["sequence_cat"]])

dat_decisions_atypical_sequences[["sequence_cat"]] <- if_else(grepl(pattern = "atypical sequence: short survey administered (1st time), micro-randomized (1st time), short survey administered (2nd time), micro-randomized (2nd time), no EMA administered", 
                                                                    x = dat_decisions_atypical_sequences[["sequence_type"]], 
                                                                    fixed = TRUE), 7, dat_decisions_atypical_sequences[["sequence_cat"]])

dat_decisions_atypical_sequences <- dat_decisions_atypical_sequences %>% arrange(sequence_cat)


#############################################################################
# Prepare output
#############################################################################
dat_output <- dat_all_burst %>% 
  select(-tracking_id_treatment_assignments, -tracking_id_ema) %>% 
  select(stitched_sequence_id, group_id, everything())

