# -----------------------------------------------------------------------------
# PART 2: The metalogic of this script is to format the output of PART 1 in 
# a way that is more amenable to subsequent data manipulation.
# -----------------------------------------------------------------------------

source("paths.R")
library(tidyverse)
library(tidyjson)

dat_long_ema_responses <- readRDS(file = file.path(path_manipulated_data, "dat_long_ema_responses.rds"))

# -- Create human-readable time variables
dat_long_ema_responses <- dat_long_ema_responses %>%
  mutate(timezone = as.numeric(timezone),
         start_time = as.numeric(start_time),
         finish_time = as.numeric(finish_time),
         end_time = as.numeric(end_time),
         recorded_time = as.numeric(recorded_time)) %>%
  mutate(olson = case_when(
    timezone/(1000*60*60) == -5 ~ "US/Eastern", 
    timezone/(1000*60*60) == -6 ~ "US/Central", 
    timezone/(1000*60*60) == -7 ~ "US/Mountain", 
    timezone/(1000*60*60) == -8 ~ "US/Pacific",
    .default = NULL
  )) %>%
  mutate(ts_start_time_utc = as_datetime(x = start_time/1000, tz = "UTC"),
         ts_finish_time_utc = as_datetime(x = finish_time/1000, tz = "UTC"),
         ts_end_time_utc = as_datetime(x = end_time/1000, tz = "UTC"),
         ts_recorded_time_utc = as_datetime(x = recorded_time/1000, tz = "UTC"))

# Create a column in mountain time for everyone 
# even for participants not actually in mountain time, 
# so that we have a quick way to eye-ball the data when we do quick checks.
dat_long_ema_responses[["ts_start_time_mountain"]] <- with_tz(time = dat_long_ema_responses[["ts_start_time_utc"]], tzone = "US/Mountain")
dat_long_ema_responses[["ts_finish_time_mountain"]] <- with_tz(time = dat_long_ema_responses[["ts_finish_time_utc"]], tzone = "US/Mountain")
dat_long_ema_responses[["ts_end_time_mountain"]] <- with_tz(time = dat_long_ema_responses[["ts_end_time_utc"]], tzone = "US/Mountain")
dat_long_ema_responses[["ts_recorded_time_mountain"]] <- with_tz(time = dat_long_ema_responses[["ts_recorded_time_utc"]], tzone = "US/Mountain")

# NA's will be of class "POSIXct" "POSIXt" which is what we want
dat_long_ema_responses[["ts_finish_time_utc"]] <- if_else(dat_long_ema_responses[["finish_time"]] == -1, NA, dat_long_ema_responses[["ts_finish_time_utc"]])
dat_long_ema_responses[["ts_finish_time_mountain"]] <- if_else(dat_long_ema_responses[["finish_time"]] == -1, NA, dat_long_ema_responses[["ts_finish_time_mountain"]])

# Clean up nonsensical timestamps in abandoned by timeout EMAs
# Recode blanks as NA's
dat_long_ema_responses <- dat_long_ema_responses %>%
  mutate(ts_finish_time_mountain = if_else(status == "ABANDONED_BY_TIMEOUT" & ts_finish_time_mountain == ymd_hms("1969-12-31 17:00:00", tz = "US/Mountain"), NA, ts_finish_time_mountain))

# After converting timestamps, simply keep timestamps in mountain time
dat_long_ema_responses <- dat_long_ema_responses %>%
  select(-ts_start_time_utc, -ts_finish_time_utc, -ts_end_time_utc, -ts_recorded_time_utc)

# -----------------------------------------------------------------------------
# Comprehensive list of questions
# -----------------------------------------------------------------------------
all_question_text <- unique(dat_long_ema_responses[["question_text"]])
all_question_identifiers <- paste("Q", 1:length(all_question_text), sep = "")
dat_master_ema_questions <- tibble(ema_item_id = all_question_identifiers, question_text = all_question_text)

# -----------------------------------------------------------------------------
# Comprehensive list of questions, paired with response options and 
# conditions
# -----------------------------------------------------------------------------
all_question_text <- dat_long_ema_responses[["question_text"]]
all_question_type <- dat_long_ema_responses[["question_type"]]
all_response_option <- dat_long_ema_responses[["response_option"]]
all_condition <- dat_long_ema_responses[["condition"]]
dat_master_ema_response_options <- tibble(all_question_text = all_question_text,
                        all_question_type = all_question_type,
                        all_response_option = all_response_option,
                        all_condition = all_condition)
dat_master_ema_response_options <- distinct(dat_master_ema_response_options)
dat_master_ema_response_options <- left_join(x = dat_master_ema_questions, y = dat_master_ema_response_options, by = join_by(question_text == all_question_text))

# -----------------------------------------------------------------------------
# Append EMA item ID's
# -----------------------------------------------------------------------------

dat_long_ema_responses <- left_join(x = dat_long_ema_responses,
                                    y = dat_master_ema_questions,
                                    by = join_by("question_text"))

# -----------------------------------------------------------------------------
# Note that exact duplicates exist in the raw data, e.g., see mars_74
# So we need a way to identify these exact duplicates programatically.
# As a first step, we isolate columns containing the responses and associated 
# timestamps. Then, we reshape into our conventional long format, which is more
# amenable to visual inspection.
# -----------------------------------------------------------------------------

tot_vars <- nrow(dat_master_ema_questions)

dat_analysis_ema_responses <- dat_long_ema_responses %>%
  filter(ema_item_id %in% paste("Q",1:tot_vars, sep=""))%>%
  select(mars_id, document_id, olson, status, ema_item_id,
         ts_start_time_mountain, ts_end_time_mountain, ts_recorded_time_mountain, 
         ts_finish_time_mountain, response)

# Using as_tibble provides more protection compared to as.data.frame
# For example,
#> dat_analysis_ema_responses$newcol <- c(1)  # -- both as_tibble and as.data.frame permit recycling of arrays of length 1
#> dat_analysis_ema_responses$newcol <- c(3,4)  # -- as_tibble will throw an error but as.data.frame will NOT throw an error 
# In this example, as.data.frame will permit recycling the array of length 2 and newcol will then appear as rep(x = c(1,2), times = some number divisible by the number of rows)
# which is not the kind of behavior we would like to have. Hence, using as_tibble facilitates automated checking against accidental misspecification of new columns.
dat_analysis_ema_responses <- as_tibble(dat_analysis_ema_responses)

# A spot check on dat_long_ema_responses reveals that there is at least 
# one participant for whom the first three questions were not delivered, 
# though, all the remaining questions of the EMA were delivered
# For that participant their status was marked as "COMPLETED" by the system
# even though the first three questions were never delivered to this participant.
dat_document_master <- dat_long_ema_responses %>%
  select(mars_id, document_id) %>%
  as_tibble(.) %>%
  unique(.)

dat_fixed_by_document <- dat_long_ema_responses %>%
  select(mars_id, document_id, olson, status, 
         ts_start_time_mountain, ts_end_time_mountain, ts_recorded_time_mountain) %>%
  as_tibble(.) %>%
  unique(.)

num_row_orig <- nrow(dat_document_master)
dat_document_master <- rep(x = list(dat_document_master), times = tot_vars)
dat_document_master <- bind_rows(dat_document_master)
dat_document_master <- dat_document_master %>% arrange(mars_id, document_id)
dat_document_master[["ema_item_id"]] <- rep(x = paste("Q", 1:tot_vars, sep=""), times = num_row_orig)
dat_document_master <- left_join(x = dat_document_master,
                                 y = dat_fixed_by_document,
                                 by = join_by("mars_id" == "mars_id",
                                              "document_id" == "document_id"))

dat_analysis_ema_responses <- full_join(x = dat_document_master, 
                                        y = dat_analysis_ema_responses,
                                        multiple = "all",
                                        by = join_by("mars_id" == "mars_id", 
                                                     "document_id" == "document_id", 
                                                     "ema_item_id" == "ema_item_id",
                                                     "olson" == "olson",
                                                     "status" == "status",
                                                     "ts_start_time_mountain" == "ts_start_time_mountain",
                                                     "ts_end_time_mountain" == "ts_end_time_mountain",
                                                     "ts_recorded_time_mountain" == "ts_recorded_time_mountain"))
# Recode blanks as NA's
dat_analysis_ema_responses <- dat_analysis_ema_responses %>%
  mutate(response = if_else(response == "", NA, response))

tmp <- dat_analysis_ema_responses %>% 
  select(mars_id, document_id, olson, status, ts_start_time_mountain, ts_end_time_mountain, ts_recorded_time_mountain,
         ema_item_id, 
         ts_finish_time_mountain, response)

tmp2 <- pivot_wider(data = tmp, 
                    names_from = ema_item_id, 
                    values_from = c("ts_finish_time_mountain", "response"),
                    names_glue = "{ema_item_id}_{.value}")

# Now, each row corresponds to one document_id
# This format is now more amenable to visual spot checking to see which
# document_id's actually are exact duplicates of each other.
dat_conventional_long_format <- tmp2
dat_conventional_long_format <- dat_conventional_long_format %>%
  select(-document_id) %>%
  mutate(is_repeated = duplicated(.))

# How many exact duplicates do we have?
sum(dat_conventional_long_format[["is_repeated"]])

# Which participants have exact duplicates?
dat_conventional_long_format %>% 
  filter(is_repeated == TRUE) %>% 
  select(mars_id) %>% 
  unique(.)

# Now, eliminate those exact duplicates!!!
dat_conventional_long_format <- dat_conventional_long_format %>%
  filter(is_repeated == FALSE) %>%
  select(-is_repeated)

dat_conventional_long_format <- dat_conventional_long_format %>%
  select(-ts_recorded_time_mountain)

# -----------------------------------------------------------------------------
# Create an ID for each participant-EMA having any response
# -----------------------------------------------------------------------------

dat_conventional_long_format <- dat_conventional_long_format %>%
  group_by(mars_id) %>%
  arrange(ts_start_time_mountain) %>%
  mutate(response_ema_id = 1:n()) %>%
  ungroup(.)

dat_conventional_long_format <- dat_conventional_long_format %>%
  select(mars_id, response_ema_id, everything()) %>%
  arrange(mars_id, ts_start_time_mountain)

# -----------------------------------------------------------------------------
# Note that there is one participant for whom the software DID NOT present the
# first three questions of the EMA, even though this participant completed
# all the questions they were asked to complete in the EMA (their EMAs were
# regarded by the software as "COMPLETED")
# -----------------------------------------------------------------------------

dat_conventional_long_format %>%
  filter(status == "COMPLETED" & is.na(Q1_response))

# -----------------------------------------------------------------------------
# Isolate variables needed for MRT primary outcome 
# -----------------------------------------------------------------------------

dat_conventional_long_format <- dat_conventional_long_format %>% select(-status)

dat_primary_outcome <- dat_conventional_long_format %>%
  select(mars_id, response_ema_id, olson,
         ts_start_time_mountain, ts_end_time_mountain,
         Q1_ts_finish_time_mountain, Q2_ts_finish_time_mountain, Q3_ts_finish_time_mountain,
         Q1_response, Q2_response, Q3_response)

# -----------------------------------------------------------------------------
# Save output
# -----------------------------------------------------------------------------

saveRDS(dat_master_ema_response_options, file = file.path(path_manipulated_data, "dat_master_ema_response_options.rds"))
saveRDS(dat_master_ema_questions, file = file.path(path_manipulated_data, "dat_master_ema_questions.rds"))
saveRDS(dat_conventional_long_format, file = file.path(path_manipulated_data, "dat_conventional_long_format_ema_responses.rds"))
saveRDS(dat_primary_outcome, file = file.path(path_manipulated_data, "dat_primary_outcome.rds"))

rm(list = ls())
