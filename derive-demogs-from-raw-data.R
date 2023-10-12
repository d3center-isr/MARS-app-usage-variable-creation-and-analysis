rm(list = ls())

source("paths.R")
library(tidyverse)

dat_visit_dates <- readRDS(file = file.path(path_manipulated_data, "dat_visit_dates.rds"))
redcap_crosswalk <- readRDS(file = file.path(path_visit_data, "redcap_crosswalk.rds"))
redcap_metadata <- readRDS(file = file.path(path_demog_data, "redcap_metadata.rds"))
redcap_data <- readRDS(file = file.path(path_demog_data, "redcap_data.rds"))

redcap_crosswalk <- redcap_crosswalk %>% rename(rsr_id = SubjectID, redcap_id = REDCap_ID)
grafana_crosswalk <- dat_visit_dates %>% select(rsr_id, mars_id)

# ------------------------------------------------------------------------------
# Create skeleton
# ------------------------------------------------------------------------------

dat_demogs <- full_join(x = grafana_crosswalk, y = redcap_crosswalk, by = join_by(rsr_id == rsr_id))

# ------------------------------------------------------------------------------
# Variable: age
# ------------------------------------------------------------------------------

dat_age <- redcap_data %>% select(REDCap_ID, scr_age) %>% rename(redcap_id = REDCap_ID, age = scr_age)
dat_demogs <- left_join(x = dat_demogs, y = dat_age, by = join_by(redcap_id == redcap_id))

# ------------------------------------------------------------------------------
# Variable: SES
# ------------------------------------------------------------------------------

# Create categories so that incomes are bucketed in increments of USD 10,000
dat_ses <- redcap_data %>% 
  select(REDCap_ID, dses14_1) %>% 
  rename(redcap_id = REDCap_ID) %>%
  mutate(income_category = case_when(
    (dses14_1 == 1) | (dses14_1 == 2) ~ "less than or equal to USD 9,999",
    (dses14_1 == 3) | (dses14_1 == 4) ~ "greater than USD 9,999 and less than or equal to USD 19,999",
    (dses14_1 == 5) | (dses14_1 == 6) ~ "greater than USD 19,999 and less than or equal to USD 29,999",
    dses14_1 == 7 ~ "greater than USD 29,999 and less than or equal to USD 39,999",
    dses14_1 == 8 ~ "greater than USD 39,999 and less than or equal to USD 49,999",
    dses14_1 == 9 ~ "greater than USD 49,999 and less than or equal to USD 59,999",
    dses14_1 == 10 ~ "greater than USD 59,999 and less than or equal to USD 69,999",
    dses14_1 == 11 ~ "greater than USD 69,999 and less than or equal to USD 79,999",
    dses14_1 == 12 ~ "greater than USD 79,999 and less than or equal to USD 89,999",
    dses14_1 == 13 ~ "greater than USD 89,999 and less than or equal to USD 99,999",
    dses14_1 == 14 ~ "greater than or equal to USD 100,000",
    dses14_1 == 15 ~ NA_character_,
    .default = NULL
  )) %>%
  mutate(income_val = case_when(
    income_category == "less than or equal to USD 9,999" ~ 1,
    income_category == "greater than USD 9,999 and less than or equal to USD 19,999" ~ 2,
    income_category == "greater than USD 19,999 and less than or equal to USD 29,999" ~ 3,
    income_category == "greater than USD 29,999 and less than or equal to USD 39,999" ~ 4,
    income_category == "greater than USD 39,999 and less than or equal to USD 49,999" ~ 5,
    income_category == "greater than USD 49,999 and less than or equal to USD 59,999" ~ 6,
    income_category == "greater than USD 59,999 and less than or equal to USD 69,999" ~ 7,
    income_category == "greater than USD 69,999 and less than or equal to USD 79,999" ~ 8,
    income_category == "greater than USD 79,999 and less than or equal to USD 89,999" ~ 9,
    income_category == "greater than USD 89,999 and less than or equal to USD 99,999" ~ 10,
    income_category == "greater than or equal to USD 100,000" ~ 11,
    .default = NULL
  )) %>%
  select(redcap_id, 
         income_category, # this is the verbal description of the category
         income_val)      # this is the numerical coding of the category which can be used if we were to treat this variable as continuous

dat_demogs <- left_join(x = dat_demogs, y = dat_ses, by = join_by(redcap_id == redcap_id))

# ------------------------------------------------------------------------------
# Variable: partner status
# ------------------------------------------------------------------------------

dat_partner_status <- redcap_data %>% 
  select(REDCap_ID, dses3_1) %>% 
  rename(redcap_id = REDCap_ID) %>%
  mutate(partner_status_category = case_when(
    dses3_1 == 1 ~ "single",
    dses3_1 == 2 ~ "married",
    dses3_1 == 3 ~ "divorced",
    dses3_1 == 4 ~ "widowed",
    dses3_1 == 5 ~ "living with significant other",
    dses3_1 == 6 ~ "separated",
    .default = NULL
  )) %>%
  select(redcap_id, partner_status_category)  

dat_demogs <- left_join(x = dat_demogs, y = dat_partner_status, by = join_by(redcap_id == redcap_id))

# ------------------------------------------------------------------------------
# Variable: tobacco history
# ------------------------------------------------------------------------------

dat_tobacco_history <- redcap_data %>% 
  select(REDCap_ID, th2_1) %>% 
  rename(redcap_id = REDCap_ID,
         baseline_tobacco_history = th2_1) %>%
  select(redcap_id, 
         baseline_tobacco_history)

dat_demogs <- left_join(x = dat_demogs, y = dat_tobacco_history, by = join_by(redcap_id == redcap_id))

# ------------------------------------------------------------------------------
# Variable: gender
# ------------------------------------------------------------------------------

dat_gender <- redcap_data %>%
  select(REDCap_ID, dses1_1, screener_gender = gender) %>%
  mutate(gender_category = case_when(
    dses1_1 == 1 ~ "male",
    dses1_1 == 2 ~ "female",
    dses1_1 == 8 ~ NA_character_,
    (dses1_1 != 1) & (dses1_1 != 2) & (dses1_1 != 8) ~ "other",
    .default = NULL
  )) %>%
  mutate(screener_gender = case_when(
    screener_gender == "M" ~ "male",
    screener_gender == "F" ~ "female",
    .default = NULL
  )) %>%
  mutate(is_missing_v1_gender = if_else(is.na(gender_category), 1, 0)) %>%
  # If no reported gender at V1, then use reported gender at screener
  mutate(gender_category = if_else(is_missing_v1_gender == 1, screener_gender, gender_category)) %>%
  rename(redcap_id = REDCap_ID) %>%
  select(redcap_id, gender_category, is_missing_v1_gender, screener_gender) %>%
  select(-is_missing_v1_gender, -screener_gender)

dat_demogs <- left_join(x = dat_demogs, y = dat_gender, by = join_by(redcap_id == redcap_id))

# ------------------------------------------------------------------------------
# Variable: race/ethnicity
# ------------------------------------------------------------------------------

dat_race_ethnicity <- redcap_data %>%
  select(REDCap_ID, 
         # The variable in the next line contains responses to the item
         #   "Are you Hispanic or Latino?"
         dses4_1,  
         # The variables in the next line contain responses to the item
         #   "You have stated that you are Hispanic/Latino. 
         #    Being Hispanic/Latino is considered an ethnicity, not race. 
         #    We want to find out what race with which you identify. 
         #    Please look at the categories below and mark the one(s) 
         #    that best describe your race. (Check all that apply)"
         #
         #    Check all that apply:
         #    1 -- White
         #    2 -- Black or African American
         #    3 -- Asian
         #    4 -- Native Hawaiian or Other Pacific Islander
         #    5 -- American Indian/Alaska Native
         dses5_1___1, dses5_1___2, dses5_1___3, dses5_1___4, dses5_1___5, dses5_1___nask,
         # The variables in the next line contain responses to the item
         #   "Please look at the categories below 
         #    and mark the one(s) that best describes your race."
         #
         #    Check all that apply:
         #    1 -- White
         #    2 -- Black or African American
         #    3 -- Asian
         #    4 -- Native Hawaiian or Other Pacific Islander
         #    5 -- American Indian/Alaska Native
         dses5a_1___1, dses5a_1___2, dses5a_1___3, dses5a_1___4, dses5a_1___5, dses5a_1___nask,
         # These variables were assessed at screener
         ethnicity,
         race___1, race___2, race___3, race___7, race___96, race___99) %>%
  rename(redcap_id = REDCap_ID) %>%
  mutate(is_latino_category = if_else(dses4_1 == 1, "latino", "not latino"))

dat_race_ethnicity <- dat_race_ethnicity %>%
  mutate(num_checked_race = if_else(is_latino_category == "latino", 
                                    dses5_1___1 + dses5_1___2 + dses5_1___3 + dses5_1___4 + dses5_1___5, 
                                    dses5a_1___1 + dses5a_1___2 + dses5a_1___3 + dses5a_1___4 + dses5a_1___5))

# Consolidate responses from individuals who reported to be latino and 
# individuals who reported to not be latino into one variable for each possible category
# Here, we consolidate into dummy coded variables
dat_race_ethnicity <- dat_race_ethnicity %>%
  mutate(race_is_white = if_else(is_latino_category == "latino", dses5_1___1, dses5a_1___1),
         race_is_black_or_african_american = if_else(is_latino_category == "latino", dses5_1___2, dses5a_1___2),
         race_is_asian = if_else(is_latino_category == "latino", dses5_1___3, dses5a_1___3),
         race_is_native_hawaiian_or_other_pacific_islander = if_else(is_latino_category == "latino", dses5_1___4, dses5a_1___4),
         race_is_american_indian_or_alaska_native = if_else(is_latino_category == "latino", dses5_1___5, dses5a_1___5))
# Here we consolidate into one categorical variable
dat_race_ethnicity <- dat_race_ethnicity %>%
  mutate(race_category = case_when(
    num_checked_race == 1 & race_is_white == 1 ~ "white",
    num_checked_race == 1 & race_is_black_or_african_american == 1 ~ "black or african american",
    num_checked_race == 1 & race_is_american_indian_or_alaska_native == 1 ~ "american indian or alaska native",
    num_checked_race == 1 & race_is_native_hawaiian_or_other_pacific_islander == 1 ~ "native hawaiian or other pacific islander",
    num_checked_race == 1 & race_is_asian == 1 ~ "asian",
    num_checked_race >= 2 ~ "multiracial",
    .default = NULL
  ))

# If race/ethnicity was not reported in V1 look back to screener
dat_race_ethnicity <- dat_race_ethnicity %>%
  mutate(is_missing_v1_ethnicity = if_else(is.na(is_latino_category), 1, 0),
         is_missing_v1_race = if_else(is.na(race_category), 1, 0))

dat_race_ethnicity <- dat_race_ethnicity %>%
  mutate(screener_ethnicity = case_when(
    ethnicity == 70 ~ "latino",
    ethnicity == 970 ~ "not latino",
    ethnicity == 99 ~ NA_character_,
    .default = NULL
  )) %>%
  mutate(screener_race_is_white = if_else(race___1 == 1, 1, 0),
         screener_race_is_black_or_african_american = if_else(race___2 == 1, 1, 0),
         screener_race_is_american_indian_or_alaska_native = if_else(race___3 == 1, 1, 0),
         screener_race_is_native_hawaiian_or_other_pacific_islander = if_else(race___7 == 1, 1, 0),
         screener_race_is_asian = if_else(race___96 == 1, 1, 0)) %>%
  mutate(screener_race_is_white = replace(screener_race_is_white, race___99 == 1, NA),
         screener_race_is_black_or_african_american = replace(screener_race_is_black_or_african_american, race___99 == 1, NA),
         screener_race_is_american_indian_or_alaska_native = replace(screener_race_is_american_indian_or_alaska_native, race___99 == 1, NA),
         screener_race_is_native_hawaiian_or_other_pacific_islander = replace(screener_race_is_native_hawaiian_or_other_pacific_islander, race___99 == 1, NA),
         screener_race_is_asian = replace(screener_race_is_asian, race___99 == 1, NA)) %>%
  mutate(screener_num_checked_race = screener_race_is_white + screener_race_is_black_or_african_american + screener_race_is_american_indian_or_alaska_native + screener_race_is_native_hawaiian_or_other_pacific_islander + screener_race_is_asian) %>%
  mutate(screener_race_category = case_when(
    screener_num_checked_race == 1 & screener_race_is_white == 1 ~ "white",
    screener_num_checked_race == 1 & screener_race_is_black_or_african_american == 1 ~ "black or african american",
    screener_num_checked_race == 1 & screener_race_is_american_indian_or_alaska_native == 1 ~ "american indian or alaska native",
    screener_num_checked_race == 1 & screener_race_is_native_hawaiian_or_other_pacific_islander == 1 ~ "native hawaiian or other pacific islander",
    screener_num_checked_race == 1 & screener_race_is_asian == 1 ~ "asian",
    screener_num_checked_race >= 2 ~ "multiracial",
    .default = NULL
  ))

dat_race_ethnicity <- dat_race_ethnicity %>%
  mutate(is_latino_category = if_else(is_missing_v1_ethnicity == 1, screener_ethnicity, is_latino_category),
         race_category = if_else(is_missing_v1_race == 1, screener_race_category, race_category),
         num_checked_race = if_else(is_missing_v1_race == 1, screener_num_checked_race, num_checked_race))

dat_race_ethnicity <- dat_race_ethnicity %>%
  mutate(race_is_white = if_else(is_missing_v1_race == 1, screener_race_is_white, race_is_white),
         race_is_black_or_african_american = if_else(is_missing_v1_race == 1, screener_race_is_black_or_african_american, race_is_black_or_african_american),
         race_is_asian = if_else(is_missing_v1_race == 1, screener_race_is_asian, race_is_asian),
         race_is_native_hawaiian_or_other_pacific_islander = if_else(is_missing_v1_race == 1, screener_race_is_native_hawaiian_or_other_pacific_islander, race_is_native_hawaiian_or_other_pacific_islander),
         race_is_american_indian_or_alaska_native = if_else(is_missing_v1_race == 1, screener_race_is_american_indian_or_alaska_native, race_is_american_indian_or_alaska_native)) 

# Collapse categories into buckets recommended by collaborators
dat_race_ethnicity <- dat_race_ethnicity %>% 
  mutate(race_and_ethnicity = case_when(
    is_latino_category == "latino" ~ "latino",
    is_latino_category == "not latino" & (race_is_black_or_african_american == 1 & num_checked_race == 1) ~ "not latino and black",
    is_latino_category == "not latino" & (race_is_white == 1 & num_checked_race == 1) ~ "not latino and white",
    is_latino_category == "not latino" & (num_checked_race > 1 | race_is_asian == 1 | race_is_native_hawaiian_or_other_pacific_islander == 1 | race_is_american_indian_or_alaska_native == 1) ~ "other",
    .default = NULL))

# ------------------------------------------------------------------------------
# Grab the columns needed
# ------------------------------------------------------------------------------
dat_race_ethnicity <- dat_race_ethnicity %>%
  select(redcap_id, 
         screener_ethnicity, screener_race_category, is_missing_v1_ethnicity, is_missing_v1_race,
         is_latino_category, 
         race_and_ethnicity,
         num_checked_race,
         race_is_white, race_is_black_or_african_american, race_is_asian,
         race_is_native_hawaiian_or_other_pacific_islander, race_is_american_indian_or_alaska_native) %>%
  select(-screener_ethnicity, -screener_race_category, -is_missing_v1_ethnicity, -is_missing_v1_race)

dat_demogs <- left_join(x = dat_demogs, y = dat_race_ethnicity, by = join_by(redcap_id == redcap_id))

# ------------------------------------------------------------------------------
# Save dataset
# ------------------------------------------------------------------------------
saveRDS(dat_demogs, file = file.path(path_manipulated_data, "dat_demogs.rds"))

