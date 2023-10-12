rm(list = ls())

source("paths.R")
library(tidyverse)

list_ids <- readRDS(file = file.path(path_manipulated_data, "list_ids.rds"))
arr_ids <- unlist(list_ids)

###############################################################################
# Create crosswalk between Grafana ID's and RSR ID's
###############################################################################
v1_data_checklist <- readRDS(file = file.path(path_visit_data, "Visit1_DataChecklist.rds"))

# Note that RSR uses a different set of ID's (Subject ID) than 
# the ID's used by MD2K (Grafana ID).
# Hence, we need to have a crosswalk between both data collection systems
# in order to link data across systems.
# The v1 data checklist contains two columns which supply that link
# This crosswalk provides us with a "masterlist" of all enrolled participants

crosswalk_grafana_rsr <- v1_data_checklist %>%
  select(SubjectID, Grafana_ID, Remote_Enrolled) %>%
  # MD2K data was collected under mars_122 and not under mars_114
  mutate(Grafana_ID = if_else(Grafana_ID == "mars_114/mars_122", "mars_122", Grafana_ID)) %>%
  # Rename SubjectID to rsr_id
  rename(rsr_id = SubjectID) %>%
  # Rename Grafana_ID to mars_id
  rename(mars_id = Grafana_ID) %>%
  # The Grafana ID "mars_63" was "trashed" by study staff and replaced by "mars_70"
  # But Visit1_DataChecklist recorded data under the trashed ID mars_63 (not under the new ID mars_70)
  # Hence, we need to reflect the change manually here
  mutate(mars_id = replace(mars_id, mars_id == "mars_63", "mars_70"))

crosswalk_grafana_rsr[["md2k_directory_exist"]] <- crosswalk_grafana_rsr[["mars_id"]] %in% arr_ids
crosswalk_grafana_rsr[["duplicated_mars_id_in_crosswalk"]] <-  duplicated((crosswalk_grafana_rsr[["mars_id"]]))

###############################################################################
# Sanity check
###############################################################################

if(FALSE){  # Change from FALSE to TRUE to execute code
  # How many Grafana ID's were matched to more than one RSR ID in the crosswalk?
  # Answer: None
  crosswalk_grafana_rsr %>% 
    group_by(duplicated_mars_id_in_crosswalk) %>%
    summarise(`Number of Grafana ID's` = n())
  
  # The ID's in RSR represent the enrolled participants,
  # Of the ID's in RSR, how many have a directory in the MD2K data?
  # Answer: all of them
  crosswalk_grafana_rsr %>% 
    filter(duplicated_mars_id_in_crosswalk == FALSE) %>%
    group_by(md2k_directory_exist) %>%
    summarise(`Number of Participants` = n())
}

###############################################################################
# Save output
###############################################################################
saveRDS(crosswalk_grafana_rsr, file.path(path_manipulated_data, "crosswalk_grafana_rsr.rds"))

