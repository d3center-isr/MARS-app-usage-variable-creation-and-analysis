source("paths.R")
library(tidyverse)

# Read in parsed Grafana ID's and crosswalk between RSR ID's and Grafana ID's
list_ids <- readRDS(file = file.path(path_manipulated_data, "list_ids.rds"))

# -----------------------------------------------------------------------------
# Define functions useful for working with data. 
# These functions were created after noticing chunks of code repeated at 
# various parts of the pipeline.
# -----------------------------------------------------------------------------

unzip_files <- function(search_these_ids, search_this_name, use_path, use_sep = ","){
  # About:
  #   Read bz2 files located within different directories.
  #   Each participant has one dedicated directory for all their MD2K data.
  # 
  # Args:
  #   search_these_ids -- this is a list
  #   search_this_name -- this is a string
  #
  # Output:
  #   list_log -- this is a list
  
  list_log <- list()
  
  for(i in 1:length(search_these_ids)){
    current_id <- search_these_ids[[i]]
    
    is_here <- file.exists(file.path(use_path, current_id, search_this_name))
    
    if(isTRUE(is_here)){
      dat_log <- read.csv(file.path(use_path, current_id, search_this_name), header = FALSE, sep = use_sep)
    }
    
    # Append ID since the raw data files do not come with 
    # the ID's in and of themselves
    dat_log[["mars_id"]] <- current_id
    list_log <- append(list_log, list(dat_log))
  }
  
  return(list_log)
}

# -----------------------------------------------------------------------------
# Unzip files!
# -----------------------------------------------------------------------------

# -- System log file: A treasure trove of information about where to situate 
#                     events within the overall timeline of the study.
#                     This file also contains the randomized assignments.
list_all_system_log <- unzip_files(search_these_ids = list_ids, 
                                   search_this_name = "SYSTEM_LOG--org.md2k.scheduler.csv.bz2", 
                                   use_path = path_raw_data)

# -- Low effort log file: This is probably the data stream with the most 
#                         comprehensive information about responses to the 
#                         2-question survey.
#
#                         Contains two types of responses: 
#                         (a) responses from 2-question surveys that the 
#                             software delivers as part of the pre-defined 
#                             6-block schedule.
#                         (b) responses from 2-question surveys that pop-up
#                             whenever the participant tries to access the 
#                             low-effort intervention messages in the app
#                             during moments of time that are unprompted
#                             by the app.
#
#                         Note that for primary aim analysis for the MRT,
#                         we only need (a) above and not (b), but (b)
#                         could potentially be used in other types of analysis.

list_all_loweffort_log <- unzip_files(search_these_ids = list_ids, 
                                      search_this_name = "INTERVENTION_LOG--org.md2k.loweffortintervention.csv.bz2", 
                                      use_path = path_raw_data,
                                      use_sep = ";")  # -- This is unique to 2qs. We will leverage the data structure that results from using this separator.

# -----------------------------------------------------------------------------
# Save files
# -----------------------------------------------------------------------------

saveRDS(list_all_system_log, file = file.path(path_manipulated_data, "list_all_system_log.rds"))
saveRDS(list_all_loweffort_log, file = file.path(path_manipulated_data, "list_all_loweffort_log.rds"))
