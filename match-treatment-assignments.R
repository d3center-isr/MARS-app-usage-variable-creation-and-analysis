source("paths.R")
library(tidyverse)

source("check-linkage-algorithm/node-B.R")

saveRDS(dat_for_output, file = file.path(path_manipulated_data, "dat_all_with_matched_treatment_assignments.rds"))
