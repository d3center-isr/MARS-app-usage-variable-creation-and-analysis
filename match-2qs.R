source("paths.R")
library(tidyverse)

source("check-linkage-algorithm/node-A.R")
source("check-linkage-algorithm/node-D.R")

saveRDS(dat_decisions_2qs, file = file.path(path_manipulated_data, "dat_decisions_2qs.rds"))
saveRDS(dat_output, file = file.path(path_manipulated_data, "dat_all_with_matched_status_2qs.rds"))
