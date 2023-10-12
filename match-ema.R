source("paths.R")
library(tidyverse)

source("check-linkage-algorithm/node-C.R")
source("check-linkage-algorithm/node-E.R")

saveRDS(dat_output, file = file.path(path_manipulated_data, "dat_all_with_matched_status_ema.rds"))
