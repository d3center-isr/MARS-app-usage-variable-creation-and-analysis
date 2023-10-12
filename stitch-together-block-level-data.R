source("paths.R") 
library(tidyverse)

source("check-linkage-algorithm/node-F.R")

saveRDS(dat_output, file = file.path(path_manipulated_data, "dat_all_burst.rds"))
saveRDS(dat_decisions_atypical_sequences, file = file.path(path_manipulated_data, "dat_decisions_atypical_sequences.rds"))
