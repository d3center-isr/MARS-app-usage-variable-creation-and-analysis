source("paths.R") 
library(tidyverse)

source("check-linkage-algorithm/node-G.R")

saveRDS(dat_output, file = file.path(path_manipulated_data, "dat_cleaned_burst.rds"))
