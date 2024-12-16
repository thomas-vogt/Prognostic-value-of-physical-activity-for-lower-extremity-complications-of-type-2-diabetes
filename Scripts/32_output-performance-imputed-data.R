rm(list = ls())


library(tidyverse)

options(scipen = 999)

source("32_functions.R")

files <- list.files("../Processed_data/22_performance-imputed", full.names = T)
nm_pattern <- "(neuropathy|ulcer|major-amputation|all-amputation)[[:digit:]]+"
names(files) <- str_extract(files, pattern = nm_pattern)

results <- names(files) %>% 
  set_names() %>% 
  lapply(\(x) readRDS(files[x])) %>% 
  lapply(\(x) computePerformance(x)) %>% 
  lapply(\(x) bind_rows(x, .id = "term")) %>% 
  bind_rows(.id = "lm_dataset")

results$conf.low  <- results$`2.5%`
results$conf.high <- results$`97.5%`
results$`2.5%`  <- NULL
results$`97.5%` <- NULL

results$outcome      <- gsub("[[:digit:]]*", "", results$lm_dataset)
results$landmark_age <- gsub("[[:alpha:]]*", "", results$lm_dataset)
results$landmark_age <- gsub("-?", "", results$landmark_age)
results$subgroup <- as.character(NA)
results$lm_dataset <- NULL

results <- results[, c(5:7, 1:4)]

warnings()

saveRDS(results, "../Output/Results/Table3_performance-imputed-data.rds")


rm(list = ls())
