rm(list = ls())


library(ggplot2)
library(cmprsk)


source("31_functions.R")


file_names <- list.files(
  "../Processed_data/21_modelled-complete-case", 
  full.names = T
)

file_names <- file_names[grepl("rds", file_names)]


# GATHER ALL RESULTS IN A DATA FRAME --------------------------------------

all_results <- vector("list", length = length(file_names))

for (i in seq_along(file_names)) all_results[[i]] <- crr_results(file_names[i])

warnings()
all_results <- dplyr::bind_rows(all_results)

saveRDS(all_results, "../Output/Results/Table2_all_results_cc_cases.rds")


rm(list = ls())