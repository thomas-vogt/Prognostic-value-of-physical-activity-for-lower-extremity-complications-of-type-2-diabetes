rm(list = ls())


library(stringr)
library(tidyverse)
library(cmprsk)
library(mice)

source("31_functions.R")


mira_files <- list.files(
  "../Processed_data/21_modelled-imputed-data/Mira_objects", 
  full.names = T
)
mira_files <- mira_files[grepl("rds", mira_files)]
names(mira_files) <- str_extract(mira_files, "[[:alpha:]]*-?[[:alpha:]]+[[:digit:]]{2,3}\\.")


# POOL RESULTS ------------------------------------------------------------

pooled_res <- vector("list", length = length(mira_files))

for (i in seq_along(mira_files)) {
  mira_obj <- readRDS(mira_files[i])
  names(mira_obj) <- paste0(
    names(mira_files)[i],
    gsub("(neuropathy|ulcer|major-amputation|all-amputation)", "", names(mira_obj))
  )
  pooled_res[[i]] <- lapply(mira_obj, function(x) pool(x))
  names(pooled_res[[i]]) <- gsub("\\.+", "\\.", names(pooled_res[[i]]))
}

pooled_res <- unlist(pooled_res, recursive = F)

all(sapply(pooled_res, is.mipo))


# GATHER ALL RESULTS IN A DATA FRAME --------------------------------------

out <- vector("list", length = length(pooled_res))
for (i in seq_along(pooled_res)) {
  out[[i]] <- df_results_mice(pooled_res[[i]], mipo_name = names(pooled_res)[i])
}

all_results <- bind_rows(out) %>% 
  separate(mipo_name, into = c("event", "subgroup", "model"), sep = "\\.", remove = F) %>% 
  select(outcome, landmark_age, event, subgroup, model, everything()) %>% 
  mutate(
    model    = if_else(grepl("model", subgroup), subgroup, model),
    subgroup = if_else(grepl("model", subgroup), as.character(NA), subgroup),
    event    = gsub("[[:digit:]]*", "", event)
  )

warnings()

saveRDS(all_results, "../Output/Results/Table2_all_results_mice.rds")


rm(list = ls())