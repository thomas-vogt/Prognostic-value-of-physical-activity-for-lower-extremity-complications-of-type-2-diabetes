# NOTES:
# 1. Model 1: y ~ phys_activity
#    Model 2: y ~ all_covariates
#    Model 3: y ~ phys_activity + all_covariates
# 2. All variables need to be int or dbl. Covariates need to be coded as dummy
#    variables. "phys_activity" (for example) should be replaced with 
#    several corresponding dummy variables in the "cov1" part of crr().
# 3. "modifiers" is all the categories of the study population that should
#    have their own models (e.g., "male", "low educ")


rm(list = ls())


library(tidyverse)
library(cmprsk)
library(mice)


source("21_functions.R")


# SCRIPT PARAMETERS -------------------------------------------------------

command_line_args <- commandArgs(trailingOnly = T)
file_name <- command_line_args[1]
bootstrapCV_seed <- as.integer(command_line_args[2])

reg_ex <- "(neuropathy|ulcer|major-amputation|all-amputation)"

y_name       <- stringr::str_extract(file_name, pattern = reg_ex)
landmark_age <- gsub(
  "\\.", "", stringr::str_extract(file_name, pattern = "[[:digit:]]+\\.")
)

if (file.exists(paste0("../Processed_data/22_performance-imputed/", y_name, landmark_age, ".rds"))) {
  stop("cross-validation already done")
}

# RECODE VARIABLES --------------------------------------------------------

imp1 <- readRDS(file_name)

imp2 <- complete(imp1, "long", include = T) %>% 
  select(-log_surv_time) %>% 
  mutate(
    surv_status         = as.integer(as.character(surv_status)),
    sex                 = as.integer(as.character(sex)),
    education           = as.integer(as.character(education)),
    rokare              = as.integer(as.character(rokare)),
    stroke              = as.integer(as.character(stroke)),
    ischemisk_hjsjukdom = as.integer(as.character(ischemisk_hjsjukdom)),
    fysisk_aktivitet    = as.integer(as.character(fysisk_aktivitet)),
    ihd_stroke          = as.integer(as.character(ihd_stroke))
  ) %>% 
  to_dummy(recode_ihd_stroke = F)
 

# COVARIATES AND MODIFIERS ------------------------------------------------

load_covariates()

if (grepl("(ulcer|amputation)", file_name)) {
  covariates <- c(covariates, "neuropathy")
}

phys_activity <- names(imp2)[grep("fysisk_aktivitet_", names(imp2))]

covariates    <- rm_if_unique(imp2, covariates)
phys_activity <- rm_if_unique(imp2, phys_activity)

sex_levels  <- levels(imp2$sex)
educ_levels <- levels(imp2$education)
modifiers   <- c(sex_levels, educ_levels)


# MODELLING ---------------------------------------------------------------

out <- crr_imputed(imp1, imp2, fail_code = 1L, adjustment_covs = covariates)

out2 <- vector("list", length = length(modifiers))
names(out2) <- modifiers
for (nm in modifiers) {
  if (nm %in% sex_levels) {
    imp3        <- imp2[imp2$sex %in% nm, ]
    covariates2 <- covariates[!covariates == "sex_2"]
  }   
  if (nm %in% educ_levels) {
    imp3        <- imp2[imp2$education %in% nm, ]
    covariates2 <- covariates
  }  
  out2[[nm]] <- crr_imputed(imp1, imp3, fail_code = 1L, adjustment_covs = covariates2)
}

out2 <- unlist(out2, recursive = F)
out <- c(out, out2)

names(out) <- paste0(y_name, ".", names(out))

saveRDS(
  out,
  paste0(
    "../Processed_data/21_modelled-imputed-data/Mira_objects/",
    y_name,
    landmark_age,
    ".rds"
  )
)

source("22_model-performance-imputed-data.R")

warnings()


rm(list = ls())
