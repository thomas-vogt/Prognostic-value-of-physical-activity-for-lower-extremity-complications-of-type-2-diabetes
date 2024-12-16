# NOTES:
# 1. Model 1: y ~ phys_activity
#    Model 2: y ~ all_covariates
#    Model 3: y ~ phys_activity + all_covariates
# 2. Covariates need to be coded as dummy variables.
# 3. "modifiers" is all the categories of the study population that should
#    have their own models (e.g., "male", "low educ").
# 4. This shouldn't run on all datasets, i.e., not on those where no event is 
#    observed (or a very small number of events).


rm(list = ls())


library(tidyverse)
library(cmprsk)


source("21_functions.R")


# SCRIPT PARAMETERS -------------------------------------------------------

file_name <- commandArgs(trailingOnly = T)

reg_ex <- "(neuropathy|ulcer|major-amputation|all-amputation)"

y_name       <- stringr::str_extract(file_name, pattern = reg_ex)
landmark_age <- stringr::str_extract(file_name, pattern = "-[[:digit:]]+")

if_test <- paste0(y_name, gsub("-", "", landmark_age))

imputed <- list.files("../Processed_data/11_imputed-datasets/", full.names = F)

if (!any(grepl(if_test, imputed))) stop("no corresponding imputed dataset")


# RECODE VARIABLES --------------------------------------------------------

dat <- readRDS(file_name)
dat <- to_dummy(dat)


# COVARIATES AND MODIFIERS ------------------------------------------------

load_covariates()

if (grepl("(ulcer|amputation)", file_name)) {
  covariates <- c(covariates, "neuropathy")
}

phys_activity <- names(dat)[grep("fysisk_aktivitet_", names(dat))]

covariates    <- rm_if_unique(dat, covariates)
phys_activity <- rm_if_unique(dat, phys_activity)

sex_levels  <- levels(dat$sex)
educ_levels <- levels(dat$education)
modifiers   <- c(sex_levels, educ_levels)

dat <- dat[, c("LopNr", phys_activity, covariates, "surv_time", "surv_status", "sex", "education")]
dat <- dat[complete.cases(dat), ]


# MODELLING ---------------------------------------------------------------

out <- vector("list", length = 1L)

if (y_name == "neuropathy")            names(out) <- "neuropathy"
if (y_name == "ulcer")                 names(out) <- "ulcer"
if (grepl("major-amputation", y_name)) names(out) <- "major-amputation"
if (grepl("all-amputation", y_name))   names(out) <- "all-amputation"

out[[1]] <- modelling(dat = dat, failure_code = 1, covs = covariates)

out2 <- vector("list", length = length(modifiers))
names(out2) <- modifiers
for (nm in modifiers) {
  if (nm %in% sex_levels) {
    dat2        <- dat[dat$sex == nm, ]
    covariates2 <- covariates[!covariates == "sex_2"]
  }   
  if (nm %in% educ_levels) {
    dat2        <- dat[dat$education == nm, ]
    covariates2 <- covariates
  }
  # dat2 <- as.data.frame(dat2)
  out2[[nm]] <- modelling(dat = dat2, failure_code = 1, covs = covariates2)
}

out2 <- unlist(out2, recursive = F)
out[[1]] <- c(out[[1]], out2)

out <- unlist(out, recursive = F)

saveRDS(
  out,
  paste0(
    "../Processed_data/21_modelled-complete-case/",
    y_name,
    landmark_age,
    ".rds"
  )
)

warnings()


rm(list = ls())