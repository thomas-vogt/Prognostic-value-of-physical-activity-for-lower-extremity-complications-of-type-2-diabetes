# NOTES:
# 1. Horizon is not relative to landmark point, but in absolute terms (age).
# 2. Need original variable (e.g., "sex") and dummy variable(s) (e.g., sex_2).
#    Original variable -> analyses restricted to levels of that variable.
#    Dummy variable(s) -> covariate(s) in the models.
#    Plus variables for imputation.
# 3. surv_time is recoded to reflect the fact that landmark age is time 0.


rm(list = ls())


source("06_functions.R")


# SCRIPT PARAMETERS -------------------------------------------------------

file_name <- commandArgs(trailingOnly = T)

reg_ex <- "(neuropathy|ulcer|major-amputation|all-amputation)"
y_name <- stringr::str_extract(file_name, pattern = reg_ex)

landmark_ages  <- as.integer(seq(19, 100, by = 1))
horizon_window <- 5L
horizon_ages   <- landmark_ages + horizon_window

all_covs <- c(
  "last_meas",
  "ischemisk_hjsjukdom", 
  "stroke", 
  "rokare", 
  "fysisk_aktivitet",
  "bmi",
  "hba1c",
  "systoliskt",
  "hdl",
  "kolesterol",
  "albuminuria",
  "retinopati",
  "year",
  "sex",
  "education",
  "ALosDag",
  "Civil",
  "Kommun",
  "DispInk04",
  "birth_country", 
  "diag_age"
)

if (grepl("ulcer", file_name)) all_covs <- c(all_covs, "neuropathy")

if (grepl("amputation", file_name)) {
  all_covs <- c(all_covs, "neuropathy", "foot_ulcer")
} 

all_covs <- c("LopNr", "age", "surv_time", "surv_status", all_covs)


# CREATE LANDMARK DATASETS ------------------------------------------------

dat <- arrow::read_parquet(file_name)

for (i in seq_along(landmark_ages)) {
  LM_df <- find_LOCF(
    dat          = dat,
    id           = "LopNr",
    LM_times     = landmark_ages[i],
    horizons     = horizon_ages[i],
    time_var     = "age",
    event_time   = "surv_time",
    event_status = "surv_status"
  )[[1]]
  
  LM_df[["age"]]       <- landmark_ages[i]
  LM_df[["last_meas"]] <- LM_df[["age"]] - LM_df[["last_meas"]]
  LM_df[["surv_time"]] <- LM_df[["surv_time"]] - LM_df[["age"]]
  
  LM_df <- LM_df[, all_covs]
  
  saveRDS(
    LM_df, 
    paste0(
      "../Processed_data/06_landmark-datasets/landmark-", 
      y_name,
      "-",
      landmark_ages[i], 
      ".rds"
    )
  )
}


rm(list = ls())
