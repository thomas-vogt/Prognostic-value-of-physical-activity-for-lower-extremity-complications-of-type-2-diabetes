# NOTES:
# 1. For secondary analysis for all amputations (here we restrict to major 
#    amputations)
# 2. No imputation if no event.


rm(list = ls())


library(stringr)
library(magrittr)
library(dplyr)
library(mice)
library(cmprsk)

source("11_functions.R")


file_name <- commandArgs(trailingOnly = T)

reg_ex <- "major-amputation"
lm_df_name <- gsub(
  "-", "", str_extract(file_name, "amputation-[[:digit:]]{2,3}")
)
imputed <- list.files("../Processed_data/11_imputed-datasets/", full.names = F)

if (!grepl(reg_ex, file_name)) {
  stop("landmark df doesn't have major amputation as outcome and was skipped")
} 

dat <- readRDS(file_name)


# RECODE VARIABLES FOR IMPUTATION -----------------------------------------

dat <- dat %>% 
  mutate(
    dat,
    log_surv_time       = log(surv_time),
    surv_status         = factor(surv_status),
    birth_country       = if_else(birth_country == "00", "Sweden", "Other"),
    birth_country       = factor(birth_country),
    Civil               = if_else(Civil == "G" | Civil == "RP", "cohabit", "no"),
    Civil               = factor(Civil),
    Region              = factor(str_extract(Kommun, "^[[:digit:]]{2}")),
    sex                 = factor(sex),
    rokare              = factor(rokare),
    ihd_stroke          = if_else(
      ischemisk_hjsjukdom == 1L | stroke == 1L, 
      1L, 
      as.integer(NA)
    ),
    ihd_stroke          = if_else(
      is.na(ihd_stroke) & ischemisk_hjsjukdom == 0L & stroke == 0L, 
      0L, 
      ihd_stroke
    ),
    ihd_stroke          = factor(ihd_stroke),
    education           = factor(education, levels = c("1", "2"), ordered = T),
    fysisk_aktivitet    = factor(
      fysisk_aktivitet, 
      levels = c("1", "2", "3", "4", "5"), 
      ordered = T
    )
  ) %>% 
  select(-Kommun)

if ("neuropathy" %in% names(dat)) dat$neuropathy <- factor(dat$neuropathy)
if ("foot_ulcer" %in% names(dat)) dat$foot_ulcer <- factor(dat$foot_ulcer)


# MISSINGNESS TABLE -------------------------------------------------------

table1a_NA <- cbind(
  "number NA"     = sort(colSums(is.na(dat))),
  "percentage NA" = round(sort(colMeans(is.na(dat))) * 100,  digits = 2)
)

sheetname <- paste0(
  stringr::str_extract(file_name, pattern = reg_ex),
  gsub("-", "", stringr::str_extract(file_name, pattern = "-[[:digit:]]+"))
)

writexl::write_xlsx(
  as_tibble(table1a_NA, rownames = "variable"),
  paste0("../Output/Imputation/Table1NA/Table-NA-1a-", sheetname, ".xlsx")
)

# See Note 1 at the top of the script:
if (!any(grepl(lm_df_name, imputed))) {
  stop("corresponding all-amputation landmark dataset was not imputed")
}


# IMPUTATION --------------------------------------------------------------

imp0 <- mice(dat, maxit = 0)

# 1. Choose imputation methods
imp_methods <- imp0$method
imp_methods["Region"]              <- ""
imp_methods["stroke"]              <- ""
imp_methods["ischemisk_hjsjukdom"] <- ""
imp_methods["albuminuria"]         <- ""
imp_methods["retinopati"]          <- ""

# 2. Update predictor matrix if needed
pred_matrix <- imp0$predictorMatrix
not_pred    <- c("LopNr", "surv_time", "Region", "albuminuria", "retinopati", "age", "stroke", "ischemisk_hjsjukdom")
pred_matrix[, not_pred] <- 0
pred_matrix[not_pred, ] <- 0

# 3. Change the visit sequence if needed
visit_seq <- imp0$visitSequence

# 4. Determine number of imputations
avg_pct_NA <- mean(
  table1a_NA[!rownames(table1a_NA) %in% not_pred, "percentage NA"]
)
if (avg_pct_NA > 20) {
  n_imputed_dfs <- avg_pct_NA
} else {
  n_imputed_dfs <- 20
}

# 5. Impute
if (sum(dat$surv_status %in% 1L) >= 1) {
  
  imp1 <- mice(
    dat, 
    m = n_imputed_dfs, 
    method = imp_methods, 
    predictorMatrix = pred_matrix, 
    visitSequence = visit_seq, 
    maxit = 20, 
    seed = 2023
  )
  
  saveRDS(
    imp1, 
    paste0("../Processed_data/11_imputed-datasets/imputed-", sheetname, ".rds")
  )
  
  # 6. Checking the settings
  check_mice_settings(imp_methods, imp1$method, sheetname)
  check_mice_settings(pred_matrix, imp1$predictorMatrix, sheetname)
  check_mice_settings(visit_seq, imp1$visitSequence, sheetname)
  
  
} else {
  print(paste0("Not enough events: ", sheetname))
}

x <- warnings()
if (!is.null(x)) {
  for (nm in names(x)) print(paste0("Warning ", sheetname, ": ", nm))
}


rm(list = ls())