# NOTES:
# Script to be sourced from 21_modelling-imputed-data.R

library(prodlim)
library(riskRegression)

source("22_functions.R")

n_cv <- 1:500


# RANDOMLY SELECT AN IMPUTED DATA SET -------------------------------------

set.seed(bootstrapCV_seed)
sample_data <- sample(1:max(imp2$.imp), size = 1)
dat <- imp2[imp2$.imp %in% sample_data, ]

covs_mod2 <- paste(covariates, sep = "", collapse = " + ")
covs_mod3 <- paste(c(covariates, phys_activity), sep = "", collapse = " + ")


# MODEL ASSESSMENT --------------------------------------------------------

met_NA <- F
cvfgr_results <- vector("list", length = length(n_cv))
for (i in n_cv) {
  cvfgr_results[[i]] <- cv_fgr(dat, ipcw_covs = covs_mod3)
  if (is.na(cvfgr_results[[i]]$delta_auc) 
      || is.nan(cvfgr_results[[i]]$delta_auc) 
      || is.na(cvfgr_results[[i]]$delta_brier) 
      || is.nan(cvfgr_results[[i]]$delta_brier)) {
    met_NA <- T
    break
  }
}

if (met_NA) {
  for (i in n_cv) {
    cvfgr_results[[i]] <- cv_fgr(dat, ipcw_covs = "1")
  }
} 


# RESHAPE RESULTS AND SAVE ------------------------------------------------

cvfgr_results <- names(cvfgr_results[[1]]) %>% 
  set_names() %>% 
  lapply(
    \(i) {
      lapply(seq_along(cvfgr_results), \(j) cvfgr_results[[j]][[i]]) %>% 
        bind_rows()
    }
  )

saveRDS(
  cvfgr_results,
  paste0(
    "../Processed_data/22_performance-imputed/",
    y_name,
    landmark_age,
    ".rds"
  )
)
