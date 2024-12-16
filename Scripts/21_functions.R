
# MODELLING FUNCTION ------------------------------------------------------

modelling <- function(dat, failure_code, covs) {
  
  # This function computes three models with different sets of covariates each
  # and return a list of the models.
  # The only arguments required are a data frame, the code for the failure
  # of interest and a vector of covariate names.
  
  mod1 <- crr(
    ftime = dat[["surv_time"]], 
    fstatus = dat[["surv_status"]], 
    cov1 = dat[, phys_activity], 
    failcode = failure_code, 
    cencode = 0,
    na.action = na.omit, 
    gtol = 1e-06, 
    maxiter = 50, 
    variance = T
  )
  mod2 <- update(
    mod1, 
    cov1 = dat[, covs], 
    evaluate = T
  )
  mod3 <- update(
    mod2, 
    cov1 = dat[, c(covs, phys_activity)], 
    evaluate = T
  )
  return(list(model1 = mod1, model2 = mod2, model3 = mod3))
}


# MODELLING FUNCTION FOR IMPUTED DATA -------------------------------------

crr_imputed <- function(original_mids, imp_df, fail_code, adjustment_covs) {
  # imp_dfs should be an imputed data frame containing stacked imputed datasets
  # originally coming from a mids object. Can be obtained with mice::complete().
  
  imp_dfs <- split(imp_df, imp_df$.imp)[-1]
  
  out3 <- out2 <- out1 <- list(analyses = vector("list", length(imp_dfs)))
  
  for (i in seq_along(imp_dfs)) {
    model_res <- modelling(imp_dfs[[i]], fail_code, adjustment_covs)
    out1$analyses[[i]] <- model_res[["model1"]]
    out2$analyses[[i]] <- model_res[["model2"]]
    out3$analyses[[i]] <- model_res[["model3"]]
  }
  
  model_list <- list(model1 = out1, model2 = out2, model3 = out3)
  
  out_list <- vector("list", length = length(model_list))
  names(out_list) <- names(model_list)
  for (nm in names(model_list)) {
    res <- as.mira(model_list[[nm]])
    res <- list(
      call     = model_list[[nm]]$analyses[[1]]$call, 
      call1    = original_mids$call, 
      nmis     = original_mids$nmis, 
      analyses = model_list[[nm]]$analyses
    )
    oldClass(res)  <- "mira"
    out_list[[nm]] <- res
  }
  out_list
}


# CONVERT TO DUMMY VARIABLES AND MORE -------------------------------------

to_dummy <- function(LM_df, recode_ihd_stroke = T) {
  dat <- mutate(
    LM_df,
    fysisk_aktivitet_1 = if_else(fysisk_aktivitet == 1L, 1L, 0L),
    fysisk_aktivitet_2 = if_else(fysisk_aktivitet == 2L, 1L, 0L),
    fysisk_aktivitet_3 = if_else(fysisk_aktivitet == 3L, 1L, 0L),
    fysisk_aktivitet_4 = if_else(fysisk_aktivitet == 4L, 1L, 0L),
    sex_2              = if_else(sex == 2L, 1L, 0L),
    tc_hdl             = kolesterol / hdl,
    albuminuria_0      = if_else(albuminuria == 0L, 1L, 0L),
    albuminuria_2      = if_else(albuminuria == 2L, 1L, 0L),
    albuminuria_3      = if_else(albuminuria == 3L, 1L, 0L),
    sex       = factor(sex, levels = c("1", "2"), labels = c("Male", "Female")),
    education = factor(
      education, 
      levels = c("1", "2"), 
      labels = c("EducLow", "EducMediumHigh")
    )
  )
  
  if (recode_ihd_stroke == T) {
    dat <- mutate(
      dat,
      ihd_stroke = if_else(
        ischemisk_hjsjukdom == 1L | stroke == 1L, 
        1L, 
        as.integer(NA)
      ),
      ihd_stroke = if_else(
        is.na(ihd_stroke) & ischemisk_hjsjukdom == 0L & stroke == 0L, 
        0L, 
        ihd_stroke
      )
    )
  }
  
  return(dat)
}


# COVARIATES --------------------------------------------------------------

load_covariates <- function() {
  covariates <- c(
    "diag_age",
    "bmi",
    "hba1c",
    "rokare",
    "systoliskt",
    "tc_hdl",
    "ihd_stroke",
    "last_meas",
    "sex_2"
  )
  assign("covariates", covariates, pos = .GlobalEnv)
}


# REMOVE COLUMN IF IT ONLY TAKES ONE VALUE --------------------------------

rm_if_unique <- function(LM_df, cov_names) {
  out <- cov_names
  for (nm in c(cov_names)) {
    if (length(unique(LM_df[!is.na(LM_df[[nm]]), nm, drop = T])) == 1L) {
      out <- base::setdiff(out, nm)
    }
  }
  out
}
