# BOOTSTRAP CV ------------------------------------------------------------

cv_fgr <- function(dat, ipcw_covs) {
  
  # Check that 1 row == 1 LopNr
  if (length(unique(dat$LopNr)) != nrow(dat)) stop("Not 1 LopNr per row")
  
  # Create training data frame by sampling with replacement
  n <- nrow(dat)
  train <- sample(1:n, size = n, replace = T)
  training_data <- dat[train, ]
  
  # Create validation data frame that includes IDs not in the training data
  train_ids <- unique(training_data$LopNr)
  validation_data <- dat[!(dat$LopNr %in% train_ids), ]

  # Check that there is no overlap between training and validation data
  if (any(training_data$LopNr %in% validation_data$LopNr)) stop("Data leakage")
  
  # Fit models using training data
  crr_mod2 <- FGR(
    formula = as.formula(paste("Hist(surv_time, surv_status) ~ ", covs_mod2)),
    data = training_data, 
    cause = 1, 
    y = F,
    na.action = na.omit, 
    gtol = 1e-06, 
    maxiter = 50,
    variance = T
  )
  crr_mod3 <- update(
    crr_mod2,
    formula = as.formula(paste("Hist(surv_time, surv_status) ~ ", covs_mod3))
  )
  
  # Calculate AUC and Brier score in the validation data
  model_assessment <- Score(
    list("model2" = crr_mod2,"model3" = crr_mod3),
    formula = as.formula(paste("Hist(surv_time, surv_status) ~ ", ipcw_covs)),
    data = validation_data,
    metrics = c("auc", "brier"),
    plots = NULL,
    cause = 1,
    times = 5,
    use.event.times = F,
    null.model = F,
    se.fit = F,
    conservative = F,
    multi.split.test = F,
    conf.int = F,
    cens.method = "ipcw",
    cens.model = "cox",
  )
  
  # Retrieve AUC and Brier score for the two models
  x <- model_assessment$AUC$score[, c(1, 3)]
  x1 <- x[[2]]
  names(x1) <- x[[1]]
  
  y <- model_assessment$Brier$score[, c(1, 3)]
  y1 <- y[[2]]
  names(y1) <- y[[1]]
  
  # Add change in AUC and in Brier score, and return a list
  out <- list(
    auc = x1,
    brier = y1,
    delta_auc   = c(delta_auc = model_assessment$AUC$contrasts$delta.AUC),
    delta_brier = c(delta_brier = model_assessment$Brier$contrasts$delta.Brier)
  )
  return(out)
}
