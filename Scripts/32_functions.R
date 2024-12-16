# MEAN DELTA, MEASURE OF PERF, AND 95% CI ---------------------------------

compute_performance <- function(x) {
  # Compute mean and 95% CIs from a vector of values (e.g., vector of AUCs)
  point_estimate <- mean(x)
  estimateCI <- quantile(x, probs = c(0.025, 0.975), type = 7)
  res <- c(point_estimate, estimateCI)
  names(res) <- c("estimate", names(estimateCI))
  return(res)
}

computePerformance <- function(dat) {
  # Wrapper around compute_performance()
  out <- list(
    deltaAUC   = compute_performance(dat$delta_auc$delta_auc),
    deltaBrier = compute_performance(dat$delta_brier$delta_brier),
    mod3AUC    = compute_performance(dat$auc$model3),
    mod3Brier  = compute_performance(dat$brier$model3),
    mod2AUC    = compute_performance(dat$auc$model2),
    mod2Brier  = compute_performance(dat$brier$model2)
  )
  out <- lapply(out, \(x) as.data.frame(t(x)))
  return(out)
}
