
# TIDY RESULTS FROM A CRR MODEL -------------------------------------------

df_results_mice <- function(mipo_object, mipo_name) {
  z1 <- summary(mipo_object, conf.int = 0.95, exponentiate = T)[, c(1:2, 6:8)]
  names(z1) <- c(names(z1)[1:3], c("conf.low", "conf.high"))
  z2 <- summary(mipo_object, conf.int = 0.95, exponentiate = F)[, c(1:3, 7:8)]
  names(z2) <- c("term", "log_estimate", "std.error", "log_conf.low", "log_conf.high")
  z <- left_join(z1, z2, by = "term")
  z$outcome       <- str_extract(mipo_name, "(neuropathy|ulcer|major-amputation|all-amputation)")
  z$landmark_age  <- str_extract(names(pooled_res)[i], "[[:digit:]]{2,3}")
  z$convergence   <- all(mipo_object$glanced$converged)
  z$mipo_name <- mipo_name
  z
}
 
df_results <- function(crr_mod, Yname, LMage) {
  z <- summary(crr_mod, conf.int = 0.95)
  out <- dplyr::left_join(
    tibble::rownames_to_column(as.data.frame(z[["conf.int"]][, -2]), var = "term"),
    tibble::rownames_to_column(as.data.frame(z[["coef"]][, c("coef", "se(coef)", "p-value")]), var = "term"),
    by = "term"
  )
  names(out) <- c("term", "estimate", "conf.low", "conf.high", "log_estimate", "std.error", "p.value")
  
  out <- out[, c("term", "estimate", "p.value", "conf.low", "conf.high", "log_estimate", "std.error")]
  
  out$log_conf.low  <- log(out$conf.low)
  out$log_conf.high <- log(out$conf.high)
  out$outcome       <- Yname
  out$landmark_age  <- LMage
  out$convergence   <- z$converged
  out
}


# GATHER CRR RESULTS FROM AN OUTCOME AT A CERTAIN LANDMARK AGE ------------

crr_results <- function(file_name) {
  
  res <- readRDS(file_name)
  res <- res[sapply(res, is.list)]
  
  lm_age <- gsub(
    "-", "", stringr::str_extract(file_name, pattern = "-[[:digit:]]+")
  )
  y_name <- stringr::str_extract(
    file_name, 
    pattern = "(neuropathy|ulcer|major-amputation|all-amputation)"
  )
  
  out <- lapply(
    res, 
    function(i) df_results(i, Yname = y_name, LMage = lm_age)
  )
  
  for (i in seq_along(out)) out[[i]][["new_var"]] <- names(out)[i]

  out <- dplyr::bind_rows(out)
  out <- tidyr::separate(
    out, new_var, into = c("event", "subgroup", "model"), sep = "\\."
  )
  out <- dplyr::select(
    out, outcome, landmark_age, event, subgroup, model, dplyr::everything()
  )
  out$model <- dplyr::if_else(
    grepl("model", out$subgroup), out$subgroup, out$model
  )
  out$subgroup <- dplyr::if_else(
    grepl("model", out$subgroup), as.character(NA), out$subgroup
  )
  out
}
