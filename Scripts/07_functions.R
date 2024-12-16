
# TABLE1b -----------------------------------------------------------------

table1b <- function(dat, model_covs, outcome_name) {
  # Returns % complete cases for the data containing only the model covariates. 
  dat2 <- dat[, model_covs]
  ccases <- nrow(dat2[complete.cases(dat2), ]) / nrow(dat2)
  
  dat |> 
    dplyr::select(LopNr, surv_status, surv_time, diag_age, age) |>
    dplyr::summarise(
      n_pop                = dplyr::n(),
      total_follow_up_time = round(sum(surv_time), digits = 0),
      n_censoring          = sum(surv_status == 0L),
      pct_censoring        = round((n_censoring / n_pop) * 100, digits = 1),
      n_y                  = sum(surv_status == 1L),
      pct_y                = round((n_y / n_pop) * 100, digits = 1),
      n_death              = sum(surv_status == 2L),
      pct_death            = round((n_death / n_pop) * 100, digits = 1)
    ) |>
    dplyr::mutate(
      outcome      = outcome_name,
      landmark_age = dat[["age"]][1],
      compl_cases  = round(ccases * 100, digits = 1)
    ) |>
    dplyr::select(landmark_age, outcome, dplyr::everything())
}
