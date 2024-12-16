# SET SURVIVAL TIME AND STATUS --------------------------------------------

set_survival <- function(dat, y) {
  
  dat$surv_time   <- pmin(dat[[y]],dat$death_age, dat$censor_age, na.rm = T)
  
  dat$surv_status <- case_when(
    near(dat$surv_time, dat[[y]])          ~ 1L,
    near(dat$surv_time, dat$death_age)     ~ 2L,
    near(dat$surv_time, dat$censor_age)    ~ 0L
  )
  
  return(dat)
}


# LOCF --------------------------------------------------------------------

locf_data <- function(dat) {
  dat %>%
    arrange(LopNr, age) %>% 
    group_by(LopNr) %>% 
    fill(dplyr::everything(), .direction = "down") %>% 
    ungroup() 
} 


# TABLE 1a ----------------------------------------------------------------

table1a <- function(dat) {
  y_name <- deparse(substitute(dat))
  dat |> 
    dplyr::select(LopNr, surv_status, surv_time, diag_age) |>
    dplyr::distinct() |>
    dplyr::summarise(
      n_pop                = n(),
      total_follow_up_time = sum(surv_time - diag_age),
      median_range_follow  = paste0(
        round(median(surv_time - diag_age), digits = 2), " [", 
        round(min(surv_time - diag_age), digits = 2), "-", 
        round(max(surv_time - diag_age), digits = 2), "]"
      ),
      pct_censoring        = (sum(surv_status == 0L) / n_pop) * 100,
      pct_y                = (sum(surv_status == 1L) / n_pop) * 100,
      pct_death_before_y   = (sum(surv_status == 2L) / n_pop) * 100
    ) |>
    dplyr::mutate(outcome = gsub("ndr_", "", y_name)) |>
    select(outcome, dplyr::everything())
}
