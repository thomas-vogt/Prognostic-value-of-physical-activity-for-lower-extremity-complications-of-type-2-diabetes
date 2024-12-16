# NOTES:
# 1. Table 1b = general characteristics of each landmark dataset (landmark age, 
#    N, total follow-up time, n(%) censoring, n(%) y, n(%) death, % ccases).
# 2. Table 1c = Table 1 with all model covariates. 


rm(list = ls())


library(tidyverse)

source("07_functions.R")


mod_covs <- c(
  "sex", 
  "diag_age",
  "last_meas",
  "ischemisk_hjsjukdom", 
  "stroke", 
  "rokare", 
  "fysisk_aktivitet",
  "bmi",
  "hba1c",
  "systoliskt",
  "hdl",
  "kolesterol"
)

landmark_df <- list.files(
  "../Processed_data/06_landmark-datasets", 
  full.names = T
)


# TABLE 1B ----------------------------------------------------------------

out <- vector("list", length = length(landmark_df))

for (i in seq_along(landmark_df)) {
  y_name <- gsub(
    "(../Processed_data/06_landmark-datasets/landmark-|-[[:digit:]]+.rds)", 
    "", 
    landmark_df[i]
  )
  LM_dat <- readRDS(landmark_df[i])
  if (y_name != "neuropathy") mod_covs2 <- c(mod_covs, "neuropathy")
  if (y_name == "neuropathy") mod_covs2 <- mod_covs
  out[[i]] <- table1b(LM_dat, mod_covs2, y_name)
}

out <- dplyr::bind_rows(out)

writexl::write_xlsx(out, "../Output/Table_1b.xlsx")

p <- ggplot2::ggplot(out, ggplot2::aes(landmark_age, compl_cases)) + 
  ggplot2::geom_point() + 
  ggplot2::geom_line() +
  ggplot2::facet_wrap(ggplot2::vars(outcome)) +
  ggplot2::theme_bw()

png(
  filename = "../Output/plot_complete_cases.png",
  units = "in",
  width = 15,
  height = 12,
  res = 192,
)
p
dev.off()


# TABLE 1C ----------------------------------------------------------------

table1c <- vector("list", length = length(landmark_df))
for (i in seq_along(landmark_df)) {
  LM_dat <- readRDS(landmark_df[i])
  
  y_name <- gsub(
    "(../Processed_data/06_landmark-datasets/landmark-|-[[:digit:]]+.rds)", 
    "", 
    landmark_df[i]
  )
  
  landmark_age <- gsub(
    "-", "", stringr::str_extract(landmark_df[i], "-[[:digit:]]+")
  )
  
  LM_dat$sex                 <- factor(LM_dat$sex)
  LM_dat$rokare              <- factor(LM_dat$rokare)
  LM_dat$ischemisk_hjsjukdom <- factor(LM_dat$ischemisk_hjsjukdom)
  LM_dat$stroke              <- factor(LM_dat$stroke)
  LM_dat$fysisk_aktivitet    <- factor(
    LM_dat$fysisk_aktivitet, 
    levels = c("1", "2", "3", "4", "5"), 
    ordered = T
  )
  
  if (y_name != "neuropathy") {
    LM_dat$neuropathy <- factor(LM_dat$neuropathy)
    mod_covs2 <- c(mod_covs, "neuropathy")
  } else {
    mod_covs2 <- mod_covs
  }
  
  CC_LM_dat <- LM_dat[complete.cases(LM_dat[, mod_covs2]), ]
  
  out <- lapply(mod_covs2, function(x) {
    if (is.factor(LM_dat[[x]])) {
      stats_by_level <- levels(LM_dat[[x]]) %>% 
        purrr::set_names() %>% 
        purrr::map(function(nm) {
          n_obs   <- nrow(LM_dat[LM_dat[[x]] %in% nm, ])
          percent <- round((n_obs / nrow(LM_dat[!is.na(LM_dat[[x]]), ])) * 100, digits = 1)
          paste0(n_obs, " (", percent, "%)")
        }) %>% 
        unlist()
      stats_by_level2 <- levels(LM_dat[[x]]) %>% 
        purrr::set_names() %>% 
        purrr::map(function(nm) {
          n_obs   <- nrow(CC_LM_dat[CC_LM_dat[[x]] %in% nm, ])
          percent <- round((n_obs / nrow(CC_LM_dat[!is.na(CC_LM_dat[[x]]), ])) * 100, digits = 1)
          paste0(n_obs, " (", percent, "%)")
        }) %>% 
        unlist()
      summary_stat <- data.frame(
        outcome  = y_name,
        LM_age   = landmark_age,
        variable = x, 
        levels   = names(stats_by_level), 
        summary  = stats_by_level,
        missing  = round((sum(is.na(LM_dat[[x]])) / nrow(LM_dat)) * 100, digits = 1),
        summary2 = stats_by_level2)
    } else {
      summary_stat <- data.frame(
        outcome  = y_name,
        LM_age   = landmark_age,
        variable = paste0(x, ", mean(SD)"), 
        levels   = as.character(NA), 
        summary  = paste0(
          round(mean(LM_dat[[x]], na.rm = T), digits = 1), " (", 
          round(sd(LM_dat[[x]], na.rm = T), digits = 1), ")"
        ),
        missing  = round((sum(is.na(LM_dat[[x]])) / nrow(LM_dat)) * 100, digits = 1),
        summary2 = paste0(
          round(mean(CC_LM_dat[[x]], na.rm = T), digits = 1), " (", 
          round(sd(CC_LM_dat[[x]], na.rm = T), digits = 1), ")"
        )
      )
    }
  })
  
  out <- do.call(rbind, out)
  out[["last"]] <- c(paste0("n=", nrow(LM_dat), " vs ", "n=", nrow(CC_LM_dat)), rep(as.character(NA), times = nrow(out) - 1))
  names(out) <- c(
    names(out)[1:3], 
    c(
      "factor levels", 
      "Response sample", 
      "missing (%)", 
      "Complete case sample",
      "Response vs cc sample"
    )
  )
  
  table1c[[i]] <- out
}

table1c <- dplyr::bind_rows(table1c)

writexl::write_xlsx(table1c, "../Output/Table_1c.xlsx")


rm(list = ls())