# NOTES:
# 1. Script to extract information from multiple imputations (e.g., formulas).


rm(list = ls())


library(magrittr)
library(mice)

source("11_functions.R")


file_names <- list.files("../Processed_data/11_imputed-datasets", full.names = T)
names(file_names) <- gsub(
  ".rds",
  "",
  gsub("../Processed_data/11_imputed-datasets/imputed-", "", file_names)
)


imp_dfs <- lapply(file_names, readRDS)


out        <- vector("list", length = length(imp_dfs))
names(out) <- names(imp_dfs)
for (nm in names(out)) {
  landmark_df <- nm
  imputed_var <- names(imp_dfs[[nm]][["method"]])
  imp_method  <- imp_dfs[[nm]][["method"]]
  imp_covs    <- sapply(imp_dfs[[nm]][["formulas"]], function(x) as.character(x)[3])
  n_datasets  <- imp_dfs[[nm]][["m"]] 
  
  imp_models <- data.frame(landmark_df, imputed_var, imp_method, imp_covs, n_datasets)
  out[[nm]]  <- imp_models[imp_models[["imp_method"]] != "", ]
}

out <- dplyr::bind_rows(out)

writexl::write_xlsx(out, "../Output/Imputation/imputation-models.xlsx")


# CHECKS ------------------------------------------------------------------

# 1. Logged events

logged_events <- names(imp_dfs) %>% 
  purrr::set_names() %>% 
  lapply( 
    function(j) if (!is.null(imp_dfs[[j]]$loggedEvents)) imp_dfs[[j]]$loggedEvents
  )

logged_events <- logged_events[!sapply(logged_events, is.null)]

if (length(logged_events) > 0) {
  writexl::write_xlsx(logged_events, "../Output/Imputation/Imputation-logged-events.xlsx")
}

for (nm in names(imp_dfs)) {
  
  # 2. Convergence
  
  n_layout <- ceiling(sqrt(length(imp_dfs[[nm]]$method[!imp_dfs[[nm]]$method %in% ""])) * 1.4)
  png(
    filename = paste0("../Output/Imputation/traceplot-", nm, ".png"),
    units = "in",
    width = 15,
    height = 12,
    res = 192,
  )
  print(plot(imp_dfs[[nm]], layout = c(n_layout, n_layout)))
  dev.off()
  
  # 3. Imputed values
  
  png(
    filename = paste0("../Output/Imputation/densityplot-", nm, ".png"),
    units = "in",
    width = 15,
    height = 12,
    res = 192,
  )
  print(densityplot(data = ~ DispInk04 + systoliskt + bmi + hba1c + hdl + kolesterol, x = imp_dfs[[nm]]))
  dev.off()
  
  png(
    filename = paste0("../Output/Imputation/propplot-", nm, ".png"),
    units = "in",
    width = 15,
    height = 12,
    res = 192,
  )
  propplot(imp_dfs[[nm]])
  dev.off()
  
}


rm(list = ls())