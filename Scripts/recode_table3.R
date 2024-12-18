rm(list = ls())

library(tidyverse)

table_3 <- readxl::read_excel("Table3_results_performance.xlsx")

table_3 <- table_3 %>% 
  select(-subgroup) %>% 
  mutate(
    outcome = case_when(
      outcome == "all-amputation"   ~ "Amputation",
      outcome == "major-amputation" ~ "Major amputation",
      outcome == "neuropathy"       ~ "Neuropathy",
      outcome == "ulcer"            ~ "Ulcer"
    ),
    landmark_age = as.integer(landmark_age),
    term = case_when(
      grepl("delt", term) ~ gsub("delta", "delta ", term),
      grepl("mod3", term) ~ gsub("mod3", "model 3 ", term),
      grepl("mod2", term) ~ gsub("mod2", "model 2 ", term),
    )
  )

names(table_3) <- c(
  "Outcome", 
  "Landmark age",
  "Term", 
  "Estimate", 
  "conf.low",
  "conf.high"
)

writexl::write_xlsx(table_3, "Table3_results_performance_cleaned.xlsx")

rm(list = ls())