rm(list = ls())

library(tidyverse)

table_2 <- list(
  table2_cc_cases = readxl::read_excel("Table2_all_results_cc_cases.xlsx"),
  table2_mice     = readxl::read_excel("Table2_all_results_mice.xlsx")
)

for (nm in names(table_2)) {
  table_2[[nm]] <- table_2[[nm]] %>% 
    mutate(
      term = case_when(
        term == "fysisk_aktivitet_1" ~ "Never (vs daily)",
        term == "fysisk_aktivitet_2" ~ "<1 times/week (vs daily)",
        term == "fysisk_aktivitet_3" ~ "Regularly - 1-2 times/week (vs daily)",
        term == "fysisk_aktivitet_4" ~ "Regularly - 3-5 times/week (vs daily)",
        term == "diag_age"           ~ "Age at diagnosis of type 2 diabetes",
        term == "bmi"                ~ "BMI",
        term == "hba1c"              ~ "HbA1c",
        term == "rokare"             ~ "Smoking status",
        term == "systoliskt"         ~ "Systolic blood pressure",
        term == "tc_hdl"             ~ "Total:HDL-cholesterol ratio",
        term == "ihd_stroke"         ~ "Past cardiovascular event(s)",
        term == "last_meas"          ~ "Time since last healthcare visit",
        term == "sex_2"              ~ "Sex",
        term == "neuropathy"         ~ "Peripheral neuropathy/peripheral vascular disease"
      ),
      subgroup = case_when(
        subgroup == "EducLow"        ~ "Low education",  
        subgroup == "EducMediumHigh" ~ "High education", 
        .default = subgroup
      ),
      outcome = case_when(
        outcome == "all-amputation"   ~ "Amputation",
        outcome == "major-amputation" ~ "Major amputation",
        outcome == "neuropathy"       ~ "Neuropathy",
        outcome == "ulcer"            ~ "Ulcer"
      ),
      model = gsub("model", "model ", model),
      landmark_age = as.integer(landmark_age)
    )
  
  names(table_2[[nm]]) <- c(
    "Outcome", 
    "Landmark age", 
    "Subgroup", 
    "Model", 
    "Term", 
    "Estimate", 
    "p-value", 
    "conf.low",
    "conf.high",
    "log-estimate",
    "std.error",
    "log-conf.low",
    "log-conf.high",
    "Convergence"
  )
  
  
  writexl::write_xlsx(table_2[[nm]], paste0(gsub("table2", "Table2_all_results", nm), "_cleaned.xlsx"))
}

rm(list = ls())