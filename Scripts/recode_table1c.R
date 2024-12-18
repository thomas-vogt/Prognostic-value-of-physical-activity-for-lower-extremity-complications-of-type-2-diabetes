rm(list = ls())

library(tidyverse)

table1c <- readxl::read_excel("Table_1c.xlsx")


# table1c <- left_join(
#   table1c[, -ncol(table1c)],
#   table1c[!is.na(table1c$`Response vs cc sample`), c(1, 2, ncol(table1c))],
#   by = c("outcome", "LM_age")
# )

names(table1c)[4] <- "fct_levels"


table1c <- table1c %>% 
  mutate(
    fct_levels = case_when(
      variable == "sex" & fct_levels == "1"                 ~ "Males",
      variable == "sex" & fct_levels == "2"                 ~ "Females",
      variable == "ischemisk_hjsjukdom" & fct_levels == "0" ~ "No",
      variable == "ischemisk_hjsjukdom" & fct_levels == "1" ~ "Yes",
      variable == "stroke" & fct_levels == "0"              ~ "No",
      variable == "stroke" & fct_levels == "1"              ~ "Yes",
      variable == "rokare" & fct_levels == "0"              ~ "No",
      variable == "rokare" & fct_levels == "1"              ~ "Yes",
      variable == "fysisk_aktivitet" & fct_levels == "1"    ~ "Never",
      variable == "fysisk_aktivitet" & fct_levels == "2"    ~ "<1 times/week",
      variable == "fysisk_aktivitet" & fct_levels == "3"    ~ "regularly - 1-2 times/week",
      variable == "fysisk_aktivitet" & fct_levels == "4"    ~ "regularly - 3-5 times/week",
      variable == "fysisk_aktivitet" & fct_levels == "5"    ~ "Daily",
      variable == "neuropathy" & fct_levels == "0"          ~ "No",
      variable == "neuropathy" & fct_levels == "1"          ~ "Yes",
      .default = fct_levels
    ),
    variable = case_when(
      variable == "sex"                 ~ "Sex",
      variable == "ischemisk_hjsjukdom" ~ "Ischemic heart disease",
      variable == "stroke"              ~ "Stroke",
      variable == "rokare"              ~ "Smoking status",
      variable == "fysisk_aktivitet"    ~ "Self-reported physical activity",
      variable == "neuropathy"          ~ "Neuropathy",
      grepl("diag_age", variable)       ~ gsub("diag_age", "Age at diagnosis of type 2 diabetes", variable),
      grepl("last_meas", variable)      ~ gsub("last_meas", "Time since last healthcare visit", variable),
      grepl("bmi", variable)            ~ gsub("bmi", "BMI", variable),
      grepl("hba1c", variable)          ~ gsub("hba1c", "HbA1c", variable),
      grepl("systoliskt", variable)     ~ gsub("systoliskt", "Systolic blood pressure", variable),
      grepl("hdl", variable)            ~ gsub("hdl", "HDL-cholesterol", variable),
      grepl("kolesterol", variable)     ~ gsub("kolesterol", "Total cholesterol", variable)
    ),
    variable = if_else(grepl("mean", variable), variable, paste0(variable, ", n(%)")),
    outcome = case_when(
      outcome == "all-amputation"   ~ "Amputation",
      outcome == "major-amputation" ~ "Major amputation",
      outcome == "neuropathy"       ~ "Neuropathy",
      outcome == "ulcer"            ~ "Ulcer"
    ),
    LM_age = as.integer(LM_age),
   `Response vs cc sample` = if_else(is.na(`Response vs cc sample`), "", `Response vs cc sample`)
  ) 

names(table1c) <- c(
  "Outcome", 
  "Landmark age", 
  "Variable", 
  "Factor level", 
  "Response sample", 
  "Missing (%)", 
  "Complete cases", 
  "Response sample vs complete cases"
)

writexl::write_xlsx(table1c, "Table_1c_cleaned.xlsx")


rm(list = ls())