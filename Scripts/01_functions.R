# NOTES:
# Functions.


# LOAD LIBRARIES

load_packages <- function() {
  library(haven)
  library(tidyverse)
  library(arrow)
}


# LOAD REGULAR EXPRESSIONS

load_regex <- function() {
  # Some codes are identical between ICD-8 and ICD-7, but they don't
  # mean the same thing: "250,00" is also in ICD-7; "260,99" is also in ICD-8.
  # I remove "260,99" from the ICD-7 codes: there is only one occurrence here,
  # and in 1968, so in this case it is an ICD-8 code (see Ludvigsson, 2011).
  # All the "250,00" are recorded from 1969, so they are really ICD-8 codes.

  strings_icd10 <- str_c(
    c("(^E10", "^E11", "^E13", "^E14", "^O240", "^O241", "^O243", "^O244", "^O249)"), 
    collapse = ")|("
  )

  strings_icd9 <- str_c(
    c("(^250A", "^250B", "^250C", "^250D", "^250E", "^250F", "^250G", "^250H", "^250X", "^648A)"), 
    collapse = ")|("
  )

  strings_icd8 <- str_c(
    c(
      "(^250,00", "^250,01", "^250,02", "^250,03", "^250,04", "^250,05", "^250,06",
      "^250,07", "^250,08", "^250,09)"
    ), 
    collapse = ")|("
  )                                          

  strings_icd7 <- str_c(
    c("(^260,09", "^260,20", "^260,21", "^260,29", "^260,30", "^260,40", "^260,49)"), 
    collapse = ")|("
  )                                          

  strings_icd <- str_c(
    c(strings_icd10, strings_icd9, strings_icd8, strings_icd7), 
    collapse = "|"
  )

  strings_surgcodes <- c(
    "NEQ19", "NFQ09", "NFQ19", "NGQ09", "NGQ19", "NHQ09",
    "NHQ11", "NHQ12", "NHQ13", "NHQ14",
    "NHQ16", "NHQ17", # Toe amputations.
    "NGQ99", "NFQ99", "NHQ99", "NEQ99") # "Other amputation or related operation of [...]"

    assign("strings_icd", strings_icd, pos = .GlobalEnv)
    assign("strings_icd10", strings_icd10, pos = .GlobalEnv)
    assign("strings_surgcodes", strings_surgcodes, pos = .GlobalEnv)
}


# LOAD NDR LOPNRS

load_ndr_lopnrs <- function() {
  path    <- "/castor/project/proj/Data_directory/National_diabetes_register/"
  ndr_all <- read_sas(
    paste0(path, "Original_data/lopnr_ndr_15352_2020.sas7bdat"),
    col_select = "LopNr") %>% 
    mutate(LopNr = as.integer(LopNr))
  return(ndr_all)
}


# EXTRACT DATA FROM THE PATIENT REGISTERS

extract_PR_data <- function(filename, date_variable) {
  patient_reg <- read_sas(
    filename,
    col_select = c(
      "LopNr",
      "OP",
      matches(c("^DIA[[:digit:]]", "^EKOD[[:digit:]]")),
      all_of(date_variable)
    )) %>% 
    mutate(LopNr = as.integer(LopNr)) %>% 
    semi_join(ndr_all, by = "LopNr")
  
# Data frame without surgical codes.
  patient_reg_icd <- patient_reg %>%                                                  
    select(-starts_with(c("OP", "EKOD")))%>% 
    pivot_longer(
      cols = starts_with("DIA"),
      names_to = "diagnosis_number",
      values_to = "diagnosis_code") %>% 
    filter(str_detect(diagnosis_code, strings_icd))

# Data frame without ICD codes.
# count the number of spaces in a cell, take the max, and add 2.
  max_opnumber <- max(nchar(gsub("[^ ]", "", patient_reg$OP))) + 2
  strings_opnumber <- paste0("OP", 1:max_opnumber)

  patient_reg <- patient_reg %>% 
    select(-starts_with("DIA")) %>% 
    separate(OP, into = strings_opnumber, sep = " ", remove = T) %>% 
    pivot_longer(
      cols = starts_with("OP"),
      names_to = "operation_number",
      values_to = "surgical_code") %>% 
    filter(surgical_code %in% strings_surgcodes) 

# Re-join the two data frames.
  patient_reg <- full_join(patient_reg_icd, patient_reg, by = c("LopNr", date_variable))
  rm(patient_reg_icd)

  for (nm in names(patient_reg)) {
    if (all(is.na(patient_reg[, nm]))) patient_reg[, nm] <- NULL
  }
  return(patient_reg)
}


# RANDOM DATE 

rdate <- function(x, n) {                                                       
  # Function to draw random date(s) from a year.
  y <- seq(
    as.Date(paste0(x, "-01-01")), 
    as.Date(paste0(x, "-12-31")), 
    by = "day"
  )
  return(sample(y, size = n, replace = T))
}


# REPOPULATE CELLS AFTER FULL JOIN ----------------------------------------

twoway_locf <- function(var_name) {
  ndr %>% 
    arrange(LopNr, record_date) %>% 
    group_by(LopNr) %>% 
    mutate("{{var_name}}" := DescTools::LOCF({{ var_name }})) %>%
    ungroup() %>% 
    arrange(LopNr, desc(record_date)) %>% 
    group_by(LopNr) %>% 
    mutate("{{var_name}}" := DescTools::LOCF({{ var_name }}))
}

repop_cells <- function(var_name) {
  # The variable in var_name needs to be constant for the same individual.
  no_duplicates <- distinct(ndr[!is.na(ndr[[var_name]]), c("LopNr", var_name)])
  ndr[[var_name]] <- NULL
  return(left_join(ndr, no_duplicates, by = "LopNr"))
}
