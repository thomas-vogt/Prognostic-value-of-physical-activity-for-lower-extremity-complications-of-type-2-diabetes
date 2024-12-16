# NOTES:
# 1. Time variable = age.
# 2. Less than 2% of the rows are missing LISA variables.


rm(list = ls())


library(lubridate)
library(tidyverse)

source("04_functions.R")
source("01_functions.R")


ndr <- arrow::read_parquet(
  "../Processed_data/03_joined-datasets.parquet", 
  as_data_frame = T
)

path <- "/castor/project/proj/Data_directory/LISA/Original_data/"


# MISSING DATE OF BIRTH ---------------------------------------------------


grunduppg <- haven::read_sas(
  paste0(path, "brook_lev_grunduppg.sas7bdat"),
  col_select = c("LopNr", "Fodelsear")
) %>% 
  mutate(LopNr = as.integer(LopNr)) # no_dob[no_dob$AterPNr == 1L, ]

no_dob <- distinct(ndr[is.na(ndr$date_of_birth), c("LopNr")])
no_dob <- left_join(no_dob, grunduppg, by = "LopNr")
no_dob$rdate_of_birth <- ymd(NA)

set.seed(5761)
for (i in seq_along(no_dob$LopNr)) {
  no_dob$rdate_of_birth[i] <- rdate(as.integer(no_dob$Fodelsear[i]), 1)
}

ndr <- ndr %>% 
  left_join(no_dob[, c("LopNr", "rdate_of_birth")], by = "LopNr") %>% 
  mutate(date_of_birth = if_else(
    is.na(date_of_birth), 
    rdate_of_birth, 
    date_of_birth)) %>% 
  select(-rdate_of_birth) %>% 
  filter(as.integer(diag_date - date_of_birth) / 365.25 >= 18)

length(unique(ndr$LopNr))


# NDR VARIABLES -----------------------------------------------------------


ndr <- ndr %>% 
  mutate(
    year       = as.integer(format(record_date, "%Y")),
    age        = as.integer(record_date - date_of_birth) / 365.25,
    emig_age   = as.integer(emig_date - date_of_birth) / 365.25,
    death_age  = as.integer(DODSDAT - date_of_birth) / 365.25,
    censor_age = pmin(
      as.integer(ymd("2019-12-31") - date_of_birth) / 365.25, 
      emig_age, 
      na.rm = T
    ),
    diag_age   = as.integer(diag_date - date_of_birth) / 365.25,
    diab_durat = age - diag_age,
    last_meas  = if_else(
      is.na(last_meas),
      diag_age,
      as.integer(last_meas - date_of_birth) / 365.25
      ),
    insulin    = if_else(ins_metod %in% 1:2, 1L, 0L)
  ) 

y_vars  <- c("neuropathy", "foot_ulcer", "any_amp", "maj_amp")
y_names <- c("neuro_fail", "ulcer_fail", "any_amp_fail", "maj_amp_fail")
for (i in seq_along(y_vars)) {
  ndr <- failure_date(var_name = y_vars[i], new_var = y_names[i])
}


# LISA VARIABLES ----------------------------------------------------------


ndr <- arrow::read_parquet(
  "../Processed_data/01_extracted-lisa-data.parquet", 
  as_data_frame = T
) %>% 
  semi_join(ndr, by = "LopNr") %>% 
  mutate(
    year      = lisa_year + 1L,
    education = as.factor(case_when(
      substr(Sun2000niva, 1, 1) %in% c("0", "1", "2") ~ 1L,
      substr(Sun2000niva, 1, 1) %in% c("3")           ~ 2L,
      substr(Sun2000niva, 1, 1) %in% c("4", "5", "6") ~ 2L
    )),
  ) %>% 
  filter(!is.na(education)) %>%
  select(LopNr, year, education, DispInk04, ALosDag, Civil, Kommun) %>% 
  distinct() %>% 
  right_join(ndr, by = c("LopNr", "year"))

ndr <- haven::read_sas(
  paste0(path, "brook_lev_lisa_bakgrund.sas7bdat"),
  col_select = c("LopNr", "FodGrEg4")
) %>% 
  mutate(LopNr = as.integer(LopNr)) %>% 
  semi_join(ndr, by = "LopNr") %>% 
  filter(FodGrEg4 != "11") %>% 
  rename(birth_country = FodGrEg4) %>% 
  distinct() %>% 
  right_join(ndr, by = "LopNr")

arrow::write_parquet(ndr, "../Processed_data/04_set-up-data.parquet")


rm(list = ls())