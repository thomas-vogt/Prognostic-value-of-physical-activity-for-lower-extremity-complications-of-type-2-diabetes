# NOTES:
# 1. Extracts date of birth for everyone in the NDR from the patient registers.
# 2. Some LopNrs have several dates of birth and misrecorded dates 
#    (e.g., "19800588").
# 3. If several dates remain after data cleaning, one date is randomly selected. 


rm(list = ls())


source("01_functions.R")


load_packages()
ndr_all  <- load_ndr_lopnrs()
path     <- "/castor/project/proj/Data_directory/"
pat_regs <- list(
  "Inpatient_register/Original_data/ut_r_par_sv_36759_2021.sas7bdat",
  "Outpatient_register/Original_data/ut_r_par_ov_36759_2021.sas7bdat"
)


dob <- lapply(
  pat_regs, 
  function(x) {
    read_sas(paste0(path, x), col_select = c("LopNr", "FODDAT")) %>%
      mutate(LopNr = as.integer(LopNr)) %>% 
      semi_join(ndr_all, by = "LopNr") %>% 
      mutate(date_of_birth = lubridate::ymd(FODDAT))
  }
)

dob <- distinct(do.call(rbind, dob))


# DUPLICATES AND MISRECORDED DATES ----------------------------------------


dob_duplicates <- dob %>%
  count(LopNr) %>% 
  filter(n > 1)

dob_duplicates <- semi_join(dob, dob_duplicates, by = "LopNr")
dob <- dob %>%
  select(-FODDAT) %>% 
  anti_join(dob_duplicates, by = "LopNr")

# For inspection of the misrecorded dates.
dob_duplicates %>% 
  semi_join(dob_duplicates[is.na(dob_duplicates$date_of_birth),], "LopNr") %>% 
  arrange(LopNr, as.integer(FODDAT))

dob_duplicates <- dob_duplicates %>% 
  arrange(LopNr, as.integer(FODDAT)) %>% 
  mutate(FODDAT = if_else(is.na(date_of_birth), as.character(NA), FODDAT)) %>% 
  group_by(LopNr) %>% 
  mutate(FODDAT = DescTools::LOCF(FODDAT)) %>%
  ungroup() %>% 
  mutate(date_of_birth = lubridate::ymd(FODDAT)) %>% 
  mutate(
    date_of_birth = if_else(
      is.na(date_of_birth), 
      lubridate::ymd(paste0(substr(FODDAT, 1, 6), "16")), 
      date_of_birth
    )
  ) %>% 
  select(-FODDAT) %>% 
  distinct()

set.seed(654214)

dob_duplicates <- by(
  dob_duplicates, 
  dob_duplicates$LopNr, 
  function(x) x[sample(1:nrow(x), 1), ]
)

dob_duplicates <- do.call(rbind, dob_duplicates)


# END DUPLICATES SECTION --------------------------------------------------


dob <- distinct(rbind(dob, dob_duplicates))

length(unique(dob$LopNr))

write_parquet(
  dob, 
  "../Processed_data/01_extracted-date-of-birth.parquet"
)

rm(list = ls())
