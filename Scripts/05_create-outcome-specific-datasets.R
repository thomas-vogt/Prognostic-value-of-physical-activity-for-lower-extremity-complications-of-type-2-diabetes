# NOTES:
# 1. Save one dataset for each outcome after applying outcome-specific 
#    eligibility criteria (and washout period for neuropathy/pvd and ulcers).
# 2. Determine surv_time and surv_status.
# 3. Need to use LOCF for time-varying variables before creating landmark 
#    datasets or will get NAs at landmark ages.
# 4. BUT need to get descriptive stats before point 3.


rm(list = ls())


library(tidyverse)
library(arrow)

source("05_functions.R")


ndr <- read_parquet(
  "../Processed_data/04_set-up-data.parquet", 
  as_data_frame = T
) %>% 
  arrange(LopNr, age) %>% 
  group_by(LopNr, record_date) %>% 
  fill(everything(), .direction = "downup") %>% 
  ungroup() %>%
  distinct()

ndr <- anti_join(ndr, ndr[ndr$death_age < ndr$diag_age, ], by = "LopNr")
length(unique(ndr$LopNr))


# NEUROPATHY - PVD --------------------------------------------------------

ndr_neuro <- ndr %>% 
  anti_join(subset(ndr, neuro_fail <= diag_age + (120 / 365.25)), "LopNr") %>% 
  anti_join(subset(ndr, censor_age <= diag_age + (120 / 365.25)), "LopNr") %>% 
  anti_join(subset(ndr, death_age  <= diag_age + (120 / 365.25)), "LopNr") %>%
  set_survival(dat = ., y = "neuro_fail")

ndr_neuro[ndr_neuro$surv_time - ndr_neuro$diag_age <= 0, ]

length(unique(ndr_neuro$LopNr))

tab1_neuro <- table1a(ndr_neuro)

locf_data(ndr_neuro) %>% 
  write_parquet("../Processed_data/05_neuropathy.parquet")

rm(ndr_neuro)


# FOOT ULCER --------------------------------------------------------------

ndr_ulcer <- ndr %>% 
  anti_join(subset(ndr, ulcer_fail <= diag_age + (90 / 365.25)), "LopNr") %>% 
  anti_join(subset(ndr, censor_age <= diag_age + (90 / 365.25)), "LopNr") %>% 
  anti_join(subset(ndr, death_age  <= diag_age + (90 / 365.25)), "LopNr") %>%
  set_survival(dat = ., y = "ulcer_fail")

ndr_ulcer[ndr_ulcer$surv_time - ndr_ulcer$diag_age <= 0, ]

length(unique(ndr_ulcer$LopNr))

tab1_ulcer <- table1a(ndr_ulcer)

locf_data(ndr_ulcer) %>% 
  write_parquet("../Processed_data/05_ulcer.parquet") 

rm(ndr_ulcer)


# MAJOR AMPUTATION --------------------------------------------------------

ndr_majamp <- ndr %>% 
  anti_join(subset(ndr, maj_amp_fail <= diag_age), "LopNr") %>% 
  anti_join(subset(ndr, censor_age   <= diag_age), "LopNr") %>% 
  anti_join(subset(ndr, death_age    <= diag_age), "LopNr") %>%
  set_survival(dat = ., y = "maj_amp_fail")

ndr_majamp[ndr_majamp$surv_time - ndr_majamp$diag_age <= 0, ]

length(unique(ndr_majamp$LopNr))

tab1_majamp <- table1a(ndr_majamp)

locf_data(ndr_majamp) %>% 
  write_parquet("../Processed_data/05_major-amputation.parquet")

rm(ndr_majamp)


# ANY AMPUTATION ----------------------------------------------------------

ndr_anyamp <- ndr %>% 
  anti_join(subset(ndr, any_amp_fail <= diag_age), "LopNr") %>% 
  anti_join(subset(ndr, censor_age   <= diag_age), "LopNr") %>% 
  anti_join(subset(ndr, death_age    <= diag_age), "LopNr") %>% 
  set_survival(dat = ., y = "any_amp_fail")

ndr_anyamp[ndr_anyamp$surv_time - ndr_anyamp$diag_age <= 0, ]

length(unique(ndr_anyamp$LopNr))

tab1_anyamp <- table1a(ndr_anyamp)

locf_data(ndr_anyamp) %>% 
  write_parquet("../Processed_data/05_all-amputation.parquet")


writexl::write_xlsx(
  bind_rows(tab1_neuro, tab1_ulcer, tab1_majamp, tab1_anyamp),
  "../Output/Table_1a.xlsx"
)


rm(list = ls())