# NOTES:
# 1. Find the minimum date by moving all dates to a vector per LopNr, 
#    and removing NAs before getting the min. Exclude if min(date) is 
#    earlier than 2007-01-01.
# 2. Amputations due to external causes are not considered an event of interest.


rm(list = ls())


library(tidyverse)
library(arrow)
source("01_functions.R")


load_regex()

data_files <- list.files(
  path = "../Processed_data",
  pattern = "(01|02)_.*.parquet$", 
  full.names = FALSE
)

data_files <- data_files[!grepl("lisa", data_files)] 

names(data_files) <- str_extract(
  data_files, 
  "(ndr|death|drugs|inpatient|outpatient|migration|date-of-birth)"
)


# READ DATA ---------------------------------------------------------------


for (nm in data_files) {
  if (grepl("(inpatient|outpatient)", nm)) {
    dat <- read_parquet(
      paste0("../Processed_data/", nm), 
      col_select = c("LopNr", "record_date", matches("(_code|EKOD)"))
    )
  } else {
    dat <- read_parquet(paste0("../Processed_data/", nm)) 
  }
  if (grepl("date-of-birth", nm)) {
    assign("dob", dat, pos = .GlobalEnv)
  } else {
    assign(names(data_files[data_files == nm]), dat, pos = .GlobalEnv)
  }
}

rm(dat)


# EXCLUDE IF NOT INCIDENT T2D FROM 2007 -----------------------------------


diag_date <- do.call(
  rbind,
  list(
    ndr[, c("LopNr", "record_date")],
    drugs[, c("LopNr", "record_date")],
    inpatient[inpatient$diagnosis_code %in% strings_icd, c("LopNr", "record_date")],
    outpatient[outpatient$diagnosis_code %in% strings_icd, c("LopNr", "record_date")]
  )
)

diag_date <- by(diag_date, diag_date$LopNr, function(x) x[which.min(x$record_date), ])
diag_date <- bind_rows(unclass(diag_date))

ndr <- anti_join(
  ndr, 
  diag_date[diag_date$record_date < lubridate::ymd("2007-01-01"), ],
  by = "LopNr"
)

length(unique(ndr$LopNr))


# FIND DIAGNOSIS DATE -----------------------------------------------------


diag_year <- by(ndr[, c("LopNr", "debutar")], ndr$LopNr, function(x) x[which.min(x$debutar), ])
diag_year <- distinct(bind_rows(unclass(diag_year)))

diag_date <- diag_date %>%
  rename(diag_date = record_date) %>%
  left_join(diag_year, by = "LopNr") %>% 
  semi_join(ndr, by = c("LopNr")) %>% 
  distinct() 

no_diag_date <- filter(
  diag_date,
  is.na(diag_date) | as.integer(format(diag_date, "%Y")) > debutar
)

diag_date <- anti_join(diag_date, no_diag_date, by = "LopNr")

set.seed(6548524)
for (i in seq_along(no_diag_date$LopNr)) {
  no_diag_date$diag_date[i] <- rdate(no_diag_date$debutar[i], 1)
}

diag_date <- rbind(
  diag_date[, c("LopNr", "diag_date")], 
  no_diag_date[, c("LopNr", "diag_date")]
)

ndr <- left_join(ndr, diag_date, by = "LopNr")


# EXCLUDE IF IMMIGRATION < 365 DAY BEFORE diag_date -----------------------


immigration <- left_join(
  distinct(ndr[, c("LopNr", "diag_date")]), 
  migration[migration$PostTyp == "Inv", c("LopNr", "record_date")], 
  by = "LopNr"
) %>% 
  filter(
    !is.na(record_date) 
    & (diag_date - record_date) < 365L
  )

ndr <- anti_join(ndr, immigration, by = "LopNr")

length(unique(ndr$LopNr))


# RECODE VARIABLES --------------------------------------------------------

ndr$neuropathy <- if_else(ndr$fotrisk %in% 2:4, 1L, as.integer(NA))

patient_reg <- semi_join(bind_rows(inpatient, outpatient), ndr, by = "LopNr")

majamp_codes <- strings_surgcodes[grepl("NGQ|NFQ|NEQ", strings_surgcodes)]

patient_reg <- patient_reg %>% 
  mutate(
    ext_cause  = if_else(if_all(starts_with("EKOD"), ~ . == ""), 0L, 1L),
    ext_cause  = if_else(is.na(ext_cause), 0L, ext_cause),
    foot_ulcer = if_else(diagnosis_code %in% "E116D", 1L, as.integer(NA)),
    any_amp    = if_else(surgical_code %in% strings_surgcodes, 1L, as.integer(NA)),
    maj_amp    = if_else(surgical_code %in% majamp_codes, 1L, as.integer(NA)),
    any_amp    = if_else(ext_cause == 1L, as.integer(NA), any_amp),  # DELETE AMPUTATIONS DUE TO EXT CAUSES.
    maj_amp    = if_else(ext_cause == 1L, as.integer(NA), maj_amp)   # DELETE AMPUTATIONS DUE TO EXT CAUSES.
  ) %>% 
  select(-starts_with(c("EKOD", "diagnosis_", "surgical_"))) %>%
  filter(foot_ulcer %in% 1L | any_amp %in% 1L | maj_amp %in% 1L) %>% 
  distinct()

migration <- left_join(
  distinct(ndr[, c("LopNr", "diag_date")]), 
  migration, 
  by = "LopNr"
) %>%
  filter(PostTyp %in% "Utv" & record_date >= diag_date) %>% 
  group_by(LopNr) %>% 
  slice_min(record_date) %>% 
  rename(emig_date = record_date) %>% 
  ungroup() %>% 
  select(-diag_date, -PostTyp)


# JOINS -------------------------------------------------------------------


ndr <- full_join(ndr, patient_reg, by = c("LopNr", "record_date"))
ndr <- left_join(ndr, dob, by = "LopNr")
ndr <- left_join(ndr, death, by = "LopNr")
ndr <- left_join(ndr, migration, by = "LopNr")

ndr <- repop_cells("diag_date")
ndr <- repop_cells("DODSDAT")

ndr <- ndr %>% 
  arrange(LopNr, record_date) %>% 
  group_by(LopNr) %>% 
  fill(neuropathy, foot_ulcer, any_amp, maj_amp, last_meas, .direction = "down") %>% 
  ungroup() %>% 
  mutate(
    neuropathy = if_else(foot_ulcer %in% 1L, 1L, neuropathy),
    neuropathy = if_else(any_amp %in% 1L, 1L, neuropathy),
    neuropathy = if_else(maj_amp %in% 1L, 1L, neuropathy),
    neuropathy = if_else(is.na(neuropathy), 0L, neuropathy), 
    foot_ulcer = if_else(is.na(foot_ulcer), 0L, foot_ulcer), 
    any_amp    = if_else(is.na(any_amp), 0L, any_amp), 
    maj_amp    = if_else(is.na(maj_amp), 0L, maj_amp),
    ext_cause  = if_else(is.na(ext_cause), 0L, ext_cause)
  )

write_parquet(
  ndr, 
  "../Processed_data/03_joined-datasets.parquet"
)


rm(list = ls())
