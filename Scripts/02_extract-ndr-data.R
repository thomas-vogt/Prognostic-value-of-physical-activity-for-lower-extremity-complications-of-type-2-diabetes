# NOTES:
# 1. Apply eligibility criteria to NDR data based on NDR variables.
# 2. The reason for the line of code below is that I am assuming that the last
#    recorded diabetes type is the one most likely to be correct:
#    filter(!(klin_diab_typ[which.max(regdat)] %in% c(1, 3, 5)))


rm(list = ls())


source("01_functions.R")

load_packages()
library(lubridate)
load_regex()

path <- "/castor/project/proj/Data_directory/National_diabetes_register/"

var_names <- c(
  "LopNr",
  "regdat",
  "sex",
  "bmi",
  "hba1c",
  "systoliskt",
  "kolesterol",
  "hdl",
  "ldl",
  "ischemisk_hjsjukdom",
  "stroke",
  "retinopati",
  "retinopathyDiagnosis",
  "rokare",
  "rokvanor",
  "albuminuria",
  "diab_beh",
  "ins_metod",
  "debutar", 
  "klin_diab_typ",
  "fysisk_aktivitet",
  "fotrisk"
)

to_dbl <- c("bmi", "hdl", "kolesterol")
to_int <- var_names[!var_names %in% c(to_dbl, "regdat")]


# READ DATA


ndr <- read_sas(
 paste0(path, "Original_data/sos_rs_2023_00886.sas7bdat"),
 col_select = all_of(var_names)
)

length(unique(ndr$LopNr))

ndr[, to_int] <- lapply(ndr[, to_int], as.integer)
ndr[, to_dbl] <- lapply(ndr[, to_dbl], as.double)
ndr$regdat    <- as.Date(ndr$regdat)


# APPLY EXCLUSION CRITERIA BASED ON NDR DATA


ndr <- ndr %>% 
  arrange(LopNr, regdat) %>% 
  group_by(LopNr) %>% 
  mutate(klin_diab_typ = DescTools::LOCF(klin_diab_typ)) %>%
  ungroup()

diagnosedBefore2007 <- ndr %>%
  filter(regdat < as.Date("2007-01-01") | debutar < 2007) %>%
  select(LopNr)

ndr <- ndr %>%
  distinct() %>%
  group_by(LopNr) %>%
  filter(
    !all(klin_diab_typ %in% c(1, 3, 5))
    & !(klin_diab_typ[which.max(regdat)] %in% c(1, 3, 5))
  ) %>%
  ungroup() %>%
  anti_join(diagnosedBefore2007, by = "LopNr") %>%
  filter(regdat < as.Date("2020-01-01")) %>%
  rename(record_date = regdat) %>% 
  mutate(last_meas = record_date)


length(unique(ndr$LopNr))


write_parquet(
  ndr, 
  "../Processed_data/02_extracted-ndr-data.parquet"
)


rm(list = ls())
