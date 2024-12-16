# NOTES:
# 1. Extracts date of death when there is a match with the NDR.
# 2. I replaced dates ending with "0000" with "0702" (exact middle of a year).
# 3. I replaced dates ending with "00" with "16" (middle of a month).


rm(list = ls())


source("01_functions.R")


load_packages()
ndr_all <- load_ndr_lopnrs()
path    <- "/castor/project/proj/Data_directory/Cause_of_death_register/"


death <- read_sas(
  paste0(path, "Original_data/r_dors_i_15352_2020.sas7bdat"),
  col_select = c("LopNr", "DODSDAT")
) %>%
  mutate(LopNr = as.integer(LopNr)) %>% 
  semi_join(ndr_all, by = "LopNr") %>%
  filter(substr(as.integer(DODSDAT), 1, 4) > 2006L) %>%
  mutate(DODSDAT = if_else(
    str_detect(DODSDAT, "0000$"), 
    str_replace(DODSDAT, "0000$", "0702"),
    if_else(
      str_detect(DODSDAT, "00$"),
      str_replace(DODSDAT, "00$", "16"), 
      DODSDAT))
  ) %>%
  mutate(DODSDAT = lubridate::ymd(DODSDAT))


write_parquet(
  death, 
  "../Processed_data/01_extracted-death-data.parquet"
)

rm(list = ls())
