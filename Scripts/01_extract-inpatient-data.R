# NOTES:
# 1. Extracts inpatient data when there is a match in the NDR.


rm(list = ls())


source("01_functions.R")


load_packages()
load_regex()
ndr_all <- load_ndr_lopnrs()
path <- "/castor/project/proj/Data_directory/Inpatient_register/Original_data/"


inpatient <- extract_PR_data(
  filename = paste0(path, "t_t_t_r_par_sv_i_15352_2020.sas7bdat"), 
  date_variable = "UTDATUM"
) %>%
  rename(record_date = UTDATUM)

rm(ndr_all)

head(inpatient, n = 20)

length(unique(inpatient$LopNr))


write_parquet(
  inpatient, 
  "../Processed_data/01_extracted-inpatient-data.parquet"
)

rm(list = ls())