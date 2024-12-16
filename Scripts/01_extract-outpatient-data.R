# NOTES:
# 1. Extracts outpatient data when there is a match in the NDR.


rm(list = ls())


source("01_functions.R")


load_packages()
load_regex()
ndr_all <- load_ndr_lopnrs()
path <- "/castor/project/proj/Data_directory/Outpatient_register/Original_data/"


outpatient <- extract_PR_data(
  filename = paste0(path, "t_t_t_r_par_ov_i_15352_2020.sas7bdat"), 
  date_variable = "INDATUM"
) %>%
  rename(record_date = INDATUM)

rm(ndr_all)

head(outpatient, n = 20)

length(unique(outpatient$LopNr))


write_parquet(
  outpatient, 
  "../Processed_data/01_extracted-outpatient-data.parquet"
)

rm(list = ls())