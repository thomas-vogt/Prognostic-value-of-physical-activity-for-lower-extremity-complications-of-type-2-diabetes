# NOTES:
# 1. Extracts rows when there is a match with someone from the NDR, and when
#    the ATC code starts with "A10".


rm(list = ls())


source("01_functions.R")


load_packages()
ndr_all <- load_ndr_lopnrs()
path <- "/castor/project/proj/Data_directory/Drugs_register/Original_data/"


drugs_0513 <- read_sas(
  paste0(path, "t_r_lmed_i_0513_15352_2020.sas7bdat"),
  col_select = c("LopNr", "ATC", "FDATUM")
) 

drugs_1421 <- read_sas(
  paste0(path, "t_r_lmed_i_1421_15352_2020.sas7bdat"),
  col_select = c("LopNr", "ATC", "FDATUM")
) 

drugs <- rbind(drugs_0513, drugs_1421)

rm(drugs_0513, drugs_1421)

drugs <- semi_join(drugs, ndr_all, by = "LopNr") %>% 
  filter(str_detect(ATC, "^A10")) %>% 
  mutate(LopNr = as.integer(LopNr)) %>% 
  rename(
    record_date = FDATUM, 
    atc_code = ATC
  )

head(drugs, n = 20)

length(unique(drugs$LopNr))


write_parquet(
  drugs, 
  "../Processed_data/01_extracted-drugs-data.parquet"
)

rm(list = ls())