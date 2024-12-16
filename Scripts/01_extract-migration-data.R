# NOTES:
# 1. Extracts imm/emigration date for everyone in the NDR from 2006-01-01.
# 2. The max date available is 2019-12-31.


rm(list = ls())


source("01_functions.R")


load_packages()
ndr_all <- load_ndr_lopnrs()
path <- "/castor/project/proj/Data_directory/LISA/Original_data/"


migration <- read_sas(
  paste0(path, "brook_lev_migrationer.sas7bdat"),
  col_select = c("LopNr", "Datum", "PostTyp")
) %>%
  mutate(LopNr = as.integer(LopNr)) %>%
  semi_join(ndr_all, by = "LopNr") %>%
  filter(substr(as.integer(Datum), 1, 4) > 2005L) %>%
  mutate(
    record_date = lubridate::ymd(Datum),
    PostTyp = as.factor(PostTyp)
  ) %>% 
  select(-Datum)


write_parquet(
  migration, 
  "../Processed_data/01_extracted-migration-data.parquet"
)

rm(list = ls())