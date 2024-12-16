# NOTES:
# 1. This script only extracts data from 2005 onwards (study starts in 2007).


rm(list = ls())


source("01_functions.R")


load_packages()
ndr_all <- load_ndr_lopnrs()


lisa_files <- list.files(
  path = "/castor/project/proj/Data_directory/LISA/Original_data",
  pattern = "brook_lev_lisa_2", 
  full.names = TRUE
)

lisa_files <- lisa_files[!grepl("200[01234]", lisa_files)] 

names(lisa_files) <- str_extract(lisa_files, "lisa_[[:digit:]]{4}")

var_names <- c(
  "LopNr",
  "Kommun",
  "Civil",
  "SenInvAr",
  "FodelseLan",
  "Sun2000niva",
  "ALosDag",
  "DispInkKE",
  "DispInkKE04",
  "DispInk04",
  "DispInkFam04"
)


out <- vector("list", length = length(lisa_files))
names(out) <- lisa_files

for (nm in lisa_files) {

  dat_year <- str_extract(names(lisa_files[lisa_files == nm]), "[[:digit:]]{4}")

  dat <- read_sas(nm, col_select = all_of(var_names)) %>%
    mutate(LopNr = as.integer(LopNr)) %>% 
    semi_join(ndr_all, by = "LopNr") %>% 
    mutate(
      lisa_year = as.integer(dat_year),
      ALosDag   = as.integer(ALosDag),
      SenInvAr  = as.integer(SenInvAr)
    ) %>% 
    distinct()

  out[[nm]] <- dat

}

lisa <- do.call(rbind, out)

for (nm in names(lisa)) {
  if (is.character(lisa[[nm]])) lisa[nm][lisa[nm] == ""] <- NA
}


write_parquet(
  lisa, 
  "../Processed_data/01_extracted-lisa-data.parquet",
)

rm(list = ls())