rm(list = ls())

library(tidyverse)

table1c <- readxl::read_excel("Table_1.3.xlsx")

table1c <- unique(table1c[, c(1:3, 6)])

sapply(c(0.25, 0.5, 0.75), function(x) quantile(table1c$`Missing (%)`, x))

(sum(table1c$`Missing (%)` < 20) / nrow(table1c)) * 100

rm(list = ls())
