
# FIND FIRST FAILURE TIME  ------------------------------------------------

failure_date <- function(var_name, new_var) {
  # var_name = name of the existing binary outcome variable.
  # new_var  = name of the new variable that will be created in the data frame.
  x <- ndr[ndr[[var_name]] == 1L, c("LopNr", "age", var_name)] %>% 
    group_by(LopNr) %>% 
    slice_min(age) %>% 
    ungroup() %>% 
    distinct()
  
  x[[var_name]] <- NULL
  x[[new_var]] <- x[["age"]]
  x[["age"]] <- NULL
  
  return(left_join(ndr, x, by = "LopNr"))
}