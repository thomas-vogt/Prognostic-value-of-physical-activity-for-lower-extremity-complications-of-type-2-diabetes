
# FIND THE RISK SET AT A CERTAIN LANDMARK TIME ----------------------------

# dat: A data frame in the long format.

# id: The name of the variable that identifies individuals/units in the
# data. It should be provided as a string (character vector of length one).

# LM_times: A vector of landmark times.

# time_var: The name of the variable that indexes longitudinal time in
# the data. It should be provided as a string (character vector of length one).

# event_time: The name of the variable that holds the failure or
# censoring time in the data. It should be provided as a string (character
# vector of length one). Each individual/unit should only have one unique value
# for this variable.
#
# return a list of data frames, one for each landmark time.

find_risk_set <- function(dat, id, LM_times, time_var, event_time) {

  # Returns a list of data frames
  # Returns all rows up to (and including) the landmark time for individuals
  # who are present in the data at the landmark time, and who have not
  # experienced an event at the landmark time.

  if (!inherits(dat, "data.frame"))      stop("dat should be a data frame")

  if (!inherits(id, "character"))        stop("id is not of class chr")

  if (!inherits(time_var,"character"))   stop("time_var is not of class chr")

  if (!is(LM_times,"numeric"))           stop("LM_times is not numeric")
  
  if (!inherits(event_time,"character")) stop("event_time is not of class chr")

  for (col in c(id, time_var, event_time)) {
    if (!col %in% names(dat)) stop(col, " is not a column name in the data")
  }

  out_list <- lapply(
    LM_times,
    function(x) {

      new_data <- dat[dat[[time_var]] <= x, ]
      new_data <- new_data[new_data[[event_time]] > x, ]

      ids_removed <- length(unique(new_data[[id]])) - length(unique(dat[[id]]))
      if (ids_removed >= 1) {
        warning(
          ids_removed, " individuals have been removed as they are not in the risk set at age ", x
        )
      }
      rownames(new_data) <- NULL
      return(new_data)
    }
  )
  names(out_list) <- LM_times
  out_list
}


# FIND THE LAST RECORDED VALUE --------------------------------------------

find_LOCF <- function(dat, id, LM_times, horizons, time_var, event_time, event_status) {

  # Returns the last row recorded before the landmark time for each individual/unit.
  # Applies censoring at horizon time.
  # Returns a list of data frames.

  dat_list <- find_risk_set(
    dat = dat,
    id = id,
    LM_times = LM_times,
    time_var = time_var,
    event_time = event_time
  )

  dat_list <- lapply(
    seq_along(dat_list),
    function(x) {
      horizon  <- horizons[x]
      new_data <- dat_list[[x]]

      # Find last observation before landmark time:
      new_data <- new_data[order(new_data[[time_var]], decreasing = TRUE), ]
      new_data <- new_data[!duplicated(new_data[[id]]), ]

      # Applies censoring at horizon time:
      new_data[new_data[[event_time]] > horizon, event_status] <- 0
      new_data[new_data[[event_time]] > horizon, event_time]   <- horizon

      rownames(new_data) <- NULL
      return(new_data)
    }
  )
  return(dat_list)
}
