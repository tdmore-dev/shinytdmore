#'
#' Auto-sort table by date and convert to tibble.
#'
#' @param data a data frame that has a date and time column
#' @return a tibble, ordered by date
#'
autoSortByDate <- function(data) {
  if (nrow(data)==0) {
    return(data)
  }
  tibble::as_tibble(data)
  dates <- dateAndTimeToPOSIX(data$date, data$time)
  return(as.tibble(data[order(dates),]))
}