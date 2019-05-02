
#'
#' Convert date and time vectors to POSIX.
#' 
#' @param date date vector
#' @param time character vector with '\%H:\%M'-formatted times
#'
dateAndTimeToPOSIX <- function(date, time) {
  return(as.POSIXct(strptime(paste(date, time), format = "%Y-%m-%d %H:%M")))
}

#'
#' Generate the list of hours that can be picked in the 'hours' combobox.
#' 
#' @return a list of all the hours
#'
hoursList <- function() {
  grid <- expand.grid(pad(c(0,30)), pad(0:23))
  hours <- paste0(grid$Var2, ":", grid$Var1)
  return(hours)
}

#'
#' Pad integer with a zero if needed.
#' 
#' @param integer the integer to be padded
#' @return a string
#'
pad <- function(integer) {
  ifelse(integer < 10, paste0('0', integer), paste0(integer))
}

#' Convert POSIX date to hours (numeric).
#'
#' @param posixDate a date
#' @return the converted dates, in hours
#' @importFrom lubridate ymd_hms
#'
posixToHours <- function(posixDate) {
  return(as.integer(lubridate::ymd_hms(posixDate)) / (3600))
}