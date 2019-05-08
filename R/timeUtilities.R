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

#' Get the DB datetime format for this project. 
#'
#' @return the datetime format
#'
getDBDatetimeFormat <- function() {
  return("%F %T %z")
}

#' Get the datetime format for this project. 
#'
#' @return the datetime format
#'
getDatetimeFormat <- function() {
  return("%Y-%m-%d %H:%M %Z")
}

#' Get the date format for this project. 
#'
#' @return the date format
#'
getDateFormat <- function() {
  return("%Y/%m/%d")
}

#' Get the time format for this project. 
#'
#' @return the time format
#'
getTimeFormat <- function() {
  return("%H:%M")
}

#'
#' Convert date and time vectors to POSIX.
#' 
#' @param date date vector
#' @param time character vector with '\%H:\%M'-formatted times
#' @return a POSIXct datetime, timezone included (e.g. CEST)
#' @export
#'
dateAndTimeToPOSIX <- function(date, time) {
  return(as.POSIXct(strftime(paste(date, time), format=getDatetimeFormat(), tz=getAppTimeZone(), usetz=T)))
}

#' Convert POSIX date to hours (numeric).
#'
#' @param posixDate a posix date
#' @return the converted dates, in hours
#' @export
#'
POSIXToHours <- function(posixDate) {
  return(as.integer(as.POSIXct(posixDate)) / (3600))
}

#' Format a POSIX datetime to a well-formatted string. 
#'
#' @param posixDatetime a POSIX datetime
#' @return a well formated string corresponding to the date, time zone included, e.g. '2018-12-08 09:00:00 +0100'
#' @export
#'
POSIXToString <- function(posixDatetime) {
  str <- format(posixDatetime, format=getDBDatetimeFormat(), tz=getAppTimeZone())
  return(str)
}

#' Format a POSIX datetime to a well-formatted pretty string. 
#'
#' @param posixDatetime a POSIX datetime
#' @return a well formated string corresponding to the date, time zone included, e.g. '2018-12-08 09:00:00 CEST'
#' @export
#'
POSIXToPrettyString <- function(posixDatetime) {
  str <- format(posixDatetime, format=getDatetimeFormat(), tz=getAppTimeZone())
  return(str)
}

#' Convert a string date to a POSIX datetime. 
#'
#' @param stringDatetime a string datetime that includes the zone, formatted this way: '2018-12-08 09:00:00 +0100'
#' @return a POSIX datetime
#' @export
#'
stringToPOSIX <- function(stringDatetime) {
  return(as.POSIXct(stringDatetime, format=getDBDatetimeFormat(), tz=getAppTimeZone()))
}

#'
#' Format a POSIX datetime to a time string.
#' Note that the returned string will depend on the application timezone.
#' 
#' @param posixDatetime POSIX datetime
#' @return a time string formatted according to getTimeFormat()
#' @export
#'
POSIXToTime <- function(posixDatetime) {
  return(format(posixDatetime, format=getTimeFormat(), tz=getAppTimeZone()))
}

#'
#' Format a POSIX datetime to a 'Date' object.
#' Note that the returned string will depend on the application timezone.
#' 
#' @param posixDatetime POSIX datetime
#' @return a 'Date' object
#' @export
#'
POSIXToDate <- function(posixDatetime) {
  return(as.Date(posixDatetime, tz=getAppTimeZone()))
}

#'
#' Get the application timezone.
#' 
#' @return by default, it returns the system timezone
#' @export
#'
getAppTimeZone <- function() {
  return(Sys.timezone())
}

