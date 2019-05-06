#'
#' Auto-sort table by date.
#'
#' @param data a data frame that has a date and time column
#' @return the same dataframe, ordered by date
#'
autoSortByDate <- function(data) {
  if (nrow(data)==0) {
    return(data)
  }
  dates <- dateAndTimeToPOSIX(data$date, data$time)
  return(data[order(dates),])
}

#'
#' Utility function to know where the horizontal line corresponding to the now date should be.
#'
#' @param data a data frame that has a date and time column
#' @param now now date
#' @param dose logical value, true if doses table, false if measures table
#' @return index of this horizontal line or integer(0) if not pertinent
#'
getTableBorderIndex <- function(data, now, dose) {
  length <- nrow(data)
  if (length==0) {
    return(integer())
  }
  dates <- dateAndTimeToPOSIX(data$date, data$time)
  
  if (dose) {
    index <- which(dates >= now)
  } else {
    index <- which(dates > now)
  }
  
  if (length(index)!=0) {
    if (length(index) > 1) {
      index <- index[1]
    }
    if (index==1) {
      return(integer())
    }
    else {
      return(index)
    }
    return(index)
  } else {
    return(integer())
  }
}