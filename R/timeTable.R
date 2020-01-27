#' Conversion function to convert an rhandsontable created with \link{timeTable} back
#' to a data.frame with a single `time` column.
#' @param x rhandsontable output object
#' @return data-frame with a single `time` column
#' @export
hot_to_r_datetime <- function(x){
  df <- rhandsontable::hot_to_r(x)
  if(nrow(df) > 0) {
    time <- as.POSIXct(df$date, format="%Y-%m-%d")
    i <- !is.na(df$time)
    time[i] <- as.POSIXct(paste(df$date, df$time), format="%Y-%m-%d %H:%M")[i]
    df$time <- time
  }
  value <- df %>% select(-date)
  value
}

#' @export
singleReactive <- function(state, key, default=NULL, from=identity, to=identity) {
  function(x) {
    if(missing(x)) {
      value <- state[[key]]
      if(is.null(value)) {
        if(is.reactive(default)) default() else default
      } else {
        from(value)
      }
    } else {
      state[[key]] <- to(x)
    }
  }
}

#' Create an rhandsontable based on the given data-frame
#' 
#' @param df data-frame with at least a 'time' column as the first column
#' @param borders a list() with custom borders. See getTableBorder() for more information.
#' @param colHeaders headers for the data-frame *excluding* the time column.
#' @return an rhandsontable with columns Date, Time and any extra columns specified in colHeaders
#' @seealso hot_to_r_datetime
#' @export
timeTable <- function(df, borders=list(), colHeaders=c()) {
  stopifnot( names(df)[1] == "time" )
  time <- as.POSIXct(df$time)
  
  #split time into two columns
  date <- format(time, format="%Y-%m-%d")
  time <- format(time, format="%H:%M")
  time[is.na(time)] <- "00:00"
  df <- tibble::add_column(.data=df, 
                           date=date,
                           .before="time")
  df$time <- time
  
  stopifnot(ncol(df) == (length(colHeaders)+2)) #number of columns should match colHeaders
  
  z <- rhandsontable::rhandsontable(
    df,
    useTypes = TRUE,
    stretchH = "all",
    rowHeaders = NULL,
    colHeaders = c("Date", "Time", colHeaders),
    preventOverflow='horizontal', #see https://handsontable.com/blog/articles/2016/3/a-complete-guide-to-changing-size-of-handsontable
    selectCallback=TRUE #force callback on every select, see https://github.com/jrowen/rhandsontable/issues/325
  )
  
  grid <- expand.grid(pad(0:59), pad(0:23) )
  source <- paste0(grid$Var2, ":", grid$Var1)
  z <- rhandsontable::hot_col(z, col="Time", source=source, allowInvalid=FALSE, strict = TRUE, autocomplete=TRUE)
  
  ## rhandsontable formats all dates as "MM/DD/YYYY" by default...
  ## with no way to change this hardcoded behavior
  ## 
  ## we fix it by pre-formatting as char
  z <- rhandsontable::hot_col(hot=z, col="Date", 
                              strict = TRUE, type="date", 
                              allowInvalid = FALSE, dateFormat = "YYYY-MM-DD")
  z <- rhandsontable::hot_table(z, customBorders = borders)
  ## make very limited contextMenu
  z$x$contextMenu <- list(items = list(
    row_above = list(), row_below = list(), remove_row = list()
  ))
  #z <- rhandsontable::hot_context_menu(z, allowRowEdit=TRUE, allowColEdit=FALSE, allowComments=FALSE, allowCustomBorders=FALSE, allowReadOnly=FALSE)
  
  z
}


#'
#' Utility function to know where the horizontal line corresponding to the now date should be
#'
#' @param df data.frame with at least column 'time'
#' @param now now date
#' @return empty list if no border required, or a list() according to https://handsontable.com/docs/7.3.0/Options.html#customBorders
#' 
#' Please note that handsontable.js has a bug in the handling of customBorders
#' See https://github.com/jrowen/rhandsontable/issues/336
#'
getTableBorder <- function(df, now) {
  BORDER_WIDTH <- 2
  #BORDER_COLOR <- sample(x=c("red","blue","green","orange","purple","cyan","yellow","magenta"), 1)
  BORDER_COLOR <- nowColorHex()
  
  times <- df$time
  
  if(is.null(now)) return(list())
  if(is.null(times)) return(list())
  if (length(times)==0) return(list())
  
  index <- match(TRUE, times >= now)
  if (is.na(index)) { #no times later than now; everything is earlier
    border <- list(
      row=length(times) - 1, #JS index
      bottom=list(width=BORDER_WIDTH, color=BORDER_COLOR) #NOW line at end of table
    )
  } else {
    # index is the first time later than now
    border <- list(
      row=index - 1, #JS index
      top=list(width=BORDER_WIDTH, color=BORDER_COLOR) #NOW line just before that row
    )
  }
  
  #provide a custom border for every cell on that row
  lapply(seq_len(ncol(df)), function(i) {
    border$col <- i-1 #JS index
    border
  })
}