.shinytdmoreGlobals <- new.env(parent=environment())

#' Add key/value pair to configuration.
#'
#' @param key a key
#' @param value any object
#' 
#' @export
#' 
toConfig <- function(key, value) {
  tryCatch(
    {
      assign(key, value, pos=.shinytdmoreGlobals, inherits=FALSE)
    }, error = function(e) {
      if (exists(key, where=.shinytdmoreGlobals, inherits=FALSE))
        remove(key, envir=.shinytdmoreGlobals)
      e
    })
}

#' Get the value corresponding to the given key from the configuration.
#'
#' @param key a key
#' @return the value corresponding to the key
#' @export
#' 
fromConfig <- function(key) {
  if (exists(key, where=.shinytdmoreGlobals, inherits=FALSE))
    .shinytdmoreGlobals[[key]]
  else
    NULL
}

#' Create a DB configuration.
#'
#' @param collection a mongo collection, not null
#' @param db a mongo collection, not null
#' @param user a mongo collection
#' @param password a mongo collection
#' @param server a mongo collection
#' @return a shinytdmore_db_config object
#' @export
#' 
createDBConfig <- function(collection, db, user=NULL, password=NULL, server=NULL) {
  config <- structure(list(
    collection=collection,
    db=db,
    user=user,
    password=password,
    server=server
  ), class="shinytdmore_db_config")
  return(config)
}

#' Get the default DB configuration.
#'
#' @return a shinytdmore_db_config object
#' @export
#' 
defaultDBConfig <- function() {
  config <- createDBConfig(collection="patients", db="shinytdmore")
}

#' Get the DB configuration for tests.
#'
#' @return a shinytdmore_db_config object
#' @export
#' 
testDBConfig <- function() {
  config <- createDBConfig(collection="patients_test", db="shinytdmore")
}

