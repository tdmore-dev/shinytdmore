##
## Script name: db.R
##
## Purpose of script:
## Define an interface for interaction with a database
##
## Author: Ruben Faelens
##
## Date Created: Thu Jul 11 12:08:26 2019
##
## Copyright (c) Ruben Faelens, 2019
## Email: ruben.faelens@gmail.com
##
## ---------------------------
##
## Notes:
## Can be implemented by different subclasses
##
## ---------------------------



#' Database connection
#'
#' This R6 class provides an interface for `shinytdmore` to connect
#' to a database of patient information. This could be a simple
#' in-memory database, a file-based storage, or even an external database.
#' 
#' @section Implementing a database:
#' ```
#' InMemoryDatabase <- R6::R6Class("InMemoryDatabase", inherit=Database,
#'  private=list(storage=list()),
#'  public=list(
#'    get=function(id) storage[[id]],
#'    update=function(id,patient) {
#'      storage[[id]] <- patient
#'      invisible(self)
#'    },
#'    remove=function(id) {
#'      storage[[id]] <- NULL
#'      invisible(self)
#'    },
#'    add=function(patient) {
#'      private$storage <- c(private$storage, list(patient))
#'      patient$id <- length(storage)
#'    }
#'  ),
#'  active=list(
#'    patients=function(value) {
#'      if(missing(value)) private$storage
#'      else private$storage <- value
#'    }
#'  ))
#' ```
#'
#' @section Methods:
#' `$new()` creates a new database connection, connecting to the 
#' database and performing required sanity checks.
#'
#' `$get(id)` retrieves a patient with the given `id` from the database
#' 
#' `$update(id, patient)` updates an existing patient with new information
#' 
#' `$remove(id)` removes a patient from the database
#' 
#' `$add(patient)` adds a patient to the database, and returns the patient with 
#' the `id` field filled in
#' 
#' `$patients` retrieves a list of all patients in the database
#'
#' @section JSON Database:
#' For simplicity, one could also inherit the `JsonDatabase` R6::R6Class instead. 
#' This class takes care of marshalling from/to JSON, making an implementation easier.
#' ```
#' FileDatabase <- R6::R6Class("FileDatabase", inherit=JsonDatabase,
#'    private=list(folder=list(),
#'    doGet=function(id) {
#'      fileName <- file.path(folder, sprintf('%s.json', id))
#'      readChar(fileName, file.info(fileName)$size)
#'    },
#'    doUpdate=function(id,patient) {
#'      fileName <- file.path(folder, sprintf('%s.json', id))
#'      writeChar(fileName, patient)
#'    },
#'    doRemove=function(id) {
#'      fileName <- file.path(folder, sprintf('%s.json', id))
#'      unlink(fileName)
#'    },
#'    doAdd=function(patient) {
#'      id <- floor(runif(n=1, max = .Machine$integer.max))
#'      fileName <- file.path(folder, sprintf('%s.json', id))
#'      if(file.exists(fileName)) return(doAdd(patient)) #try again
#'      doUpdate(id, patient)
#'      id
#'    },
#'    doGetPatients=function() {
#'      map( dir(folder), ~ readChar(.x, file.info(.x)$size) )
#'    }
#'  ),
#'  public=list(
#'    initialize=function(folder) {
#'      private$folder <- folder
#'      invisible(self)
#'    }
#'  )
#' )
#' ```
#'
#' @name Database
#' @examples
#' db <- InMemoryDatabase$new()
#' patient <- db$add(createFakePatient())
#' id <- patient$id
#'
#' @import R6 
#' @importFrom R6 R6Class
#'
NULL

not_implemented <- function() {
  stop("Not implemented yet. This function needs to be implemented in subclasses.")
}

#' @export
Database <- R6::R6Class("Database", lock_objects=TRUE, lock_class=TRUE,
                    public=list(
                      get=function(id) not_implemented(),
                      update=function(id,patient) not_implemented(),
                      remove=function(id) not_implemented(),
                      add=function(patient) not_implemented()
                    ),
                    active=list(
                      patients=function(value) not_implemented()
                      ))

#' @export
JsonDatabase <- R6::R6Class("JsonDatabase", 
                        inherit=Database,
                        lock_objects=TRUE, lock_class=TRUE,
                    public=list(
                      get=function(id) {
                        json <- private$doGet(id)
                        patient <- private$fromJson(json)
                        patient$id <- id
                        patient
                        },
                      update=function(id,patient) {
                        patient$modified_at <- Sys.time()
                        private$doUpdate(id, private$toJson(patient))
                        invisible(self)
                        },
                      remove=function(id) {
                        private$doRemove(id)
                        invisible(self)
                        },
                      add=function(patient) {
                        id <- private$doAdd(private$toJson(patient))
                        patient$id <- id
                        patient
                      }
                    ),
                    active=list(
                      patients=function(value) {
                        jsonList <- private$doGetPatients()
                        purrr::map(jsonList, private$fromJson)
                      }
                    ),
                    private=list(
                      fromJson=function(json) jsonToPatientModel(json),
                      toJson=function(x) patientModelToJson(x),
                      doGet=function(id) not_implemented(),
                      doUpdate=function(id,patient) not_implemented(),
                      doRemove=function(id) not_implemented(),
                      doAdd=function(patient) not_implemented(),
                      doGetPatients=function() not_implemented()
                      ))

#' @export
InMemoryDatabase <- R6::R6Class("InMemoryDatabase", inherit=Database,
  private=list(storage=list()),
  public=list(
    get=function(id) private$storage[[id]],
    update=function(id,patient) {
      patient$id <- id
      private$storage[[id]] <- patient
      invisible(self)
    },
    remove=function(id) {
      private$storage[[id]] <- NULL
      invisible(self)
    },
    add=function(patient) {
      patient$id <- length(private$storage) + 1
      private$storage <- c(private$storage, list(patient))
    }
  ),
  active=list(
    patients=function(value) {
      if(missing(value)) private$storage
      else private$storage <- value
    }
  ))

#' @export
FileDatabase <- R6::R6Class("FileDatabase", inherit=JsonDatabase,
  private=list(folder=list(),
  doGet=function(id) {
    fileName <- file.path(private$folder, sprintf('%s.json', id))
    readChar(fileName, file.info(fileName)$size)
  },
  doUpdate=function(id,patient) {
    fileName <- file.path(private$folder, sprintf('%s.json', id))
    writeChar(patient, fileName)
  },
  doRemove=function(id) {
    fileName <- file.path(private$folder, sprintf('%s.json', id))
    unlink(fileName)
  },
  doAdd=function(patient) {
    id <- floor(runif(n=1, max = .Machine$integer.max))
    fileName <- file.path(private$folder, sprintf('%s.json', id))
    if(file.exists(fileName)) return(doAdd(patient)) #try again
    private$doUpdate(id, patient)
    id
  },
  doGetPatients=function() {
    read <- function(filename) {
      ## Add the `id`
      x <- readChar(filename, file.info(filename)$size) %>% rjson::fromJSON()
      id <- basename(filename) %>% gsub("\\.json$", "", .)
      x$id <- id
      rjson::toJSON(x)
    }
    purrr::map( dir(private$folder, full.names=T), read)
  }
),
public=list(
  initialize=function(folder) {
    private$folder <- folder
    invisible(self)
  }
)
)