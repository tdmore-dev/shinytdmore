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

not_implemented <- function() {
  stop("Not implemented yet. This function needs to be implemented in subclasses.")
}

#' Database class
#' @title Database Class
#' @docType class
#' @description Database class description
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

#' JSON database class
#' @title JsonDatabase Class
#' @docType class
#' @description JsonDatabase class description
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

#' In-memory database class
#' @title InMemoryDatabase Class
#' @docType class
#' @description InMemoryDatabase class description
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
      id <- length(private$storage) + 1
      patient$id <- id
      private$storage <- c(private$storage, list(patient))
      patient
    }
  ),
  active=list(
    patients=function(value) {
      if(missing(value)) private$storage
      else private$storage <- value
    }
  ))

#' File database class
#' @title FileDatabase Class
#' @docType class
#' @description FileDatabase class description
#' @export
FileDatabase <- R6::R6Class("FileDatabase", inherit=JsonDatabase,
  private=list(folder=list(),
  doGet=function(id) {
    fileName <- file.path(private$folder, sprintf('%s.json', id))
    if(!file.exists(fileName)) stop("Invalid id `", id, "'; file `", fileName, "' does not exist")
    readChar(fileName, file.info(fileName)$size)
  },
  doUpdate=function(id,patient) {
    fileName <- file.path(private$folder, sprintf('%s.json', id))
    if(!file.exists(fileName)) stop("Invalid id `", id, "'; file `", fileName, "' does not exist")
    writeChar(patient, fileName)
  },
  doRemove=function(id) {
    fileName <- file.path(private$folder, sprintf('%s.json', id))
    if(!file.exists(fileName)) stop("Invalid id `", id, "'; file `", fileName, "' does not exist")
    unlink(fileName)
  },
  doAdd=function(patient) {
    id <- floor(runif(n=1, max = .Machine$integer.max))
    fileName <- file.path(private$folder, sprintf('%s.json', id))
    if(file.exists(fileName)) return(doAdd(patient)) #try again
    file.create(fileName)
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