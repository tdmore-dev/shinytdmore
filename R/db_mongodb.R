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

#' @export
#' @importFrom mongolite mongo
#' @importFrom R6 R6Class
MongoDatabase <- R6::R6Class("MongoDatabase", inherit=JsonDatabase,
  private=list(
    db=NULL,
    doGet=function(id) {
      ret <- private$db$iterate(
        query=sprintf('{"_id":{"$oid":"%s"}}', id),
        fields='{}'
      )
      ret$json()
    },
    doUpdate=function(id,patient) {
      ret <- private$db$replace(
        query=sprintf('{"_id":{"$oid":"%s"}}', id),
        update=patient,
        upsert=FALSE
      )
      if(ret$matchedCount==0) stop("Patient id ",id," not found!")
      invisible(self)
    },
    doRemove=function(id) {
      ret <- private$db$remove(
        query=sprintf('{"_id":{"$oid":"%s"}}', id), just_one=TRUE
      )
      invisible(self)
    },
    doAdd=function(patient) {
      # We do not use `insert`, because we need to know the insertedId!
      # See https://github.com/jeroen/mongolite/issues/175
      # and https://docs.mongodb.com/manual/reference/method/db.collection.insertOne/
      ret <- private$db$update(
        '{"_id": {"$lt":-1}}', 
        paste0('{"$set": ',patient,'}'), 
        upsert=TRUE, 
        multiple=FALSE)
      ret$upsertedId
    },
    doGetPatients=function() {
      it <- private$db$iterate(
        # no need to exclude fields; include everything!
        #fields = '{"id":true, "firstname":true, "lastname":true, "model":true, "created_at":true, "modified_at":true}', 
        fields='{}', #include everything, including $id !
        sort = '{"created_at": -1}'
      )
      it$json()
    },
    fromJson=function(json) {
      x <- super$fromJson(json)
      
      # search ID
      raw <- rjson::fromJSON(json)
      id <- raw$`_id`
      if(is.list(id)) id <- id$`$oid`
      
      x$id <- id
      x
    },
    toJson=function(x) {
      ## Swap `id` to `_id`
      ## TODO: should not be necessary...
      json <- super$toJson(x)
      x2 <- rjson::fromJSON(json)
      x2[["_id"]] <- NULL
      rjson::toJSON(x2)
    }
  ),
  public=list(
    initialize=function(collection, db, url="mongodb://localhost") {
      private$db <- mongolite::mongo(collection = collection,
        db=db,
        url=url)
      invisible(self)
    }
  )
)

MongoDatabase$composeUrl <- function(host="localhost", user=NULL, password=NULL) {
  url <- "mongodb://"
  if(!is.null(user)) {
    if(is.null(password)) stop("User specified, but password was not...")
    url <- paste0(url, utils::URLencode(user), ":", utils::URLencode(password), "@")
  }
  url <- paste0(url, host)
  url
}