#' A facade object 
ShinyToDplyrProgressFacade <- R6::R6Class("Progress",
 public = list(
   proxy=NULL,
   n = NULL,
   i = 0,
   offset=0, #starting value for the proxy backend
   amount=1, #total amount progression for the proxy backend
   
   initialize = function(n=1, min_time = 0, proxy=NULL, amount=1, ...) {
     if(!is.null(proxy)) {
       self$proxy <- proxy
       self$offset <- proxy$getValue()
       self$amount <- amount
       if(self$offset + self$amount > proxy$getMax()) stop("Current proxy value + offset exceeds proxy max")
     }
     self$n <- n
     self$i <- 0
   },
   tick = function() {
     self$i = self$i + 1
     self
   },
   stop = function() {
     self$proxy$set(value=self$offset + self$amount)
     self
   },
   print = function(...) {
     value <- self$offset + self$i / self$n * self$amount
     self$proxy$set(value=value)
     invisible(self) 
   }
 )
)