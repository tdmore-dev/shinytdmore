#' A facade object to use Shiny Progress with dplyr
#' 
#' @description 
#' Used to translate dplyr progress to a Shiny progress bar
ShinyToDplyrProgressFacade <- R6::R6Class("Progress",
 public = list(
   #' @field proxy Shiny Progress object
   proxy=NULL,
   
   #' @field n total ticks for dplyr progress; can be reinitialized
   n = NULL,
   
   #' @field i current tick for dplyr progress; can be reinitialized
   i = 0,
   
   #' @field offset offset for Shiny Progress
   offset=0,
   
   #' @field amount total advanced amount in Shiny after the dplyr progress is done
   amount=1,
   
   #' @description 
   #' Initialize a shiny progress to dplyr facade. This can either be called with the `proxy` and `amount`
   #' arguments for a first initialization (e.g. when using `new`), or with 
   #' `n` and `min_time` arguments for the dplyr-like initialization
   #' 
   #' @param n Total number of items
   #' @param min_time Progress bar will wait until at least `min_time`
   #'   seconds have elapsed before displaying any results.
   #' @param proxy Shiny Progress object
   #' @param amount amount advanced in Shiny Progress
   #' @param ... ignored
   #' @return an object that can be used in `.progress` arguments for dplyr
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
   #' @description
   #' Advance a tick
   tick = function() {
     self$i = self$i + 1
     self
   },
   #' @description
   #' Stop the timer; advance until the end
   stop = function() {
     self$proxy$set(value=self$offset + self$amount)
     self
   },
   #' @description
   #' Print out the progress; transfers the progress to Shiny
   #' @param ... ignored
   print = function(...) {
     value <- self$offset + self$i / self$n * self$amount
     self$proxy$set(value=value)
     invisible(self) 
   }
 )
)