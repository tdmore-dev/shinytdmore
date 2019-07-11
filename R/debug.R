#' Debug react object.
#'
#' @param react reactive object
#'
debugReact <- function(react) {
  print(react()$doses)
  print(react()$obs)
  print(react()$now)
  print(react()$covs)
  print(react()$target)
}