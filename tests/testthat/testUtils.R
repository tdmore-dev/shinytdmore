# Useful commands:

# Set working directory to testthat folder (useful to run the tests manually):
# setwd("C:/prj_dev/shinytdmore/tests/testthat")

# Set working directory to shinytdmore package (needed to regenerate namespace with roxygen)
# setwd("C:/prj_dev/shinytdmore")


#'
#' Find a patient from the file database.
#'
#' @param db a file database
#' @param patientId file identifier
#' @return the shiny patient
#'
findPatient <- function(db, patientId) {
  index <- which(sapply(db$patients, function(patient) patient$id==patientId))
  if(length(index) == 0) stop(paste("Patient", patientId, "not found"))
  
  patient <- db$patients[[index]]
  
  # TEMPORARY, add use column and target field
  # For me, both should be persisted, this way, we do not have this issue anymore
  # Or at least setPatient method should arranged a bit
  measures <- patient$measures
  measures$use <- TRUE
  patient$measures <- measures
  patient$target <- list(min=12, max=15)
  
  return(patient)
}

#' Check if the doses from the recommended regimen are correct.
#'
#' @param recommendedRegimen recommended regimen, data frame
#' @param expectedDoses expected doses, numeric vector
#'
expectDoses <- function(recommendedRegimen, expectedDoses) {
  # as.numeric used to remove names
  expect_equal(round(recommendedRegimen %>% dplyr::filter(!PAST) %>% dplyr::pull(AMT), 2), round(expectedDoses, 2))
}

#' Encapsulate the prepareRecommendation method.
#'
#' @param patient
#' @return a list
#'
prepareRecommendationTest <- function(patient) {
  list <- prepareRecommendation(
    doses = patient$doses,
    obs = patient$measures,
    model = get(patient$model),
    covs = patient$covariates,
    target = patient$target,
    now = patient$now
  )
  return(list)
} 