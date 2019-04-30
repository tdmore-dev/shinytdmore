#'
#' Say if the user went out of the 'Prediction' tab.
#'
#' @param input shiny input object
#' @param val main reactive container
#' @return a logical value
#'
tabHasChanged <- function(input, val) {
  currentTab <- input$tabs
  lastTab <- val$lastTab
  if(length(lastTab) == 0) {
    return(FALSE) 
  } else {
    return(currentTab != lastTab && lastTab == "Prediction")
  } 
}

#'
#' Say if data (measures or doses) have been changed by the user.
#'
#' @param val main reactive container
#' @return a logical value
#'
dataHasChanged <- function(val) {
  patient <- val$patient
  dosesHasChanged <- !are_equal(patient$doses, val$db_dose)
  measuresHasChanged <- !are_equal(patient$measures, val$db_obs %>% select(-use))
  nowDateHasChanged <- !are_equal(patient$now_date, val$now_date)
  return(dosesHasChanged || measuresHasChanged || nowDateHasChanged)
}

#'
#' Save project into DB (measures and doses from shiny are saved).
#'
#' @param val main reactive container
#'
saveProject <- function(val) {
  val$patient <- updatePatientDoses(val$patient, val$db_dose)
  val$patient <- updatePatientMeasures(val$patient, val$db_obs %>% select(-use))
  val$patient <- updateNowDate(val$patient, val$now_date)
  updatePatient(val$patient$id, val$patient)
}

#'
#' Save project server
#'
#' @param input shiny input object
#' @param val main reactive container
#'
saveProjectServer <- function(input, val) {
  # Save project button observer
  observeEvent(input$saveProject, {
    saveProject(val)
    removeModal()
  })
  
  # Save project logic
  observe({
    if (tabHasChanged(input, val) && dataHasChanged(val)) {
      showModal(
        modalDialog(
          title = "Save project",
          "Do you want to save the changes?",
          footer = tagList(modalButton("Cancel"),
                           actionButton("saveProject", "OK"))
        )
      )
    }
    val$lastTab <- input$tabs
  })
}