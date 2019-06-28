#'
#' Say if the user went out of the 'Prediction' tab.
#'
#' @param input shiny input object
#' @param onTabChanged onTabChanged reactive values
#' @return a logical value
#'
tabHasChanged <- function(onTabChanged) {
  currentTab <- onTabChanged$currentTab
  lastTab <- onTabChanged$lastTab
  if (is.null(lastTab)) {
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
saveProjectToDB <- function(val) {
  val$patient <- updatePatientDoses(val$patient, val$db_dose)
  val$patient <- updatePatientMeasures(val$patient, val$db_obs %>% select(-use))
  val$patient <- updateNowDate(val$patient, val$now_date)
  updatePatient(val$patient$id, val$patient)
}

#'
#' Save project server
#'
#' @param input shiny input
#' @param output shiny output
#' @param session shiny session
#' @param onTabChanged onTabChanged reactive values
#' @param val main reactive container
#' 
#' @export
#'
saveProject <- function(input, output, session, onTabChanged, val) {
  # Save project button observer
  observeEvent(input$saveProject, {
    saveProjectToDB(val)
    removeModal()
  })
  
  # Save project logic
  observeEvent(onTabChanged$currentTab, {
    if (tabHasChanged(onTabChanged) && dataHasChanged(val)) {
      # Make sure the patient is not read-only
      if (isReadOnlyPatient(val$patient)) {
        showModal(
          modalDialog(
            title = "Save project",
            "This is a read-only patient, it cannot be saved.",
            footer = tagList(modalButton("OK"))
          )
        )
      } else {
        showModal(
          modalDialog(
            title = "Save project",
            "Do you want to save the changes?",
            footer = tagList(modalButton("Cancel"),
                             actionButton(session$ns("saveProject"), "OK"))
          )
        )
      }
    }
  })
}