#'
#' Model tab user interface.
#'
#' @param id namespace id
#' @return a panel
#' 
#' @export
#'
modelTabUI <- function(id) {
  ns <- NS(id)
  panel <- tabPanel(
    "Model",
    icon = icon("file-text"),
    fluidRow(
      selectInput(ns("modelCombobox"), "Your model", getModelList()),
      htmlOutput(outputId=ns("modelDescription")),
      style="margin-left: 5px; margin-right: 5px;"
    )
  )
  return(panel)
}

#' Model tab server.
#'
#' @param input shiny input
#' @param output shiny output
#' @param session shiny session
#' @param val main reactive container
#' @param onTabChanged on tab changed
#' 
#' @export
#' 
modelTab <- function(input, output, session, val, onTabChanged) {
  
  renderModelDescription <- function(selectedModel) {
    output$modelDescription <- renderUI({
      tempRmd <- file.path(tempdir(), "model.Rmd")
      file.copy("model.Rmd", tempRmd, overwrite = TRUE)
      
      # Set up parameters to pass to Rmd document
      params <- list(modelId=selectedModel)
      
      # Path of the temporary markdown file
      tempFile<- file.path(tempdir(), "model.md")
      
      # Render markdown
      rmarkdown::render(input=tempRmd,
                        output_file=tempFile,
                        params=params,
                        envir=new.env(parent=globalenv()))
      
      # Include markdown in model tab
      includeMarkdown(path = tempFile)
    })
  }
  
  
  # On model changed
  observeEvent(val$model, {
    updateModelInSelectInput(val$model_id, session)
    renderModelDescription(val$model_id)
  })
  
  # On select input changed
  observeEvent(input$modelCombobox, {
    renderModelDescription(input$modelCombobox)
    val$updated_model <- input$modelCombobox
  })
  
  # Apply model modal dialog button observer
  observeEvent(input$applyModel, {
    applyModel(val)
    removeModal()
  })
  
  # Reject model modal dialog button observer
  observeEvent(input$rejectModel, {
    rejectModel(val, session)
    removeModal()
  })
  
  # Apply model logic
  observeEvent(onTabChanged$currentTab, {
    if (onModelTabExit(onTabChanged) && !is.null(val$updated_model) && val$updated_model != val$model_id) {
      showModal(
        modalDialog(
          title = "Apply model",
          "A new model has been selected. Do you want to apply it?",
          footer = tagList(actionButton(session$ns("rejectModel"), "Cancel"),
                           actionButton(session$ns("applyModel"), "OK"))
        )
      )
    }
  })
}

#'
#' Update the model in the select input component.
#'
#' @param selectedModel the new selected model
#' @param session shiny session
#'
updateModelInSelectInput <- function(selectedModel, session) {
  updateSelectInput(session, "modelCombobox",
                    label="Your model",
                    choices=getModelList(),
                    selected=selectedModel)
}

#'
#' Say if the user went out of the 'Model' tab.
#'
#' @param onTabChanged onTabChanged reactive values
#' @return a logical value
#'
onModelTabExit <- function(onTabChanged) {
  currentTab <- onTabChanged$currentTab
  lastTab <- onTabChanged$lastTab
  if (is.null(lastTab)) {
    return(FALSE) 
  } else {
    return(currentTab != lastTab && lastTab == "Model")
  } 
}

#'
#' Apply new selected model.
#' The old model is replaced by the new one.
#' If some covariates are missing, they are added automatically into the covariates data frame, with their default value.
#'
#' @param val main reactive container
#'
applyModel <- function(val) {
  oldCovariateNames <- getCovariateNames(val$model)
  val$model_id <- val$updated_model
  val$model <- get(val$model_id)
  newCovariateNames <- getCovariateNames(val$model)
  
  missingCovariateNames <- newCovariateNames[!(newCovariateNames %in% oldCovariateNames)]
  currentCovariateNames <- colnames(val$covs %>% dplyr::select(-date, -time))
  missingCovariateNames <- missingCovariateNames[!(missingCovariateNames %in% currentCovariateNames)]
  covs <- val$covs
  
  print(missingCovariateNames)
  
  # Special case, empty data frame (issue in tdmore/RxODE but easy fix here)
  if (length(missingCovariateNames) > 0 && nrow(covs) == 0) {
    if (nrow(val$doses) > 0) {
      covs <- val$doses[1,] %>% dplyr::select(date, time)
    } else {
      covs <- tibble::tibble(date=Sys.Date(), time=c("08:00"))
    }
  }
  
  for (covariate in missingCovariateNames) {
    metadata <- getMetadataByName(val$model, covariate)
    defaultValue <- 0
    if (inherits(metadata, "tdmore_covariate")) {
      choices <- metadata$choices
      if (is.null(choices)) {
        defaultValue <- (metadata$min + metadata$max) / 2
      } else {
        defaultValue <- choices[[1]]
      }
    }
    covs[, covariate] <- defaultValue
  }
  # Covariates update
  val$covs <- covs
}

#'
#' Reject new selected model.
#'
#' @param val main reactive container
#' @param session shiny session
#'
rejectModel <- function(val, session) {
  val$updated_model <- val$model_id
  updateModelInSelectInput(val$model_id, session)
}