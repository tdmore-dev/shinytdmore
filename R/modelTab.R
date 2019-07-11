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
#' 
#' @export
#' 
modelTab <- function(input, output, session, val) {
  observeEvent(val$set_patient_counter, {
    output$modelDescription <- renderUI({
      tempRmd <- file.path(tempdir(), "model.Rmd")
      file.copy("model.Rmd", tempRmd, overwrite = TRUE)
      
      # Set up parameters to pass to Rmd document
      params <- list(id = val$patient$id)
      
      # Path of the temporary markdown file
      tempFile<- file.path(tempdir(), "model.md")
      
      # Render markdown
      rmarkdown::render(input = tempRmd,
                        output_file = tempFile,
                        params = params,
                        envir = new.env(parent = globalenv()))
      
      # Include markdown in model tab
      includeMarkdown(path = tempFile)
    })
  })
  
  
}