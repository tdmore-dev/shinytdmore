#'
#' Reports tab user interface.
#'
#' @param id namespace id
#' @return a panel
#' 
#' @export
#'
reportsTabUI <- function(id) {
  ns <- NS(id)
  panel <- tabPanel(
    "Reports",
    icon = icon("book"),
    fluidRow(
      div(
        h5("Click the button below to download the report of", style="display: inline-block; vertical-align: middle;"),
        h5(textOutput(outputId=ns("patient_name")), style="display: inline-block; vertical-align: middle;")
      ),
      downloadButton(outputId=ns("report"), label="Generate report"),
      style="margin-left: 5px;"
    )
  )
  return(panel)
}

#' Reports tab server.
#'
#' @param input shiny input
#' @param output shiny output
#' @param session shiny session
#' @param val main reactive container
#' 
#' @export
#' 
reportsTab <- function(input, output, session, val) {
  
  # Update tab title according to patient's name
  output$patient_name <- renderText({return(paste(val$patient$firstname, val$patient$lastname))})
  
  # Download handler
  output$report <- downloadHandler(
    filename = "report.html",
    content = function(file) {
      # Copy the report file to a temporary directory before processing it, in
      # case we don't have write permissions to the current working dir (which
      # can happen when deployed).
      tempReport <- file.path(tempdir(), "report.Rmd")
      file.copy("report.Rmd", tempReport, overwrite = TRUE)
      
      # Set up parameters to pass to Rmd document
      params <- list(id = val$patient$id)
      
      # Knit the document, passing in the `params` list, and eval it in a
      # child of the global environment (this isolates the code in the document
      # from the code in this app).
      rmarkdown::render(input = tempReport,
                        output_file = file,
                        params = params,
                        envir = new.env(parent = globalenv())
      )
    }
  )
}