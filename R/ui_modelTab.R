#' Model tab user interface.
#'
#' @param id namespace id
#' @return a panel
#' 
#' @export
#'
modelTabUI <- function(id, dir=system.file("models", package="tdmore")) {
  ns <- NS(id)
  panel <- tabPanel(
    "Model",
    icon = icon("microchip"),
    fluidRow(
      textInput(ns("dir"), "Directory", value=dir),
      selectInput(ns("model"), "Your model", choices=tdmore::listModels(dir=dir)),
      actionButton(ns("update"), label="Apply this model", icon=icon("thumbs-up")),
      htmlOutput(ns("description")),
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
modelTab <- function(input, output, session, state, rmdFile=system.file("model.Rmd", package="shinytdmore")) {
  observeEvent(input$dir, {
    updateSelectInput(session, "model", choices=tdmore::listModels(dir=input$dir))
  })
  output$description <- renderUI({
    if(! input$model %in% tdmore::listModels(dir=input$dir) ) return("Model not available in dir")
    tempRmd <- file.path(tempdir(), basename(rmdFile))
    on.exit({unlink(tempRmd)})
    file.copy(rmdFile, tempRmd, overwrite = TRUE)
    
    # Set up parameters to pass to Rmd document
    params <- list(dir=input$dir, modelId=input$model)
    
    # Path of the temporary markdown file
    tempFile<- file.path(tempdir(), "out.md")
    on.exit({unlink(tempFile)})

    # Render markdown
    rmarkdown::render(input=tempRmd,
                      output_file=tempFile,
                      params=params,
                      envir=new.env(parent=globalenv()))
    
    # Include markdown in model tab
    includeMarkdown(path = tempFile)
  })
  
  # Apply model modal dialog button observer
  observeEvent(input$update, {
    state$model <- tdmore::getModel(input$model, dir=input$dir)
  })
}