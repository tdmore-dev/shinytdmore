#' Input element representing the target upper limit and lower limits
#' The input element is synchronized to state$target$min and state$target$max
#' If state$target does not exist, it creates this as list(min=NA, max=NA).
#' NA assumes the model-defined min/max criteria will be used.
#' 
#' @param id unique identifier
#' @name targetInput
targetInputUI <- function(id) {
  ns <- NS(id)
  tags$div(
    uiOutput(ns("targetDown"), inline=TRUE),
    uiOutput(ns("targetUp"), inline=TRUE),
  )
}

#' @name targetInput
#' @inheritParams shinytdmore-module
targetInput <- function(input, output, session, state) {
  modelTargets <- reactive({
    target <- tdmore::getMetadataByClass(state$model, "tdmore_target")
    list(min=target$min, max=target$max)
  })
  output$targetDown <- renderUI({
    # this is only reset if modelTargets changes
    value <- state$target$min
    if(!is.numeric(value)) value <- modelTargets()$min
    numericInput(session$ns("targetDown"), "Lower limit", value=value)
  })
  output$targetUp <- renderUI({
    value <- state$target$max
    if(!is.numeric(value)) value <- modelTargets()$max
    numericInput(session$ns("targetUp"), "Upper limit", value=value)
  })
  
  # link targetDown
  observeEvent(input$targetDown, {
    target <- if(is.null(state$target)) list(min=NA, max=NA) else state$target
    target$min <- as.numeric(input$targetDown)
    state$target <- target
  })
  observeEvent(state$target, {
    updateNumericInput(session=session, inputId="targetDown", value=state$target$min)
  })
  
  # link targetUp
  observeEvent(input$targetUp, {
    target <- if(is.null(state$target)) list(min=NA, max=NA) else state$target
    target$max <- as.numeric(input$targetUp)
    state$target <- target
  })
  observeEvent(state$target, {
    updateNumericInput(session=session, inputId="targetUp", value=state$target$max)
  })
}