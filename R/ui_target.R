#' Input element representing the target upper limit and lower limits
#' 
#' @param id unique identifier
#' @name targetInput
targetInputUI <- function(id) {
  ns <- NS(id)
  tags$div(
    numericInput(ns("targetDown"), "Lower limit", 0),
    numericInput(ns("targetUp"), "Upper limit", 0)
  )
}

#' @name targetInput
targetInput <- function(input, output, session, state) {
  # link targetDown
  observeEvent(input$targetDown, {
    if(is.null(state$target)) state$target <- list(min=0, max=0)
    if(state$target$min != input$targetDown) state$target$min <- as.numeric(input$targetDown)
  })
  observeEvent(state$target, {
    if(state$target$min != input$targetDown) updateNumericInput(session=session, inputId="targetDown", value=state$target$min)
  })
  
  # link targetUp
  observeEvent(input$targetUp, {
    if(is.null(state$target)) state$target <- list(min=0, max=0)
    if(state$target$max != input$targetUp) state$target$max <- as.numeric(input$targetUp)
  })
  observeEvent(state$target, {
    if(state$target$max != input$targetUp) updateNumericInput(session=session, inputId="targetUp", value=state$target$max)
  })
}