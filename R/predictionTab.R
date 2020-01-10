#'
#' Prediction tab user interface.
#'
#' @param id namespace id
#' @return a panel
#' 
#' @export
#'
predictionTabUI <- function(id) {
  ns <- NS(id)
  panel <- shiny::tabPanel(
    "Prediction",
    value="prediction",
    id="prediction",
    icon = icon("address-card"),
      column(4,
        tags$head(
         tags$style(HTML(paste0("
            #", id, " .handsontable {
              overflow: hidden;
            }
          ")))),
    shinyBS::bsCollapse(id=ns("bsCollapse"), multiple=T, open=c("Doses", "Measures", "Now"),
      shinyBS::bsCollapsePanel(title="Doses", style="primary",
        doseTableUI(ns("doses"))
      ),
      shinyBS::bsCollapsePanel(title="Measures", style="primary", 
        observationTableUI(ns("observation"))
      ),
      shinyBS::bsCollapsePanel(title="Now", style="primary",
        nowInputUI(ns("now"))
      ),
      shinyBS::bsCollapsePanel(title="Target", style="primary",
        targetInputUI(ns("target"))
      )
    )
    ),
    column(8, fitPlotUI(ns("plots")) )
  )
  return(panel)
}

#'
#' Prediction tab server.
#'
#' @param input shiny input
#' @param output shiny output
#' @param session shiny session
#' @param state program state, containing the fields `now`, `model`, `regimen`, `observed`
#' 
#' @export
#'
predictionTab <- function(input, output, session, state) {
  # link input elements to `state`
  callModule(doseTable, "doses", state=state)
  callModule(observationTable, "observation", state=state)
  callModule(nowInput, "now", state=state)
  callModule(targetInput, "target", state=state)
  
  # output plots
  callModule(fitPlot, "plots", state=state)
  
  # adapt visibility of input elements
  visibilityStates <- tibble(
    row.names=c("population", "fit", "recommendation"),
    doses=c(TRUE, TRUE, FALSE),
    recommendation=c(FALSE, FALSE, TRUE)
  )
  observeEvent(input$`plots-selected`, {
    browser()
    i <- input$`plots-selected`
    row <- visibilityStates[i, , drop=TRUE]
    for(i in names(row)) shinyjs::toggle(id=i, condition=row[i])
  })
}
