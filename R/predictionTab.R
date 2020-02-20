#'
#' Prediction tab user interface.
#'
#' @param id namespace id
#' @inheritParams fitPlotUI
#' 
#' @return a panel
#' 
#' @export
#'
predictionTabUI <- function(id, height="auto") {
  if(utils::packageVersion("shinyBS") <= package_version("0.61")) {
    # bugfix for https://github.com/ebailey78/shinyBS/issues/115
    shiny::addResourcePath("sbs", system.file("www", package="shinyBS"))
  }
  ns <- NS(id)
  panel <- shiny::tabPanel(
    "Prediction",
    value="prediction",
    id="prediction",
    icon = icon("address-card"),
      column(4,
        tags$head(
         tags$style(HTML(paste0( #remove scrollbars around the handsontable
         "#", id, " .handsontable {
              overflow: hidden;
          }"
          )))),
    shinyjs::useShinyjs(), #useShinyjs must appear in the UI output!
    shinyBS::bsCollapse(id=ns("bsCollapse"), multiple=T, open=c("Doses", "Measures", "Now"),
      shinyBS::bsCollapsePanel(title="Doses", style="primary",
        tableUI(ns("doses"), "Add dose"), #either one of these is shown through shinyJS::toggle()
        shinyjs::hidden(tableUI(ns("recommendation"), "Add dose"))
      ),
      shinyBS::bsCollapsePanel(title="Measures", style="primary", 
        tableUI(ns("observation"), "Add observation")
      ),
      shinyBS::bsCollapsePanel(title="Covariates", style="primary", 
        tableUI(ns("covariates"), "Add covariate")
      ),
      shinyBS::bsCollapsePanel(title="Now", style="primary",
        nowInputUI(ns("now"))
      ),
      shinyBS::bsCollapsePanel(title="Target", style="primary",
        targetInputUI(ns("target"))
      )
    )
    ),
    column(8, fitPlotUI(ns("plots"), height=height) )
  )
  return(panel)
}

#'
#' Prediction tab server.
#'
#' @inheritParams shinytdmore-module
#' @export
#'
predictionTab <- function(input, output, session, state, cr=NULL) {
  if(is.null(cr)) cr <- calculationReactives(state)
  
  # link input elements to `state`
  callModule(doseTable, "doses", state=state)
  callModule(recommendationTable, "recommendation", state=state, recommendation=cr$recommendation)
  callModule(observationTable, "observation", state=state)
  callModule(covariatesTable, "covariates", state=state)
  callModule(nowInput, "now", state=state)
  callModule(targetInput, "target", state=state)
  
  # output plots
  callModule(fitPlot, "plots", state=state, cr=cr)
  
  # adapt visibility of input elements
  visibilityStates <- tibble(
    row.names=c("population", "fit", "recommendation"),
    doses=c(TRUE, TRUE, FALSE),
    recommendation=c(FALSE, FALSE, TRUE)
  ) %>% tibble::column_to_rownames("row.names")
  
  observeEvent(input$`plots-active`, {
    i <- input$`plots-active`
    row <- visibilityStates[i, , drop=TRUE]
    for(j in names(row)) {
      condition <- as.logical(row[j])
      id <- j #ns is already applied by shinyjs!
      shinyjs::toggle(id=id, condition=condition)
    }
  })
}
