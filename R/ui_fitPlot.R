#' This UI element displays several plots
#' that show the current state of the fit.
#' 
#' It is displayed in a tabsetPanel
#' @export
# TODO: change this to use Next / Previous buttons
# TODO: Change the plots to render a single time
# and then update the data on them instead
# TODO: add progress bars while rendering the plots
# TODO: add interactivity in the plots themselves
fitPlotUI <- function(id) {
  ns <- NS(id)
  pagerPanel(
    id=ns("active"),
    tabPanel(title = "Population",
             value="population",
             plotly::plotlyOutput(ns("population"))
    ),
    tabPanel(title = "Fit",
             value="fit",
             plotly::plotlyOutput(ns("fit"))
    ),
    tabPanel(title = "Recommendation",
             value="recommendation",
             plotly::plotlyOutput(ns("recommendation"))
    )
  )
}

pagerPanel <- function(id, ...) {
  tsPanel <- tabsetPanel(id = id, ...)
  ## TODO: add Previous / Next buttons instead
  ## They should work fully using javascript,
  ## because we do not want to add server-side code for something
  ## so trivial...
  
  tsPanel
}

#' Fitplot creates a Population, Fit and Recommendation plot
#' 
#' @export
fitPlot <- function(input, output, session, state) {
  output$population <- plotly::renderPlotly({
    shiny::req(state$populationPredict)
    plots <- preparePredictionPlots(state$populationPredict,
                                    NULL,
                                    observed=state$observed, target=state$target, model=state$model, now=state$now,
                                    regimen=state$regimen)
    mergePlots(plots$p1, plots$p2, plots$p3, getModelOutput(getDefaultModel(state$model)) )
  })
  outputOptions(output, "population", priority = -10)
  output$fit <- plotly::renderPlotly({
    shiny::req(state$individualPredict)
    plots <- preparePredictionPlots(state$populationPredict,
                                    state$individualPredict,
                                    observed=state$observed, target=state$target, model=state$model, now=state$now,
                                    regimen=state$regimen)
    mergePlots(plots$p1, plots$p2, plots$p3, getModelOutput(getDefaultModel(state$model)) )
  })
  outputOptions(output, "fit", priority = -10)
  ##
  ##recommendedRegimen <- recommendationData$recommendedRegimen
  ##data <- state$regimen
  ##data$rec <- ifelse(recommendedRegimen$PAST, "/", round(recommendedRegimen$AMT, 2))
  ## renderHotDoseFuture(data)
  output$recommendation <- plotly::renderPlotly({
    shiny::req(state$populationPredict)
    shiny::req(state$individualPredict)
    shiny::req(state$recommendationPredict)
    shiny::req(state$recommendation)
    plots <- prepareRecommendationPlots(
      state$populationPredict,
      state$individualPredict,
      state$recommendationPredict,
      observed=state$observed, target=state$target, model=state$model, now=state$now,
      regimen=state$regimen,
      state$recommendation
      )
    mergePlots(plots$p1, plots$p2, plots$p3, getModelOutput(getDefaultModel(state$model)) )
  })
  outputOptions(output, "recommendation", priority = -10)
}