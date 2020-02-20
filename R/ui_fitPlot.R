#' This UI element displays several plots
#' that show the current state of the fit.
#' 
#' It is displayed in a tabsetPanel
#' 
#' @param id output ID
#' @param height height of the plotly plots
#' Use "80vh" to cover 80\% of the screen space, or use a pixel amount to have a fixed size plot.
#' 
#' @export
# TODO: change this to use Next / Previous buttons
# TODO: Change the plots to render a single time
# and then update the data on them instead
# TODO: add progress bars while rendering the plots
# TODO: add interactivity in the plots themselves
fitPlotUI <- function(id, height="auto") {
  ns <- NS(id)
  panel <- pagerPanel(
    id=ns("active"),
    tabPanel(title = "Population",
             value="population",
             plotly::plotlyOutput(ns("population"), height=height)
    ),
    tabPanel(title = "Fit",
             value="fit",
             plotly::plotlyOutput(ns("fit"), height=height)
    ),
    tabPanel(title = "Recommendation",
             value="recommendation",
             plotly::plotlyOutput(ns("recommendation"), height=height)
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
#' @inheritParams shinytdmore-module
#' @export
fitPlot <- function(input, output, session, state, cr=NULL) {
  if(is.null(cr)) cr <- calculationReactives(state)
  
  output$population <- plotly::renderPlotly({
    plots <- preparePredictionPlots(cr$populationPredict(),
                                    NULL,
                                    observed=state$observed, target=state$target, model=state$model, now=state$now,
                                    regimen=state$regimen)
    z <- mergePlots(plots$p1, plots$p2, plots$p3, getModelOutput(getDefaultModel(state$model)), source=session$ns("population"))
    z
  })
  outputOptions(output, "population", priority = -10)
  
  output$fit <- plotly::renderPlotly({
    plots <- preparePredictionPlots(cr$populationPredictNoSe(),
                                    cr$individualPredict(),
                                    observed=state$observed, target=state$target, model=state$model, now=state$now,
                                    regimen=state$regimen)
    mergePlots(plots$p1, plots$p2, plots$p3, getModelOutput(getDefaultModel(state$model)),
               source=session$ns("fit"))
  })
  outputOptions(output, "fit", priority = -10)
  
  output$recommendation <- plotly::renderPlotly({
    plots <- prepareRecommendationPlots(
      cr$populationPredictNoSe(),
      cr$individualPredictNoSe(),
      cr$recommendationPredict(),
      observed=state$observed, target=state$target, model=state$model, now=state$now,
      regimen=state$regimen,
      cr$recommendation()
      )
    mergePlots(plots$p1, plots$p2, plots$p3, getModelOutput(getDefaultModel(state$model)),
               source=session$ns("recommendation"))
  })
  outputOptions(output, "recommendation", priority = -10)
}