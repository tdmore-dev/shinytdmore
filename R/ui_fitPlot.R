#' This UI element displays several plots
#' that show the current state of the fit.
#' 
#' It is displayed in a tabsetPanel
#' 
#' @param id output ID
#' 
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

# This function checks all of the arguments.
# If any of the arguments is an error, it emits the error
checkError <- function(..., verbose=TRUE) {
  args <- list(...)
  for(i in seq_along(args)) {
    value <- args[[i]]
    if(inherits(value, "error")) {
      if(verbose) {
        cat(names(args)[i], " --> ")
        print(value)
        dput(value)
      }
      signalCondition(value)
    }
  }
}

#' Fitplot creates a Population, Fit and Recommendation plot
#' 
#' @inheritParams shinytdmore-module
#' @export
fitPlot <- function(input, output, session, state) {
  ## TODO: plots depend on state$populationPredict, which comes from a debounced regimen/observed/...
  ## However, they also directly depend on regimen/observed/...
  ## 
  ## If regimen is modified, the plot is updated twice:
  ## 1) When the regimen is modified
  ## 2) When the populationPredict is modified 500 milli-seconds later...
  ## This can probably be resolved with freeze/thaw, but this is still mystical to me...
  output$population <- plotly::renderPlotly({
    checkError(populationPredict=state$populationPredict,
               observed=state$observed, target=state$target, model=state$model, now=state$now,
               regimen=state$regimen)
    validate(
      need(state$populationPredict, label="Prediction")
    )
    plots <- preparePredictionPlots(state$populationPredict,
                                    NULL,
                                    observed=state$observed, target=state$target, model=state$model, now=state$now,
                                    regimen=state$regimen)
    z <- mergePlots(plots$p1, plots$p2, plots$p3, getModelOutput(getDefaultModel(state$model)) )
    z
  })
  outputOptions(output, "population", priority = -10)
  output$fit <- plotly::renderPlotly({
    checkError(
      populationPredict=state$populationPredict,
      individualPredict=state$individualPredict,
      observed=state$observed, target=state$target, model=state$model, now=state$now,
      regimen=state$regimen
    )
    validate(
      need(state$individualPredict, label="Prediction")
    )
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
    checkError(
      populationPredict=state$populationPredict,
      individualPredict=state$individualPredict,
      recommendationPredict=state$recommendationPredict,
      observed=state$observed, target=state$target, model=state$model, now=state$now,
      regimen=state$regimen,
      recommendation=state$recommendation
    )
    validate(
      need(state$populationPredict, label="Prediction") %||%
      need(state$individualPredict, label="Prediction") %||%
      need(state$recommendationPredict, label="Prediction")
    )
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