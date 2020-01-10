#' This UI element displays several plots
#' that show the current state of the fit.
#' 
#' It is displayed in a tabsetPanel
# TODO: change this to use Next / Previous buttons
# TODO: Change the plots to render a single time
# and then update the data on them instead
# TODO: add progress bars while rendering the plots
# TODO: add interactivity in the plots themselves
fitPlotUI <- function(id) {
  ns <- NS(id)
  tabsetPanel(
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
  #tsPanel$children[[1]]$attribs$style = "display: none;" #hide
  ## TODO: add Previous / Next buttons instead
  ## They should work fully using javascript,
  ## because we do not want to add server-side code for something
  ## so trivial...
  
  tsPanel
}

fitPlot <- function(input, output, session, state) {
  stateDebounced <- debounce(reactive({
    list(regimen=state$regimen,
         model=state$model,
         regimen=state$regimen,
         observed=state$observed,
         covs=state$covs,
         now=state$now,
         target=state$target)
  }), millis=500)
  if(is.null( isolate({state$populationData}) ))
    state$populationData <- reactive({
      preparePrediction(stateDebounced(), population=TRUE)
    })
  if(is.null(isolate({state$individualData})))
    state$individualData <- reactive({
      preparePrediction(stateDebounced(), population=FALSE)
    })
  if(is.null(isolate({state$recommendationData})))
    state$recommendationData <- reactive({
      prepareRecommendation(state$individualData())
    })
  
  output$population <- plotly::renderPlotly({
    data <- state$populationData()
    plots <- preparePredictionPlots(data)
    z1 <- mergePlots(plots$p1, plots$p2, plots$p3, getModelOutput(getDefaultModel(state$model)) )
    z1
  })
  outputOptions(output, "population", priority = -10)
  output$fit <- plotly::renderPlotly({
    data <- state$individualData()
    plots <- preparePredictionPlots(data)
    z1 <- mergePlots(plots$p1, plots$p2, plots$p3, getModelOutput(getDefaultModel(state$model)) )
    z1
  })
  outputOptions(output, "fit", priority = -10)
  ##
  ##recommendedRegimen <- recommendationData$recommendedRegimen
  ##data <- state$regimen
  ##data$rec <- ifelse(recommendedRegimen$PAST, "/", round(recommendedRegimen$AMT, 2))
  ## renderHotDoseFuture(data)
  output$recommendation <- plotly::renderPlotly({
    data <- state$recommendationData()
    plots <- prepareRecommendationPlots(data)
    z1 <- mergePlots(plots$p1, plots$p2, plots$p3, getModelOutput(getDefaultModel(state$model)) )
    z1
  })
  outputOptions(output, "recommendation", priority = -10)
}