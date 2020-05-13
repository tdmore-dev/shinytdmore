icuSetCollate(locale="ASCII")
library(shinytdmore)

myModel <- tdmore::tdmore(
  tdmore::algebraic(fun=function(t, TIME, AMT, EV, WT) {
    V <- 20 * exp(EV)
    CL <- 10 * (WT/70)**0.75
    K <- CL / V
    K
    tdmore::pk1cptiv()
  }),
  omega=c(EV=0.10),
  res_var=tdmore::errorModel(prop=0.10)
)
myModelWithMetadata <- tdmore::metadata(myModel,
                                        tdmore::formulation(name="FormA", unit="mg", dosing_interval=8, default_value=5),
                                        tdmore::formulation(name="FormB", unit="mg", dosing_interval=8, default_value=5),
                                        tdmore::covariate(name="WT", label="Weight", unit="kg", min=20, max=200)
)



myModel2 <- tdmore::tdmore(
  tdmore::algebraic(fun=function(t, TIME, AMT, EV, SEX) {
    V <- 20 * exp(EV)
    CL <- 10 * 0.8**SEX #if SEX==1, then only use 80% of the value
    K <- CL / V
    K
    tdmore::pk1cptiv()
  }),
  omega=c(EV=0.10),
  res_var=tdmore::errorModel(prop=0.10)
)
myModelWithMetadata2 <- tdmore::metadata(myModel2,
                                        tdmore::formulation(name="FormA", unit="mg", dosing_interval=8, default_value=5),
                                        tdmore::formulation(name="FormB", unit="mg", dosing_interval=8, default_value=5),
                                        tdmore::covariate(name="SEX", label="Sex", choices=c(Male=0, Female=1))
)

ui <- fluidPage(
  tableUI("myCovs", label="Add covariate"),
  checkboxInput("metadata", label="Enable metadata", value=TRUE),
  checkboxInput("alternate", label="Use alternate model", value=FALSE)
)

shinyApp(ui=ui, server=function(input, output, session) {
  state <- reactiveValues()
  # observeEvent(input$alternate, {
  #   state$covariates <- NULL #remove the existing covariates
  # })
  observeEvent(c(input$alternate, input$metadata), {
    if(input$alternate) {
      if(input$metadata) {
        state$model <- myModelWithMetadata2
      } else {
        state$model <- myModel2
      }
    } else {
      if(input$metadata) {
        state$model <- myModelWithMetadata
      } else {
        state$model <- myModel
      }
    }
  })
  callModule(covariatesTable, "myCovs", state=state)
  exportTestValues(covs = { state$covariates })
})
