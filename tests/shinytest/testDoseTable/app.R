icuSetCollate(locale="ASCII")
library(shinytdmore)

options(shiny.reactlog=TRUE)
options(useFancyQuotes = FALSE)

myModel <- tdmore::tdmore(
  tdmore::algebraic(fun=function(t, TIME, AMT, EV) {
    V <- 20 * exp(EV)
    CL <- 10
    K <- CL / V
    K
    tdmore::pk1cptiv()
  }),
  omega=c(EV=0.10),
  res_var=tdmore::errorModel(prop=0.10)
)
myModelWithMetadata <- tdmore::metadata(myModel,
  tdmore::formulation(name="FormA", unit="mg", dosing_interval=8, default_value=5),
  tdmore::formulation(name="FormB", unit="mg", dosing_interval=8, default_value=5)
)

ui <- fluidPage(
  tableUI("myDose", label="Add dose", icon=icon("add")),
  tableOutput("myDoseOut")
)

shinyApp(ui=ui, server=function(input, output, session) {
  state <- reactiveValues(model=myModelWithMetadata)
  callModule(doseTable, "myDose", state=state)
  output$myDoseOut <- renderTable( state$regimen )
  exportTestValues(regimen = { state$regimen })
})
