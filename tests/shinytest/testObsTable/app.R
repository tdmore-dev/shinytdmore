library(shinytdmore)

ui <- observationTableUI("myObs")

shinyApp(ui=ui, server=function(input, output, session) {
  state <- reactiveValues()
  callModule(observationTable, "myObs", state=state)
  exportTestValues(observed = { state$observed })
})
