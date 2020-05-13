icuSetCollate(locale="ASCII")
library(shinytdmore)

ui <- tableUI("myObs", label="Add")

shinyApp(ui=ui, server=function(input, output, session) {
  state <- reactiveValues()
  callModule(observationTable, "myObs", state=state)
  exportTestValues(observed = { state$observed })
})
