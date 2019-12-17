library(shinytdmore)

ui <- doseTableUI("myDose")

shinyApp(ui=ui, server=function(input, output, session) {
  state <- reactiveValues()
  callModule(doseTable, "myDose", state=state)
  exportTestValues(regimen = { state$regimen })
})
