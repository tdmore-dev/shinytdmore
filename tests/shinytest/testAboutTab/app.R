icuSetCollate(locale="ASCII")
library(shinytdmore)

ui <- aboutTabUI(id = "aboutTabId")

shinyApp(ui=ui, server=function(input, output, session) {})
