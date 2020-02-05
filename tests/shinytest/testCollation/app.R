library(shinytdmore)
library(shiny)
library(tibble)
library(rhandsontable)

ui <- fluidPage(
)

server <- function(input, output, session) {
  shiny::exportTestValues(a=1, b=3, A=8, B=10, LC_COLLATE={
    Sys.getenv("LC_COLLATE")
    },
    icuGetCollate={
      icuGetCollate()
      },
  sortedArray={
    sort(strsplit("abcdefghijklmnopABCDEFGHIJKLMNOP", NULL)[[1]])
  })
}

shinyApp(ui=ui, server=server)
