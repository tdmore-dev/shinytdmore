library(shinytdmore)
library(shiny)
library(tibble)
library(rhandsontable)

Nstart <- 20
PlotSleep <- 2

ui <- fluidPage(
  actionButton("add", label="Add row", icon=shiny::icon("add")),
  synchronizedHotUi("hot"),
  plotOutput("plot")
)

server <- function(input, output, session) {
  state <- reactiveValues(df = data.frame(foo=rnorm(Nstart), bar=runif(Nstart)))
  callModule(synchronizedHot, id="hot", stateDf=function(x){
      if(missing(x)) state$df else state$df <- x
    }, expr={
      cat("Running expression...\n")
      df <- isolate({state$df})
      rhandsontable::rhandsontable(df)
    }, debug=TRUE)
  observeEvent(input$add, {
    cat("EVENT input$add\n")
    state$df <- rbind( state$df, tibble(foo=rnorm(1), bar=runif(1)) )
  })
  
  debouncedState <- debounce(reactive({
    list(df=state$df)
  }), millis=2000)
  output$plot <- renderPlot({
    state <- debouncedState()
    cat("RENDER output$plot\n")
    Sys.sleep(PlotSleep) #this is usually a lengthy calculation
    plot( state$df, main=paste(nrow(state$df), "rows") )
  })
  outputOptions(output, "plot", priority=-99) #low priority
  
  shiny::exportTestValues( df = state$df, Nstart=Nstart, PlotSleep=PlotSleep )
}

shinyApp(ui=ui, server=server)
