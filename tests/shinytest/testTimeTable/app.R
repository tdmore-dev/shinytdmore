icuSetCollate(locale="ASCII")
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
  state <- reactiveValues(df = data.frame(time=rep(Sys.time(), Nstart), bar=runif(Nstart)))
  callModule(synchronizedHot, 
             id="hot", 
             stateDf=singleReactive(state, "df"), expr={
      cat("Running expression...\n")
      df <- isolate({state$df})
      timeTable(df, colHeaders=c("Bar"))
    }, hot_to_r=hot_to_r_datetime)
  observeEvent(input$add, {
    cat("EVENT input$add\n")
    state$df <- rbind( state$df, tibble(time=Sys.time(), bar=runif(1)) )
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
