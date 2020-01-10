library(shinytdmore)
library(shiny)
library(tibble)
library(rhandsontable)

Nstart <- 20
PlotSleep <- 2

ui <- fluidPage(
  actionButton("add", label="Add row", icon=shiny::icon("add")),
  rhandsontable::rHandsontableOutput("table"),
  plotOutput("plot")
)

server <- function(input, output, session) {
  state <- reactiveValues(df = data.frame(foo=1:Nstart, bar=1:Nstart))
  invalidateTable <- reactiveVal(value=NA)
  
  #Changing the output also triggers a change in the input$table
  output$table <- rhandsontable::renderRHandsontable({
    cat("RENDER output$table\n")
    invalidateTable()
    df <- isolate({state$df})
    rhandsontable::rhandsontable(df)
  })
  observeEvent(state$df, {
    if(!is.null(input$table) && !isTRUE(all.equal(state$df, rhandsontable::hot_to_r(input$table)))) {
      cat("INVALIDATING output$table\n")
      invalidateTable(runif(1))
    } else {
      cat("IGNORING state$df change; equal to input$table\n")
    }
  })
  
  observeEvent(input$add, {
    cat("EVENT input$add\n")
    state$df <- rbind( state$df, tibble(foo=nrow(state$df)+1, bar=nrow(state$df)+1) )
  })
  
  observeEvent(input$table, {
    changes <- input$table$changes
    if(isTRUE(all.equal(changes, list(event="afterChange", changes=NULL)))) {
      # change is due to loadData updating the shiny input binding
      # ## changes: { event: "afterChange", changes: null },
      # See https://github.com/jrowen/rhandsontable/blob/master/inst/htmlwidgets/rhandsontable.js#L147
      # this should *not* propagate back to state$df
    } else {
      cat("EVENT input$table with changes ", capture.output(print(input$table$changes)), "\n")
      state$df <- rhandsontable::hot_to_r(input$table)
    }
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
