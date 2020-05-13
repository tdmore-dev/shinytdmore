ui <- fluidPage(
  actionButton("randomize", label="Randomize"),
  plotly::plotlyOutput("plot")
)

model <- tdmore::getModel()
pop <- estimate(model, regimen=data.frame(TIME=0, AMT=10), covariates=c(WT=70))

server <- function(input, output, session) {
  shinytdmore::renderUpdatePlotly(output, outputId="plot", expr={
    input$randomize
    N <- 1000
    x <- predict(pop, newdata=seq(0, 24, length.out=N), se.fit=TRUE, mc.maxpts=1, level=NA)
    
    p <- ggplot(x, aes(x=TIME, y=CONC)) +
      geom_line()
    plotly::ggplotly(p)
    
    plotly::plot_ly(x, x=~TIME, y=~CONC, type="scatter", `mode`="lines")
  })
}

shiny::shinyApp(ui, server)
