library(shinytdmore)

ui <- fluidPage(
     editableInput("myText", "text", ""),
     tags$br(),
     editableHtml5("myDateHtml5", "datetime-local", ""),
     tags$br(),
     editableCombodate("myDateCombodate", value="2019-12-10 20:00"),
     #tags$br(),
     verbatimTextOutput("bar")
 )

server <- function(input, output, session) {
  output$bar <- renderText({
    names <- c("myText", "myDateHtml5", "myDateCombodate")
    values <- lapply(names, function(x){ input[[x]] })
    names(values) <- names
    myOut <- paste(
      names(values), "-->", values, " of class ", lapply(values, class), collapse="\n", sep=" "
      )
    myOut
  })
}
  
shinyApp(ui, server)
