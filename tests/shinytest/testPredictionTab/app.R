options(useFancyQuotes = FALSE)

library(shinytdmore)
options(shiny.reactlog=TRUE)

myModel <- RxODE::RxODE("
KA = 0.3;
TVCL = 10;
TVV = 5;

CL = TVCL * exp(ECL);
V = TVV * exp(EV);

d/dt(A0) = -KA*A0;
d/dt(A1) = KA*A0 - CL/V*A1;
CONC = A1 / V * 1000;
") %>% tdmore::tdmore(
  omega=c(ECL=0.3, EV=0.5),
  res_var=list(tdmore::errorModel(prop=0.3))
)

ui <- fluidPage(
  predictionTabUI("prediction"),
  htmlOutput("debug")
)

shinyApp(ui=ui, server=function(input, output, session) {
  state <- reactiveValues()
  state$target <- list(min=12, max=15)
  state$model <- myModel
  callModule(predictionTab, "prediction", state=state)
  exportTestValues(regimen = { state$regimen }, observed= {state$observed})
  # output$debug <- shiny::renderUI({
  #   
  #   out <- lapply(names(input), function(i){
  #     tags$li(tags$b(i),
  #             ":",
  #             tags$pre(
  #               paste(capture.output(print(input[[i]])), collapse="\n")
  #             )
  #     )
  #   })
  #   outTag <- tags$ol(
  #     out
  #   )
  # })
})
