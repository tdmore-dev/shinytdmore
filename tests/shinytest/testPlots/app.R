library(shinytdmore)

myModel <- RxODE::RxODE(
"
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
  fitPlotUI("plots"),
  htmlOutput("debug")
)

shinyApp(ui=ui, server=function(input, output, session) {
  state <- reactiveValues()
  state$regimen <- data.frame(
    time=as.POSIXct(c("2000-01-01 08:00", "2000-01-02 08:00", "2000-01-03 08:00")),
    dose=15,
    formulation="DRUG",
    fix=FALSE
  )
  state$target <- list(min=12, max=15)
  state$observed <- data.frame(
    time=as.POSIXct("2000-01-01 11:30"),
    dv=20,
    use=TRUE
  )
  state$model <- myModel
  state$now <- as.POSIXct("2000-01-02 08:00")
  callModule(fitPlot, "plots", state=state)
  # output$debug <- shiny::renderUI({
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
