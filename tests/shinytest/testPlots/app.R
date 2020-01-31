library(shinytdmore)

rxModel <- RxODE::RxODE(
"
KA = 0.3;
TVCL = 10;
TVV = 5;

CL = TVCL * exp(ECL);
V = TVV * exp(EV);

d/dt(A0) = -KA*A0;
d/dt(A1) = KA*A0 - CL/V*A1;
CONC = A1 / V * 1000;
")
tdmModel <- tdmore::tdmore(rxModel,
  omega=c(ECL=0.3, EV=0.5),
  res_var=list(tdmore::errorModel(prop=0.3))
)
myModel <- tdmore::metadata(tdmModel, 
                            tdmore::observed_variables(c("CL", "V")),
                            tdmore::formulation(name="CompoundA", unit="mg", dosing_interval=8))

ui <- fluidPage(
  fitPlotUI("plots"),
  htmlOutput("debug")
)

shinyApp(ui=ui, server=function(input, output, session) {
  state <- reactiveValues()
  state$regimen <- tibble::tibble(
    time=as.POSIXct("2000-01-01 08:00")+lubridate::dhours(seq(0, 100, by=8)),
    dose=15,
    formulation="CompoundA",
    fix=c(TRUE, TRUE, TRUE, rep(FALSE, 10))
  )
  state$target <- list(min=12, max=15)
  state$observed <- tibble::tibble(
    time=as.POSIXct("2000-01-01 11:30"),
    dv=60,
    use=TRUE
  )
  state$model <- myModel
  state$now <- as.POSIXct("2000-01-02 08:00")
  calculation(state)
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
