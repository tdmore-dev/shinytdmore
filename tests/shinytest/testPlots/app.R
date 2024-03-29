icuSetCollate(locale="ASCII")
Sys.setlocale(category="LC_TIME", locale="C")
library(shinytdmore)
library(shiny)

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
  fitPlotUI("plots", height="80vh"),
  htmlOutput("debug")
)

## This rounds the reactive value, to ensure reproducibility between machines
roundedReactive <- function(r, digits=6, label="Rounded reactive") {
  force(r)
  reactive({
    res <- r()
    cat("Rounding reactive ",label, " of class ", class(res), "...\n")
    if(is.data.frame(res)) {
      res <- dplyr::mutate_if(res, is.numeric, signif, digits=digits)
    } else if (tdmore::is.tdmorefit(res)){
      res$res <- signif(res$res, 5)
      res$varcov <- signif(res$varcov, 3)
    } else {
      stop("Unknown type: ", class(res))
    }
    res
  }, label=label)
}

shinyApp(ui=ui, server=function(input, output, session) {
  state <- reactiveValues()
  state$regimen <- tibble::tibble(
    time=as.POSIXct("2000-01-01 08:00", tz="GMT")+lubridate::dhours(seq(0, 100, by=8)),
    dose=15,
    formulation="CompoundA",
    fix=c(TRUE, TRUE, TRUE, rep(FALSE, 10))
  )
  state$target <- list(min=12, max=15)
  state$observed <- tibble::tibble(
    time=as.POSIXct("2000-01-01 11:30", tz="GMT"),
    dv=60,
    use=TRUE
  )
  state$model <- myModel
  state$now <- as.POSIXct("2000-01-02 08:00", tz="GMT")
  
  set.seed(0)
  cr <- calculationReactives(state)
  roundKeys <- c("fit", "populationPredict", "populationPredictNoSe", "individualPredict", "individualPredictNoSe", "recommendationPredict")
  for(i in roundKeys) cr[[i]] <- roundedReactive(cr[[i]], digits=4, label=i)
  callModule(fitPlot, "plots", state=state, cr=cr)
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
