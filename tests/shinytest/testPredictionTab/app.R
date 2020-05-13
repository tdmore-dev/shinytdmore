icuSetCollate(locale="ASCII")
Sys.setlocale(category="LC_TIME", locale="C")
Sys.setenv(TZ='GMT')
options(shiny.reactlog=TRUE)
options(useFancyQuotes = FALSE)
library(shinytdmore)

rxModel <- RxODE::RxODE("
KA = 0.3;
TVCL = 10;
TVV = 5;

CL = TVCL * (WT/70)^0.75 * exp(ECL);
V = TVV * (WT/70) * exp(EV);

d/dt(A0) = -KA*A0;
d/dt(A1) = KA*A0 - CL/V*A1;
CONC = A1 / V * 1000;
")
tdmModel <- tdmore::tdmore(rxModel,
  omega=c(ECL=0.3, EV=0.5),
  res_var=list(tdmore::errorModel(prop=0.3))
)
myModel <- tdmore::metadata(tdmModel,
  tdmore::target(min=10, max=15),
  tdmore::covariate("WT", "Weight", "kg", min=20, max=150),
  tdmore::formulation(name="Prograft", unit="mg", dosing_interval=12, default_value=5, round_function=identity),
  tdmore::formulation(name="Advagraf", unit="mg", dosing_interval=24, default_value=5, round_function=identity)
)

ui <- fluidPage(
  predictionTabUI("prediction")
)

shinyApp(ui=ui, server=function(input, output, session) {
  state <- reactiveValues()
  #state$target <- list(min=12, max=15)
  state$model <- myModel #specify a model
  state$now <- as.POSIXct("2000-01-01 12:00", tz="GMT")
  #state$covariates <- tibble::tibble(time=as.POSIXct("1999-12-15 12:00"), WT=70)
  # state$now <- as.POSIXct("2000-01-01 12:00")
  # state$regimen <- tibble(time=as.POSIXct("1999-12-15 12:00"),
  #                                         dose=15,
  #                                         formulation=1, fix=FALSE
  # )
  callModule(predictionTab, "prediction", state=state)
  exportTestValues(regimen = { state$regimen }, observed= {state$observed})
})
