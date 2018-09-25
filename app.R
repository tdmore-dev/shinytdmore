## Author: Ruben Faelens
## Goal: Quick and dirty implementation of a GUI

## Structure:
## A) Select patient
## B) Select drug
## C) Enter dosage history
## D) Enter covariates
## E) Measures
## F) Targets
## G) Adjustments
## H) Validation (? skip?)
## I) Reports

## https://github.com/jrowen/rhandsontable/
## https://github.com/yonicd/rpdf
## https://github.com/daattali/ddpcr --> Very nice shiny app, can learn something from this!
## Busy indicator: https://github.com/daattali/advanced-shiny/tree/master/busy-indicator
## Text in the navbar: https://github.com/daattali/advanced-shiny/blob/master/navbar-add-text/app.R 

library(shiny)
library(DT)
library(ggplot2)
library(tidyverse)
library(plotly)
library(crosstalk)

patients <- data.frame(
  Name="Faelens, Ruben",
  Admitted="2018/06/25"
)
dosesList <- list(
  data.frame(
    date=c("2018/06/25","2018/06/25","2018/06/26","2018/06/26", "2018/06/27"),
    time=c("08:00", "20:00","08:00", "20:00", "08:00"),
    dose=c(6, 6, 7, 7, 7)
    ) %>% transmute(t=as.POSIXct(paste(date, time)), dose)
)
measuresList <- list(
  data.frame(
    date=c("2018/06/26","2018/06/27"),
    time=c("07:30", "07:23"),
    measure=c(3.1, 5.3)
  )
)

library(tdmore)
rxModel <- RxODE::RxODE('
#Holford model
# TODO: covariate effects

KA = 1.01;
CL= 16.1*exp(0.40*ECL);
V1= 125 * exp(0.54*EV1);
Q=23.8 * exp(0.63*EQ);
V2=636;
TLag=0.41;   # no IOV or IIV; TODO needs to be included
F=1 * exp(0.57*EFDay2); # TODO: code "increase after day 2"
# TODO: include IIV correlations CL-V1 of 0.43, CL-Q of 0.62
# TODO: include IOV of 23% on F, 120% on Ka
# TODO: covariate effects

Ke=CL/V1;
K12=Q/V1;
K21=Q/V2;

CONC=CENTR/V1 * 1000;

d/dt(ABS) = -KA*ABS;
d/dt(CENTR) = KA*ABS - K12*CENTR + K21*PERIP - Ke*CENTR;
d/dt(PERIP) = K12*CENTR - K21*PERIP;
')
model <- tdmore(rxModel, prop=0.149)

ui <- navbarPage("TDMore mockup",
                tabPanel("Patient", icon=icon("users"),
                         titlePanel("Patient"),
                         DTOutput('patientTable')
                         ),
                tabPanel("Doses", icon=icon("medkit"),
                         titlePanel("Doses"),
                         fluidRow(
                           column(4, DTOutput('dosingTable'),
                                  HTML("<b>These values are filled in from the KWS and cannot be modified</b>")),
                          column(8, plotlyOutput('dosingPlot'))
                         )
                      ),
                 tabPanel("Covariates", icon=icon("address-card"),
                          titlePanel("Covariates"),
                          fluidRow(
                            column(4,
                                   DTOutput('covariatesTable'),
                                   HTML("<b>These values are filled in from the KWS and cannot be modified</b>")
                                   ),
                            column(8, plotlyOutput('covariatesPlot'))
                            )),
                 tabPanel("Measures", icon=icon("stethoscope"),
                          column(4, 
                                 DTOutput('measuresTable'),
                                 HTML("<b>These values are filled in from the KWS and cannot be modified</b>")
                          ),
                          column(8, plotlyOutput('measuresPlot'))
                          ),
                 tabPanel("Targets", icon=icon("bullseye"),
                          column(4, 
                                 HTML("Targets for Tacrolimus: <ul>
                                      <li>First 14 days post-transplant: 10-15 ug/L</li>
                                      <li>From 14 days to 3 months: 10-12 ug/L</li>
                                      <li>From 3 months onwards: 8-12 ug/L</li>
                                      </ul>"),
                                 HTML("<b>Question: Do we use a fixed rule? In the software, or in the KWS?</b>")
                          ),
                          column(8, plotlyOutput('targetsPlot'))
                          ),
                 tabPanel("Adjustments", icon=icon("user-md"),
                          column(4,
                                 HTML("A dose of XXmg BID is recommended"),
                                 HTML("<b>TODO: We need to show that our previous adjustments were right! Some kind of check 'what did we suggest' versus 'what did you do?'</b>")
                                 ),
                          column(8,
                                 plotlyOutput('adjustmentsPlot'),
                                 HTML("<b>TODO: Include a table / numeric prediction of the trough?</b>")
                                 )
                          ),
                 tabPanel("Reports", icon=icon("file-text")),
                 tabPanel("About", icon=icon("question"),
                          HTML("
This is a demo application to show the possibilities of TDMore.<br/>
The user interface can of course be adapted as required. We may even provide an API to transfer data. As an example:
<p><b>Pull/push initiated by TDM app:</b> The TDM application runs
as a client application. It polls Wintermute at specific timepoints,
downloads the required data (patient information, patient covariates, dosing history and
concentration samples), calculates the optimal next dose (and associated graphs and report),
and automatically sends this back to Wintermute.</p>
<p>Many alternatives for this 'upload results' exist:
<ol>
<li>Upload only the dose recommendation to Wintermute</li>
<li>Upload only a warning to Wintermute 'New dosing recommendation available', 
with an associated ID. From the KWS, doctors can click the link and connect to
a web app to further analyze the dosing recommendation.</li>
<li>

<p><b>Pull/push initiated by Wintermute:
                               ")
                 ),
                 inverse=TRUE,
                id="menu",
                collapsible = TRUE
)

server <- function(input, output, session) {
  # Patient selection
  output$patientTable <- renderDT(
    patients, options = list(info=FALSE, paging = FALSE, scrollX=FALSE, scrollY=FALSE, select="single")
  )
  observeEvent(input$patientTable_rows_selected, {
    updateTabsetPanel(session, "menu", selected="Doses")
  })
  patientId <- reactive({
     i <- input$patientTable_rows_selected
     i
  })
  
  # Doses
  # dosing <- reactive({  
  #   dosesList[[patientId()]] %>%
  #     transmute(t=as.POSIXct(paste(date, time)),
  #               dose)
  # })
  dosing <- SharedData$new(reactive({dosesList[[patientId()]]}))
  # https://rstudio.github.io/crosstalk/authoring.html
  # To link the hover state in DT with the selection state of crosstalk, we will need some manual adjustment
  # See the link above
  output$dosingTable <- renderDT( dosing %>%
                                    datatable(
                                      class="compact",
                                      rownames=FALSE,
                                      colnames=c('Time'='t', 'Dose (mg)'='dose'),
                                      selection='single',
                                    options = list(
                                      paging = FALSE,
                                      ordering=FALSE,
                                      info=FALSE,
                                      scrollX=FALSE, 
                                      scrollY=FALSE,
                                      searching=FALSE
                                    )) %>%
                                    formatDate("Time", method="toLocaleString", params=list('nl', list( weekday='short', month='numeric', day='numeric', hour='numeric', minute='numeric' )))
                                  , server=FALSE)
  #output$dosingTable <- renderDT( datatable(dosing), server=FALSE)
  output$dosingPlot <- renderPlotly({
    db <- dosesList[[1]]
    start <- min(db$t)
    stop <- max(db$t + 24*60*60)
    regimen <- data.frame(
      TIME=as.numeric(difftime(db$t, start, units="hour")),
      AMT=db$dose
    )
    pred <- predict(
      model,
      newdata = data.frame(TIME=seq(0, max(regimen$TIME)+24, length.out=300), CONC=NA),
      regimen=regimen,
      se=TRUE) %>% as.data.frame()
    pred$TIME <- min(db$t) + pred$TIME*60*60
    z1 <- ggplot(pred, aes(x=TIME)) +
      geom_line(aes(y=CONC)) +
      geom_ribbon(aes(ymin=CONC.lower, ymax=CONC.upper), alpha=0.3) +
      labs(title="Population prediction", x="", y="Concentration (ug/L)", size="Dose (mg)")
    ggplotly(z1) %>%
      config(scrollZoom=TRUE, collaborate=FALSE, displayModeBar=FALSE, displaylogo=FALSE) #%>%
      #add_annotations(x=~t, y=~dose, text="Dose", data=dosing) %>%
      #add_markers(x=~t, y=0, color=I("black"), size=~dose, data=dosing) %>%
      #highlight(on="plotly_hover")
    
    plot_ly(pred, x = ~TIME, y = ~CONC, type = 'scatter', mode = 'lines', color=I("black")) %>%
       add_ribbons(x=~TIME, ymin=~CONC.lower, ymax=~CONC.upper) %>%
       add_markers(x=~t, y=0, color=I("black"), size=~dose, data=dosing) %>%
       highlight(on="plotly_hover", 
                 color="red", 
                 persistent=FALSE, 
                 dynamic = FALSE) %>%
      config(scrollZoom=TRUE, collaborate=FALSE, displayModeBar=FALSE, displaylogo=FALSE)
  })
  
  predCov <- reactive({
    db <- dosing$data(withFilter=FALSE)
    db$selected <- FALSE
    if(!is.null(input$hoverIndexJS))
      db$selected[input$hoverIndexJS + 1] <- TRUE
    start <- min(db$t)
    stop <- max(db$t + 24*60*60)
    regimen <- data.frame(
      TIME=as.numeric(difftime(db$t, start, units="hour")),
      AMT=db$dose
    )
    pred <- predict(
      model, 
      newdata = data.frame(TIME=seq(0, max(regimen$TIME)+24, length.out=300), CONC=NA), 
      regimen=regimen, 
      se=TRUE) %>% as.data.frame()
    pred$TIME <- min(db$t) + pred$TIME*60*60
    
    pred
  })
  
  output$covariatesPlot <- renderPlotly({
    db <- dosing$data(withFilter=FALSE)
    pred <- predCov()
    p1 <- ggplot(db) +
      geom_point(aes(x=t, y=0, size=factor(dose))) +
      geom_line(aes(x=TIME, y=CONC), data=pred) +
      geom_ribbon(aes(x=TIME, ymin=CONC.lower, ymax=CONC.upper), data=pred, alpha=0.3) +
      labs(title="A priori prediction", caption="Same as population prediction, as no cov effects in model yet")
    ggplotly(p1) %>%
      config(scrollZoom=TRUE, collaborate=FALSE, displayModeBar=FALSE, displaylogo=FALSE)
  })
  patientId <- reactive({1})
  measures <- reactive({  
    db <- measuresList[[patientId()]]
    db$t <- as.POSIXct(paste(db$date, db$time))
    db
  })
  
  output$measuresTable <- renderDT( measures(), 
                                  options = list(
                                    paging = FALSE,
                                    ordering=FALSE,
                                    info=FALSE,
                                    scrollX=FALSE, 
                                    scrollY=FALSE,
                                    select="none",
                                    selection='none',
                                    searching=FALSE
                                    ) )
  output$measuresPlot <- renderPlotly({
    db <- dosing$data(withFilter=FALSE)
    start <- min(db$t)
    stop <- max(db$t + 24*60*60)
    regimen <- data.frame(
      TIME=as.numeric(difftime(db$t, start, units="hour")),
      AMT=db$dose
    )
    ### TODO: fix scale
    ### TODO: fix IIV band
    pred <- predict(
      model, 
      newdata = data.frame(TIME=seq(0, max(regimen$TIME)+24, length.out=300), CONC=NA), 
      regimen=regimen, 
      se=TRUE)
    pred$TIME <- min(db$t) + pred$TIME*60*60
    
    tdm <- measures()
    observed <- data.frame(
      TIME=as.numeric(difftime(tdm$t, start, units="hour")),
      CONC=tdm$measure
      )
    #debugonce(tdmore:::predict.tdmore)
    fit <- estimate(model, observed=observed, regimen=regimen)
    ipred <- fit %>%
      predict(newdata = data.frame(TIME=seq(0, max(regimen$TIME)+24, length.out=300), CONC=NA), 
              regimen=regimen, 
              se=TRUE)
    ipred$TIME <- min(db$t) + ipred$TIME*60*60
    
    p1 <- ggplot(db) +
      geom_point(aes(x=t, y=0, size=factor(dose))) +
      geom_line(aes(x=TIME, y=CONC, color="Population"), data=pred) +
      geom_ribbon(aes(x=TIME, ymin=CONC.lower, ymax=CONC.upper, fill="Population"), data=pred, alpha=0.3) +
      geom_point(aes(x=t, y=measure, color="Samples"), data=tdm)+
      geom_text(aes(x=t, y=measure, label=measure), data=tdm, nudge_x=+30, hjust=0) +
      geom_line(aes(x=TIME, y=CONC, color="Individual"), data=ipred) +
      geom_ribbon(aes(x=TIME, ymin=CONC.lower, ymax=CONC.upper, fill="Individual"), data=ipred, alpha=0.3) +
      labs(title="Individual prediction", fill="Prediction", size="Dose (mg)", color="Prediction")
    ggplotly(p1) %>%
      config(scrollZoom=TRUE, collaborate=FALSE, displayModeBar=FALSE, displaylogo=FALSE)
  })
  
  
  
  output$targetsPlot <- renderPlotly({
    db <- dosing$data(withFilter=FALSE)
    start <- min(db$t)
    stop <- max(db$t + 24*60*60)
    regimen <- data.frame(
      TIME=as.numeric(difftime(db$t, start, units="hour")),
      AMT=db$dose
    )
    pred <- predict(
      model, 
      newdata = data.frame(TIME=seq(0, max(regimen$TIME)+24, length.out=300), CONC=NA), 
      regimen=regimen, 
      se=TRUE)
    pred$TIME <- min(db$t) + pred$TIME*60*60
    
    tdm <- measures()
    observed <- data.frame(
      TIME=as.numeric(difftime(tdm$t, start, units="hour")),
      CONC=tdm$measure
    )
    fit <- estimate(model, observed=observed, regimen=regimen)
    ipred <- fit %>%
      predict(newdata = data.frame(TIME=seq(0, max(regimen$TIME)+24, length.out=300), CONC=NA), 
              regimen=regimen, 
              se=TRUE)
    ipred$TIME <- min(db$t) + ipred$TIME*60*60
    
    targets <- data.frame(
      t1=start,
      t2=stop,
      lower=10,
      upper=15
      )
    
    p1 <- ggplot(db) +
      geom_point(aes(x=t, y=0, size=factor(dose))) +
      geom_line(aes(x=TIME, y=CONC, color="Population"), data=pred) +
      geom_ribbon(aes(x=TIME, ymin=CONC.lower, ymax=CONC.upper, fill="Population"), data=pred, alpha=0.3) +
      geom_point(aes(x=t, y=measure, color="Samples"), data=tdm)+
      geom_text(aes(x=t, y=measure, label=measure), data=tdm, nudge_x=+30, hjust=0) +
      geom_line(aes(x=TIME, y=CONC, color="Individual"), data=ipred) +
      geom_ribbon(aes(x=TIME, ymin=CONC.lower, ymax=CONC.upper, fill="Individual"), data=ipred, alpha=0.3) +
      labs(title="Individual prediction", fill="Prediction", size="Dose (mg)", color="Prediction") +
      geom_rect(data=targets, aes(xmin=t1, xmax=t2, ymin=lower, ymax=upper, fill="Target"), alpha=0.3)
    ggplotly(p1) %>%
      config(scrollZoom=TRUE, collaborate=FALSE, displayModeBar=FALSE, displaylogo=FALSE)
  })
  
  
  
  
  output$adjustmentsPlot <- renderPlotly({
    db <- dosing$data(withFilter=FALSE)
    start <- min(db$t)
    stop <- max(db$t + 24*60*60)
    regimen <- data.frame(
      TIME=as.numeric(difftime(db$t, start, units="hour")),
      AMT=db$dose
    )
    
    ## We want to show:
    ## 1) What we did so far
    ## 2) Where we want to go in the future
    tdm <- measures()
    observed <- data.frame(
      TIME=as.numeric(difftime(tdm$t, start, units="hour")),
      CONC=tdm$measure
    )
    fit <- estimate(model, observed=observed, regimen=regimen)
    ipred <- fit %>%
      predict(newdata = data.frame(TIME=seq(0, max(regimen$TIME)+24, length.out=300), CONC=NA), 
              regimen=regimen, 
              se=FALSE)
    ipred$TIME <- min(db$t) + ipred$TIME*60*60
    targets <- data.frame(
      t1=start,
      t2=start + 4*24*60*60,
      lower=10,
      upper=15
    )
    lastDose <- max(regimen$TIME)
    nextDose <- lastDose + 12
    Target <- data.frame(TIME=lastDose+48, CONC=10) #trough after next dose
    
    newRegimen <- data.frame(
      TIME=c(regimen$TIME, lastDose+c(12,24,36,48)),
      AMT=c(regimen$AMT, c(NA,NA,NA,NA))
      )
    root <- tdmore::findDose(fit, doseRows=which(is.na(newRegimen$AMT)),regimen = newRegimen, target = Target)
    #browser()
    newRegimen$AMT[ is.na(newRegimen$AMT)] <- root$root
    ipredNew <- fit %>%
      predict(newdata = data.frame(TIME=seq(max(regimen$TIME), max(newRegimen$TIME)+12, length.out=300), CONC=NA), 
              regimen=newRegimen, 
              se=TRUE) %>%
      mutate(TIME = min(db$t) + TIME*60*60)
    #stop(newRegimen)
    p1 <- ggplot(db) +
      geom_point(aes(x=t, y=0, size=factor(dose))) +
      geom_point(aes(x=t, y=measure, color="Samples"), data=tdm)+
      geom_text(aes(x=t, y=measure, label=measure), data=tdm, nudge_x=+30, hjust=0) +
      geom_line(aes(x=TIME, y=CONC, color="Individual"), data=ipred) +
      geom_line(aes(x=TIME, y=CONC, color="Recommendation"), data=ipredNew) +
      geom_ribbon(aes(x=TIME, ymin=CONC.lower, ymax=CONC.upper, fill="Recommendation"), data=ipredNew, alpha=0.3) +
      labs(title="Individual prediction", fill="Prediction", size="Dose (mg)", color="Prediction") +
      geom_rect(data=targets, aes(xmin=t1, xmax=t2, ymin=lower, ymax=upper, fill="Target"), alpha=0.3)
    p1
  })
  
  
  }

# Run the application 
shinyApp(ui = ui, server = server)
