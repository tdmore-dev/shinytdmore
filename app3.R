## Author: Ruben Faelens & Quentin Leirens
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
library(shinyBS)
library(rhandsontable)
library(lubridate)
library(shinyTime)

str1 <- paste0("0",as.character(seq(0,23,1)))
v1 <- substr(str1,(nchar(str1)+1)-2,nchar(str1))
str2 <- paste0("0",as.character(seq(0,45,15)))
v2 <- paste0(":",substr(str2,(nchar(str2)+1)-2,nchar(str2)))
HOURS <- sort(apply(expand.grid(v1, v2), 1, paste, collapse = "", sep = "")) 

patients <- data.frame(
  Name="Faelens, Ruben",
  Admitted="2018/06/25"
)
dosesList <- list(
  data.frame(
    date=c("2018/06/25","2018/06/25","2018/06/26","2018/06/26", "2018/06/27"),
    time=c("08:00", "20:00","08:00", "20:00", "08:00"),
    dose=c(6, 6, 7, 7, 7)
    ) %>% transmute(date=as.Date(date), time, dose)
)
measuresList <- list(
  data.frame(
    date=c("2018/06/26","2018/06/27"),
    time=c("08:00", "08:00"),
    measure=c(3.1, 5.3)
  ) %>% transmute(date=as.Date(date), time, measure)
)

library(tdmore)
rxModel <- RxODE::RxODE('
#Holford model
# TODO: covariate effects

KA = 1.01;
CL= 16.1*exp(ECL);
V1= 125 * exp(EV1);
Q=23.8 * exp(EQ);
V2=636;
TLag=0.41;   # no IOV or IIV; TODO needs to be included
F=1 * exp(EFDay2); # TODO: code "increase after day 2"
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
omegas=c(ECL=0.40^2, EV1=0.54^2, EQ=0.63^2, EFDay2=0.57^2)
model <- tdmore(rxModel, omega=omegas, res_var=list(errorModel("CONC", prop=0.149)))


ui <- navbarPage("TDMore mockup",
                tabPanel("Patient", icon=icon("users"),
                         titlePanel("Patient"),
                         DTOutput('patientTable')
                         ),
                 tabPanel("Prediction", icon=icon("address-card"),
                          titlePanel("Data"),
                          fluidRow(
                            column(4,
                                   conditionalPanel(
                                     condition = "input.statusValue == 'check' || input.statusValue == 'valid'",
                                     h3("Observation history"),
                                     rHandsontableOutput('hotobs'),
                                     actionButton("editObs", "Add a measure"),
                                     
                                     h3("Dosing history"),
                                     rHandsontableOutput('hotdose'),
                                     actionButton("editDose", "Add a dose")
                                   ),

                                   selectInput("statusValue", "Status",
                                               c("Check data history" = "check",
                                                 "Validate and see prediction" = "valid",
                                                 "Computer recommendation" = "rec")),
                                   
                                   conditionalPanel(
                                     condition = "input.statusValue == 'rec'",
                                     h3("Dosing future"),
                                     dataTableOutput('hotdosefuture')
                                     #actionButton("getComputerPrediction", "Computer prediction")
                                   )
                            ),
                            column(8, 
                                   conditionalPanel(
                                     condition = "input.statusValue == 'check'",
                                     plotlyOutput('populationPlot',height="600px")
                                     ),
                                   
                                   conditionalPanel(
                                     condition = "input.statusValue == 'valid'",
                                     plotlyOutput('predictionPlot',height="600px")
                                   ),
                
                                   conditionalPanel(
                                     condition = "input.statusValue == 'rec'",
                                     plotlyOutput('adjustmentsPlot',height="600px")
                                   ),
                                   h4("Target range:"),
                                   numericInput("targetDown", "Lower limit", 10),
                                   numericInput("targetUp", "Upper limit", 15)
                                   )
                            
                            )),
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
    updateTabsetPanel(session, "menu", selected="Prediction")
  })
  patientId <- reactive({
     i <- input$patientTable_rows_selected
     i
  })
  
  output$hover <- renderPrint({
    d <- event_data("plotly_hover")
    if (is.null(d)) "Hover events appear here (unhover to clear)" else d
  })
  
  #dosing <- reactive({dosesList[[patientId()]]})

  db_dose <- data.frame(dosesList[[1]])
  pos <- as.POSIXct(strptime(paste(db_dose$date,db_dose$time),format = "%Y-%m-%d %H:%M"))
  start <- min(pos)
  stop <- max(pos + 24*60*60)
  regimen <- data.frame(
    TIME=as.numeric(difftime(pos, start, units="hour")),
    AMT=db_dose$dose
  )
  
  db_obs <- data.frame(measuresList[[1]])
  db_obs$use <- TRUE
  
  db_dosef <- data.frame(dosesList[[1]])
  pos <- as.POSIXct(strptime(paste(db_dose$date,db_dose$time),format = "%Y-%m-%d %H:%M"))
  start <- min(pos)
  stop <- max(pos + 24*60*60)
  regimen <- data.frame(
    TIME=as.numeric(difftime(pos, start, units="hour")),
    AMT=db_dose$dose
  )
   
  pred <- predict(
    model,
    newdata = data.frame(TIME=seq(0, max(regimen$TIME)+24, length.out=300), CONC=NA),
    regimen=regimen,
    se=TRUE) %>% as.data.frame()
  pred$TIME <- min(pos) + pred$TIME*60*60
  
  # Output for tab "Prediction"
  
  values <- reactiveValues()
  
  # Handsontable Dose
  observe({
    if (!is.null(input$hotdose)) {
      #values[["previous"]] <- isolate(values[["db_dose"]])
      db_dose = hot_to_r(input$hotdose)
    } else {
      if (is.null(values[["db_dose"]]))
        db_dose <- db_dose
      else
        db_dose <- values[["db_dose"]]
    }
    values[["db_dose"]] <- db_dose
  })
  
  output$hotdose <- renderRHandsontable({
    db_dose <- values[["db_dose"]]
    if (!is.null(db_dose))
      rhandsontable(data.frame(db_dose), useTypes = TRUE, stretchH = "all") %>%
      hot_context_menu(allowRowEdit = FALSE, allowColEdit = FALSE) %>%
      hot_col(col = "time", type = "dropdown", source = HOURS)
  }) 

  dataModalDose <- function(failed = FALSE) {
    modalDialog(
      numericInput("newdose", "Dose", 0),
      textInput("newtimedose", "Time", "08:00"),
      #timeInput("time_input", "Enter time", value = strptime("08:00:00", "%T")),
      dateInput("newdatedose", "Date", tail(db_dose,n=1)$date, format = "yyyy/mm/dd"),
      tags$style(
        type = "text/css", ".datepicker{z-index: 1100 !important;}"),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("okDose", "OK")
      )
    )
  }
  
  observeEvent(input$editDose, {
    showModal(dataModalDose())
  })
  
  observeEvent(input$okDose, {
    # Check that data object exists and is data frame.
    if (!is.null(input$newdose) & !is.null(input$newtimedose) & !is.null(input$newdatedose)) {
      db_dose <- isolate(values[["db_dose"]])
      newrow <- tail(db_dose,n=1)
      row.names(newrow) <- as.numeric(row.names(newrow))+1
      newrow$dose <- isolate(input$newdose)
      newrow$date <- isolate(input$newdatedose)
      newrow$time <- isolate(input$newtimedose)
      values[["db_dose"]] <- rbind(db_dose,newrow)
      removeModal()
    } else {
      showModal(dataModal(failed = TRUE))
    }
  })
  
  # Handsontable Obs
  observe({
    if (!is.null(input$hotobs)) {
      #values[["previous"]] <- isolate(values[["db_obs"]])
      db_obs = hot_to_r(input$hotobs)
    } else {
      if (is.null(values[["db_obs"]]))
        db_obs <- db_obs
      else
        db_obs <- values[["db_obs"]]
    }
    values[["db_obs"]] <- db_obs
  })
  
  output$hotobs <- renderRHandsontable({
    db_obs <- values[["db_obs"]]
    if (!is.null(db_obs))
      rhandsontable(data.frame(db_obs), useTypes = TRUE, stretchH = "all")
  })
  
  dataModalObs <- function(failed = FALSE) {
    modalDialog(
      numericInput("newobs", "Measure", 0),
      textInput("newtimeobs", "Time", "08:00"),
      #timeInput("time_input", "Enter time", value = strptime("08:00:00", "%T")),
      dateInput("newdateobs", "Date", tail(db_dose,n=1)$date, format = "yyyy/mm/dd"),
      tags$style(
        type = "text/css", ".datepicker{z-index: 1100 !important;}"),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("okObs", "OK")
      )
    )
  }
  
  observeEvent(input$editObs, {
    showModal(dataModalObs())
  })
  
  observeEvent(input$okObs, {
    # Check that data object exists and is data frame.
    if (!is.null(input$newobs) & !is.null(input$newtimeobs) & !is.null(input$newdateobs)) {
      db_obs <- isolate(values[["db_obs"]])
      newrow <- tail(db_obs,n=1)
      row.names(newrow) <- as.numeric(row.names(newrow))+1
      newrow$measure <- isolate(input$newobs)
      newrow$date <- isolate(input$newdateobs)
      newrow$time <- isolate(input$newtimeobs)
      newrow$use <- TRUE
      values[["db_obs"]] <- rbind(db_obs,newrow)
      removeModal()
    } else {
      showModal(dataModal(failed = TRUE))
    }
  })
  

  #Plots
  # 1 - Population check
  
  output$populationPlot <- renderPlotly({
    db_dose <- values[["db_dose"]]
    db_obs <- values[["db_obs"]]
    db_obs_filtered <-  db_obs %>% filter(use==TRUE)
    if (!is.null(db_dose) | !is.null(db_obs_filtered)) {
      pos <- as.POSIXct(strptime(paste(db_dose$date,db_dose$time),format = "%Y-%m-%d %H:%M"))
      start <- min(pos)
      stop <- max(pos + 24*60*60)
      regimen <- data.frame(
        TIME=as.numeric(difftime(pos, start, units="hour")),
        AMT=db_dose$dose
      )
      
      pred <- predict(
        model,
        newdata = data.frame(TIME=seq(0, max(regimen$TIME)+24, length.out=300), CONC=NA),
        regimen=regimen,
        se=TRUE) %>% as.data.frame()
      pred$TIME <- min(pos) + pred$TIME*60*60
      
      p1 <- ggplot(pred) +
        geom_line(aes(x=TIME, y=CONC, color="Population"), data=pred) +
        geom_ribbon(aes(x=TIME, ymin=CONC.lower, ymax=CONC.upper, fill="Population"), data=pred, alpha=0.3) +
        geom_point(aes(x=as.POSIXct(strptime(paste(db_obs_filtered$date,db_obs_filtered$time),format = "%Y-%m-%d %H:%M")), y=measure, color="Samples"),shape=4, size=3, data=db_obs_filtered)+
        geom_text(data=db_obs_filtered,aes(x=as.POSIXct(strptime(paste(db_obs_filtered$date,db_obs_filtered$time),format = "%Y-%m-%d %H:%M")), y=measure,label=measure, color="Samples"), check_overlap = T, show.legend = TRUE) +
        labs(title="Population prediction", y="Concentration (mg/L)")+
        scale_fill_discrete(guide=FALSE)+
        scale_colour_discrete(name="Data",
                              breaks=c("Population","Individual", "Samples"),
                              labels=c("Population","Individual", "Samples"))
      
      p2 <- ggplot(db_dose,aes(x=as.POSIXct(strptime(paste(db_dose$date,db_dose$time),format = "%Y-%m-%d %H:%M")), y=dose))+
        geom_text(aes(x=as.POSIXct(strptime(paste(db_dose$date,db_dose$time),format = "%Y-%m-%d %H:%M")), y=dose,label=dose), nudge_x = 0, nudge_y = 0, check_overlap = T, show.legend = FALSE) +
        geom_linerange(ymin=0, aes(ymax=dose))+
        coord_cartesian(xlim = c(min(pos),max(pred$TIME)), ylim = c(0,max(db_dose$dose)+2, expand = FALSE)) +
        labs(x="Time", y="Dose (mg)")
      
      subplot(
        ggplotly(p1) %>% config(scrollZoom=TRUE, collaborate=FALSE, displayModeBar=FALSE, displaylogo=FALSE), 
        ggplotly(p2) %>% config(scrollZoom=TRUE, collaborate=FALSE, displayModeBar=FALSE, displaylogo=FALSE), 
        nrows = 2, heights = c(0.8, 0.2), widths = c(1), 
        shareX = TRUE, shareY = FALSE, titleX = TRUE, titleY = TRUE
      )}
  })
  
  
  # 2 - Get prediction
  output$predictionPlot <- renderPlotly({
    db_dose <- values[["db_dose"]]
    db_obs <- values[["db_obs"]]
    db_obs_filtered <-  db_obs %>% filter(use==TRUE)
    if (!is.null(db_dose) | !is.null(db_obs_filtered)) {
      pos <- as.POSIXct(strptime(paste(db_dose$date,db_dose$time),format = "%Y-%m-%d %H:%M"))
      start <- min(pos)
      stop <- max(pos + 24*60*60)
      regimen <- data.frame(
        TIME=as.numeric(difftime(pos, start, units="hour")),
        AMT=db_dose$dose
      )
      
      pred <- predict(
        model,
        newdata = data.frame(TIME=seq(0, max(regimen$TIME)+24, length.out=300), CONC=NA),
        regimen=regimen,
        se=TRUE) %>% as.data.frame()
      pred$TIME <- min(pos) + pred$TIME*60*60
      
      
      pos2 <- as.POSIXct(strptime(paste(db_obs_filtered$date,db_obs_filtered$time),format = "%Y-%m-%d %H:%M"))
      observed <- data.frame(
        TIME=as.numeric(difftime(pos2, start, units="hour")),
        CONC=db_obs_filtered$measure 
      )
      #debugonce(tdmore:::predict.tdmore)
      fit <- estimate(model, observed=observed, regimen=regimen)
      ipred <- fit %>%
        predict(newdata = data.frame(TIME=seq(0, max(regimen$TIME)+24, length.out=300), CONC=NA), 
                regimen=regimen, 
                se=TRUE)
      ipred$TIME <- start + ipred$TIME*60*60
     
       p1 <- ggplot(pred) +
        geom_line(aes(x=TIME, y=CONC, color="Individual"), data=ipred) +
        geom_ribbon(aes(x=TIME, ymin=CONC.lower, ymax=CONC.upper, fill="Individual"), data=ipred, alpha=0.3) +
        geom_point(aes(x=as.POSIXct(strptime(paste(db_obs_filtered$date,db_obs_filtered$time),format = "%Y-%m-%d %H:%M")), y=measure, color="Samples"),shape=4, size=3, data=db_obs_filtered)+
        geom_text(data=db_obs_filtered,aes(x=as.POSIXct(strptime(paste(db_obs_filtered$date,db_obs_filtered$time),format = "%Y-%m-%d %H:%M")), y=measure,label=measure, color="Samples"), check_overlap = T, show.legend = TRUE) +
        labs(title="Individual prediction", y="Concentration (mg/L)")+
        scale_fill_discrete(guide=FALSE)+
        scale_colour_discrete(name="Data",
                             breaks=c("Population","Individual", "Samples"),
                             labels=c("Population","Individual", "Samples"))
     
     p2 <- ggplot(db_dose,aes(x=as.POSIXct(strptime(paste(db_dose$date,db_dose$time),format = "%Y-%m-%d %H:%M")), y=dose))+
            geom_text(aes(x=as.POSIXct(strptime(paste(db_dose$date,db_dose$time),format = "%Y-%m-%d %H:%M")), y=dose,label=dose), nudge_x = 0, nudge_y = 0, check_overlap = T, show.legend = FALSE) +
            geom_linerange(ymin=0, aes(ymax=dose))+
            coord_cartesian(xlim = c(min(pos),max(pred$TIME)), ylim = c(0,max(db_dose$dose)+2, expand = FALSE)) +
            labs(x="Time", y="Dose (mg)")
     
     subplot(
       ggplotly(p1) %>% config(scrollZoom=TRUE, collaborate=FALSE, displayModeBar=FALSE, displaylogo=FALSE), 
       ggplotly(p2) %>% config(scrollZoom=TRUE, collaborate=FALSE, displayModeBar=FALSE, displaylogo=FALSE), 
       nrows = 2, heights = c(0.8, 0.2), widths = c(1), 
       shareX = TRUE, shareY = FALSE, titleX = TRUE, titleY = TRUE
     )}
   })
  
  
  patientId <- reactive({1})
  measures <- reactive({  
    db_dose <- measuresList[[patientId()]]
    db_dose$t <- as.POSIXct(paste(db_dose$date, db_dose$time))
    db_dose
  })
  
  # 3 - Get computer recommendation
  
  output$adjustmentsPlot <- renderPlotly({
    db_dose <- values[["db_dose"]]
    db_obs <- values[["db_obs"]]
    db_obs_filtered <-  db_obs %>% filter(use==TRUE)
    if (!is.null(db_dose) | !is.null(db_obs_filtered)) {
      pos <- as.POSIXct(strptime(paste(db_dose$date,db_dose$time),format = "%Y-%m-%d %H:%M"))
      start <- min(pos)
      stop <- max(pos + 24*60*60)
      regimen <- data.frame(
        TIME=as.numeric(difftime(pos, start, units="hour")),
        AMT=db_dose$dose
      )
      
      pred <- predict(
        model,
        newdata = data.frame(TIME=seq(0, max(regimen$TIME)+24, length.out=300), CONC=NA),
        regimen=regimen,
        se=TRUE) %>% as.data.frame()
      pred$TIME <- min(pos) + pred$TIME*60*60
      
      
      pos2 <- as.POSIXct(strptime(paste(db_obs_filtered$date,db_obs_filtered$time),format = "%Y-%m-%d %H:%M"))
      observed <- data.frame(
        TIME=as.numeric(difftime(pos2, start, units="hour")),
        CONC=db_obs_filtered$measure 
      )
      #debugonce(tdmore:::predict.tdmore)
      fit <- estimate(model, observed=observed, regimen=regimen)
      ipred <- fit %>%
        predict(newdata = data.frame(TIME=seq(0, max(regimen$TIME)+24, length.out=300), CONC=NA), 
                regimen=regimen, 
                se=TRUE)
      ipred$TIME <- start + ipred$TIME*60*60
      
      targets <- data.frame(
        t1=start,
        t2=start + 4*24*60*60,
        lower=input$targetDown,
        upper=input$targetUp
      )
      lastDose <- max(regimen$TIME)
      nextDose <- lastDose + 12
      Target <- data.frame(TIME=lastDose+48, CONC=10) #trough after next dose
      
      newRegimen <- data.frame(
        TIME=c(regimen$TIME, lastDose+c(12,24,36,48)),
        AMT=c(regimen$AMT, c(NA,NA,NA,NA))
      )
      recommendation <- tdmore::findDose(fit, doseRows=which(is.na(newRegimen$AMT)),regimen = newRegimen, target = Target)

      newRegimen$AMT[ is.na(newRegimen$AMT)] <- recommendation$dose
      ipredNew <- fit %>%
        predict(newdata = data.frame(TIME=seq(max(regimen$TIME), max(newRegimen$TIME)+12, length.out=300), CONC=NA), 
                regimen=newRegimen, 
                se=TRUE) %>%
        mutate(TIME = start + TIME*60*60)
      
      testtt <- recommendation$regimen %>% 
        filter(TIME>lastDose) %>% 
        mutate(TIME = start + TIME*60*60, Dose=round(AMT,digits=2))

      p1 <- ggplot(pred) +
        geom_line(aes(x=TIME, y=CONC, color="Individual"), data=ipred, alpha=0.2) +
        geom_line(aes(x=TIME, y=CONC, color="Recommendation"), data=ipredNew) +
        geom_ribbon(aes(x=TIME, ymin=CONC.lower, ymax=CONC.upper, color="Recommendation", fill="Recommendation"), data=ipredNew, alpha=0.3) +
        geom_point(aes(x=as.POSIXct(strptime(paste(db_obs_filtered$date,db_obs_filtered$time),format = "%Y-%m-%d %H:%M")), y=measure, color="Samples"),shape=4, size=3, data=db_obs_filtered)+
        geom_text(data=db_obs_filtered,aes(x=as.POSIXct(strptime(paste(db_obs_filtered$date,db_obs_filtered$time),format = "%Y-%m-%d %H:%M")), y=measure,label=measure, color="Samples"), check_overlap = T, show.legend = TRUE) +
        geom_hline(data=targets, aes(yintercept=lower, color="Target"), lty=2)+
        geom_hline(data=targets, aes(yintercept=upper, color="Target"), lty=2)+
        labs(title="Recommendations", y="Concentration (mg/L)")+
        scale_fill_discrete(guide=FALSE)+
        scale_colour_discrete(name="Data",
                              breaks=c("Individual","Recommendation", "Samples"),
                              labels=c("Individual","Recommendation", "Samples"))
      
      p2 <- ggplot(db_dose)+
        geom_text(aes(x=as.POSIXct(strptime(paste(db_dose$date,db_dose$time),format = "%Y-%m-%d %H:%M")), y=dose,label=dose), nudge_x = 0, nudge_y = 0, check_overlap = T, show.legend = FALSE, alpha=0.2) +
        geom_linerange(ymin=0, aes(x=as.POSIXct(strptime(paste(db_dose$date,db_dose$time),format = "%Y-%m-%d %H:%M")), ymax=dose), alpha=0.2)+
        geom_text(data=testtt, aes(x=TIME, y=Dose, label=Dose) , nudge_x = 0, nudge_y = 0, check_overlap = T, show.legend = FALSE) +
        geom_linerange(data=testtt, aes(x=TIME,ymax=Dose), ymin=0)+
        coord_cartesian(xlim = c(min(pos),max(c(pred$TIME,testtt$TIME)+12*60*60)), ylim = c(0,max(c(db_dose$dose,testtt$Dose)+2)), expand = FALSE) +
        labs(x="Time", y="Dose (mg)")
      
      subplot(
        ggplotly(p1) %>% config(scrollZoom=TRUE, collaborate=FALSE, displayModeBar=FALSE, displaylogo=FALSE), 
        ggplotly(p2) %>% config(scrollZoom=TRUE, collaborate=FALSE, displayModeBar=FALSE, displaylogo=FALSE), 
        nrows = 2, heights = c(0.8, 0.2), widths = c(1), 
        shareX = TRUE, shareY = FALSE, titleX = TRUE, titleY = TRUE
      )}
  })
  
  # Handsontable Dose Future
  
  
  output$hotdosefuture <- renderDataTable({ 
    db_dose <- values[["db_dose"]]
    db_obs <- values[["db_obs"]]
    db_obs_filtered <-  db_obs %>% filter(use==TRUE)
    if (!is.null(db_dose) | !is.null(db_obs_filtered)) {
      pos <- as.POSIXct(strptime(paste(db_dose$date,db_dose$time),format = "%Y-%m-%d %H:%M"))
      start <- min(pos)
      stop <- max(pos + 24*60*60)
      regimen <- data.frame(
        TIME=as.numeric(difftime(pos, start, units="hour")),
        AMT=db_dose$dose
      )
      
      pred <- predict(
        model,
        newdata = data.frame(TIME=seq(0, max(regimen$TIME)+24, length.out=300), CONC=NA),
        regimen=regimen,
        se=TRUE) %>% as.data.frame()
      pred$TIME <- min(pos) + pred$TIME*60*60
      
      
      pos2 <- as.POSIXct(strptime(paste(db_obs_filtered$date,db_obs_filtered$time),format = "%Y-%m-%d %H:%M"))
      observed <- data.frame(
        TIME=as.numeric(difftime(pos2, start, units="hour")),
        CONC=db_obs_filtered$measure 
      )
      #debugonce(tdmore:::predict.tdmore)
      fit <- estimate(model, observed=observed, regimen=regimen)
      ipred <- fit %>%
        predict(newdata = data.frame(TIME=seq(0, max(regimen$TIME)+24, length.out=300), CONC=NA), 
                regimen=regimen, 
                se=TRUE)
      ipred$TIME <- start + ipred$TIME*60*60
      
      targets <- data.frame(
        t1=start,
        t2=start + 4*24*60*60,
        lower=input$targetDown,
        upper=input$targetUp
      )
      lastDose <- max(regimen$TIME)
      nextDose <- lastDose + 12
      Target <- data.frame(TIME=lastDose+48, CONC=10) #trough after next dose
      
      newRegimen <- data.frame(
        TIME=c(regimen$TIME, lastDose+c(12,24,36,48)),
        AMT=c(regimen$AMT, c(NA,NA,NA,NA))
      )
      recommendation <- tdmore::findDose(fit, doseRows=which(is.na(newRegimen$AMT)),regimen = newRegimen, target = Target)
      
      newRegimen$AMT[ is.na(newRegimen$AMT)] <- recommendation$dose
      ipredNew <- fit %>%
        predict(newdata = data.frame(TIME=seq(max(regimen$TIME), max(newRegimen$TIME)+12, length.out=300), CONC=NA), 
                regimen=newRegimen, 
                se=TRUE) %>%
        mutate(TIME = start + TIME*60*60)
       }
    recommendation$regimen %>% 
      filter(TIME>lastDose) %>%
      mutate(date=as.Date(start + TIME*60*60),
             time=strftime(start + TIME*60*60,"%H:%M"),
             dose=AMT,dose=round(dose,digits=2)) %>%
      select(-AMT,-TIME) })
  
  
  }

# Run the application 
shinyApp(ui = ui, server = server)
