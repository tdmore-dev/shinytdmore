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
#library(crosstalk)
library(rhandsontable)
library(lubridate)

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
model <- tdmore(rxModel, omega=vectorToDiagonalMatrix(omegas), res_var=list(errorModel("CONC", prop=0.149)))

ui <- navbarPage("TDMore mockup",
                tabPanel("Patient", icon=icon("users"),
                         titlePanel("Patient"),
                         DTOutput('patientTable')
                         ),
                 tabPanel("Prediction", icon=icon("address-card"),
                          titlePanel("Prediction"),
                          fluidRow(
                            column(4,
                                   rHandsontableOutput('hotdose'),
                                   h3("Add a row"),
                                   div(class='row', 
                                       div(class="col-sm-5", 
                                           uiOutput("ui_newdose"),
                                           uiOutput("ui_newtimedose"),
                                           uiOutput("ui_newdatedose"),
                                           actionButton("addrowdose", "Add")),
                                       div(class="col-sm-3")
                                   ),
                                   rHandsontableOutput('hotobs'),
                                   h3("Add a row"),
                                   div(class='row', 
                                       div(class="col-sm-5", 
                                           uiOutput("ui_newobs"),
                                           uiOutput("ui_newtimeobs"),
                                           uiOutput("ui_newdateobs"),
                                           actionButton("addrowobs", "Add")),
                                       div(class="col-sm-3")
                                   )
                            ),
                            column(8, plotlyOutput('predictionPlot',height="600px"))
                            
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
  
  pred <- predict(
    model,
    newdata = data.frame(TIME=seq(0, max(regimen$TIME)+24, length.out=300), CONC=NA),
    regimen=regimen,
    se=TRUE) %>% as.data.frame()
  pred$TIME <- min(pos) + pred$TIME*60*60
  
  # Output for tab "Prediction"
  
  values <- reactiveValues()

  # Handsontable
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
      rhandsontable(data.frame(db_dose), useTypes = TRUE, stretchH = "all")
  })

  output$ui_newdose <- renderUI({
    numericInput("newdose", "Dose", 0)
  })

  output$ui_newtimedose <- renderUI({
    textInput("newtimedose", "Time", "08:00")
  })
  
  output$ui_newdatedose <- renderUI({
    dateInput("newdatedose", "Date", tail(db_dose,n=1)$date, format = "yyyy/mm/dd")
  })

  observeEvent(input$addrowdose, {
    db_dose <- isolate(values[["db_dose"]])
    newrow <- tail(db_dose,n=1)
    row.names(newrow) <- as.numeric(row.names(newrow))+1
    newrow$dose <- isolate(input$newdose)
    newrow$date <- isolate(input$newdatedose)
    newrow$time <- isolate(input$newtimedose)
    values[["db_dose"]] <- rbind(db_dose,newrow)
  })

  ## Handsontable

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
  
  output$ui_newobs <- renderUI({
    numericInput("newobs", "Measure", 0)
  })
  
  output$ui_newtimeobs <- renderUI({
    textInput("newtimeobs", "Time", "08:00")
  })
  
  output$ui_newdateobs <- renderUI({
    dateInput("newdateobs", "Date", tail(db_dose,n=1)$date, format = "yyyy/mm/dd")
  })
  
  observeEvent(input$addrowobs, {
    db_obs <- isolate(values[["db_obs"]])
    newrow <- tail(db_obs,n=1)
    row.names(newrow) <- as.numeric(row.names(newrow))+1
    newrow$measure <- isolate(input$newobs)
    newrow$date <- isolate(input$newdateobs)
    newrow$time <- isolate(input$newtimeobs)
    values[["db_obs"]] <- rbind(db_obs,newrow)
  })

  output$predictionPlot <- renderPlotly({
    db_dose <- values[["db_dose"]]
    db_obs <- values[["db_obs"]]
    if (!is.null(db_dose) | !is.null(db_obs)) {
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
      
      pos2 <- as.POSIXct(strptime(paste(db_obs$date,db_obs$time),format = "%Y-%m-%d %H:%M"))
      observed <- data.frame(
        TIME=as.numeric(difftime(pos2, start, units="hour")),
        CONC=db_obs$measure
      )
      #debugonce(tdmore:::predict.tdmore)
      fit <- estimate(model, observed=observed, regimen=regimen)
      ipred <- fit %>%
        predict(newdata = data.frame(TIME=seq(0, max(regimen$TIME)+24, length.out=300), CONC=NA), 
                regimen=regimen, 
                se=TRUE)
      ipred$TIME <- start + ipred$TIME*60*60
     
     # p1 <- ggplot(pred) +
     #   geom_line(aes(x=TIME, y=CONC)) +
     #   geom_ribbon(aes(x=TIME,ymin=CONC.lower, ymax=CONC.upper), alpha=0.3) +
     #   labs(title="A priori prediction", y="Concentration (mg/L)", caption="Same as population prediction, as no cov effects in model yet")
     
     p1 <- ggplot(pred) +
       geom_line(aes(x=TIME, y=CONC, color="Population"), data=pred) +
       geom_line(aes(x=TIME, y=CONC, color="Individual"), data=ipred) +
       geom_ribbon(aes(x=TIME, ymin=CONC.lower, ymax=CONC.upper, fill="Individual"), data=ipred, alpha=0.3) +
       geom_point(aes(x=as.POSIXct(strptime(paste(db_obs$date,db_obs$time),format = "%Y-%m-%d %H:%M")), y=measure, color="Samples"),shape=4, size=3, data=db_obs)+
       geom_text(data=db_obs,aes(x=as.POSIXct(strptime(paste(db_obs$date,db_obs$time),format = "%Y-%m-%d %H:%M")), y=measure,label=measure, color="Samples"), check_overlap = T, show.legend = FALSE) +
       labs(title="Individual prediction", y="Concentration (mg/L)")+
       scale_fill_discrete(guide=FALSE)+
       scale_colour_discrete(name="Data",
                             breaks=c("Population", "Individual", "Samples"),
                             labels=c("Population", "Individual", "Samples"))
     
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
  
  # Output for tab "Measures"
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
    
    fulldb <- bind_rows(db%>%mutate(TIME=t,OCC=row_number(),eventType="TMT")%>%select(-t),pred%>%mutate(eventType="PRED"))%>%
      bind_rows(ipred%>%mutate(eventType="IPRED")) %>%
      arrange(TIME)%>%fill(OCC)
    
    df1 <- fulldb%>%filter(eventType=="TMT")%>%select(dose,TIME,OCC)
    df2 <- fulldb%>%filter(eventType=="PRED")%>%select(-dose)
    df3 <- fulldb%>%filter(eventType=="IPRED")%>%select(-dose)
    
    shared_df1 <- SharedData$new(df1, ~OCC, group = "Choose dosing")
    shared_df2 <- SharedData$new(df2, ~OCC, group = "Choose dosing")
    shared_df3 <- SharedData$new(df3, ~OCC, group = "Choose dosing")
    
    p1 <- ggplot(shared_df3) +
      geom_line(aes(x=TIME, y=CONC, color="Population"), data=shared_df2) +
      geom_line(aes(x=TIME, y=CONC, color="Individual"), data=shared_df3) +
      geom_ribbon(aes(x=TIME, ymin=CONC.lower, ymax=CONC.upper, fill="Individual"), data=shared_df3, alpha=0.3) +
      geom_point(aes(x=t, y=measure, color="Samples"),shape=4, size=3, data=tdm)+
      geom_text(data=tdm,aes(x=t, y=measure,label=measure, color="Samples"), nudge_x = 10000, nudge_y = 0, check_overlap = T, show.legend = FALSE) +
      labs(title="Individual prediction", y="Concentration (mg/L)")+
      scale_fill_discrete(guide=FALSE)+
      scale_colour_discrete(name="Data",
                            breaks=c("Population", "Individual", "Samples"),
                            labels=c("Population", "Individual", "Samples"))
    
    pt1 <- ggplotly(p1) %>% config(scrollZoom=TRUE, collaborate=FALSE, displayModeBar=FALSE, displaylogo=FALSE)
    
    mytext=paste("Time = ", shared_df2$data()$TIME, "\n" , "Conc = ", shared_df2$data()$CONC, "\n", "Data: ",shared_df2$data()$eventType,  sep="")    
    mytext2=paste("Time = ", shared_df3$data()$TIME, "\n" , "Conc = ", shared_df3$data()$CONC, "\n", "Data: ",shared_df3$data()$eventType,  sep="")    
    mytext3=paste("Time = ", tdm$t, "\n" , "Conc = ", tdm$measure, "mg/L \n", "Data: Observation",  sep="")    
    mytext4=paste("Time = ", shared_df1$data()$TIME, "\n" , "Dose = ", shared_df1$data()$dose, "mg \n", sep="")    
    
    
    p2 <- ggplot(shared_df1,aes(x=TIME, y=dose))+
      geom_linerange(ymin=0, aes(ymax=dose))+
      geom_text(aes(x=TIME, y=dose,label=dose), nudge_x = 10000, nudge_y = 0, check_overlap = T, show.legend = FALSE) +
      coord_cartesian(xlim = c(min(db$t),max(pred$TIME)), ylim = c(0,max(db$dose)+2, expand = FALSE)) +
      labs(x="Time", y="Dose (mg)")
    
    pt2 <- ggplotly(p2) %>% config(scrollZoom=TRUE, collaborate=FALSE, displayModeBar=FALSE, displaylogo=FALSE)
    subplot(
      style(pt1, text=mytext, hoverinfo = "text", traces = c(1)) %>%
        style(text=mytext2, hoverinfo = "text", traces = c(2)) %>%
        style(hoverinfo = "none", traces = c(3)),
      style(pt2),#, text=mytext4, hoverinfo = "text"),
      nrows = 2, heights = c(0.8, 0.2), widths = c(1),
      shareX = TRUE, shareY = FALSE, titleX = TRUE, titleY = TRUE, which_layout = 1
    )
  })
  
  # Output for tab "Targets"
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
    
    fulldb <- bind_rows(db%>%mutate(TIME=t,OCC=row_number(),eventType="TMT")%>%select(-t),
                        pred%>%mutate(eventType="PRED"),
                        tdm%>%mutate(TIME=t,eventType="Sample")) %>%
      bind_rows(ipred%>%mutate(eventType="IPRED"))%>%
      arrange(TIME)%>%fill(OCC)
    
    df1 <- fulldb%>%filter(eventType=="TMT")%>%select(dose,TIME,OCC)
    df2 <- fulldb%>%filter(eventType=="PRED")%>%select(-dose)
    df3 <- fulldb%>%filter(eventType=="IPRED")%>%select(-dose)
    df4 <- fulldb%>%filter(eventType=="Sample")%>%select(-dose)
    
    shared_df1 <- SharedData$new(df1, ~OCC, group = "Choose dosing")
    shared_df2 <- SharedData$new(df2, ~OCC, group = "Choose dosing")
    shared_df3 <- SharedData$new(df3, ~OCC, group = "Choose dosing")
    shared_df4 <- SharedData$new(df4, ~OCC, group = "Choose dosing")
  
    p1 <- ggplot(shared_df3) +
      geom_line(aes(x=TIME, y=CONC, color="Population"), data=shared_df2) +
      geom_line(aes(x=TIME, y=CONC, color="Individual"), data=shared_df3) +
      geom_ribbon(aes(x=TIME, ymin=CONC.lower, ymax=CONC.upper, fill="Population"), data=shared_df2, alpha=0.3) +
      geom_ribbon(aes(x=TIME, ymin=CONC.lower, ymax=CONC.upper, fill="Individual"), data=shared_df3, alpha=0.3) +
      geom_text(aes(x=TIME, y=measure,label=measure, color="Sample"), nudge_x = 10000, nudge_y = 0, check_overlap = T, show.legend = FALSE, data=shared_df4) +
      geom_point(aes(x=TIME, y=measure, color="Sample"),shape=4, size=3, data=shared_df4) +
      geom_rect(data=targets, aes(xmin=t1, xmax=t2, ymin=lower, ymax=upper, fill="Target"), alpha=0.3) +
      labs(title="Individual prediction", y="Concentration (mg/L)")+
      scale_fill_discrete(guide=FALSE)+
      scale_colour_discrete(name="Data",
                            breaks=c("Population", "Individual", "Samples"),
                            labels=c("Population", "Individual", "Samples"))
    
    pt1 <- ggplotly(p1) %>% config(scrollZoom=TRUE, collaborate=FALSE, displayModeBar=FALSE, displaylogo=FALSE)
    
    mytext=paste("Time = ", shared_df2$data()$TIME, "\n" , "Conc = ", shared_df2$data()$CONC, "\n", "Data: ",shared_df2$data()$eventType,  sep="")    
    mytext2=paste("Time = ", shared_df3$data()$TIME, "\n" , "Conc = ", shared_df3$data()$CONC, "\n", "Data: ",shared_df3$data()$eventType,  sep="")    
    mytext3=paste("Time = ", shared_df4$data()$TIME, "\n" , "Conc = ", shared_df4$data()$measure, "mg/L \n", "Data: Observation",  sep="")    
    mytext4=paste("Time = ", shared_df1$data()$TIME, "\n" , "Dose = ", shared_df1$data()$dose, "mg \n", sep="")
    mytext5=paste("Targets: \n Min ", targets$lower, "mg/L \n Max " , targets$upper, "mg/L", sep="")
    
    
    p2 <- ggplot(shared_df1,aes(x=TIME, y=dose))+
      geom_linerange(ymin=0, aes(ymax=dose))+
      geom_text(aes(x=TIME, y=dose,label=dose), nudge_x = 10000, nudge_y = 0, check_overlap = T, show.legend = FALSE) +
      coord_cartesian(xlim = c(min(db$t),max(pred$TIME)), ylim = c(0,max(db$dose)+2, expand = FALSE)) +
      labs(x="Time", y="Dose (mg)")
    
    pt2 <- ggplotly(p2) %>% config(scrollZoom=TRUE, collaborate=FALSE, displayModeBar=FALSE, displaylogo=FALSE)
    subplot(
      style(pt1, text=mytext, hoverinfo = "text", traces = c(1)) %>%
        style(text=mytext2, hoverinfo = "text", traces = c(2)) %>%
        style(hoverinfo = "none", traces = c(3,4,5)) %>%
        style(text=mytext4, hoverinfo = "text", traces = c(6)) %>%
        style(text=mytext5, hoverinfo = "text", traces = c(7)),
      style(pt2),#, text=mytext4, hoverinfo = "text"),
      nrows = 2, heights = c(0.8, 0.2), widths = c(1),
      shareX = TRUE, shareY = FALSE, titleX = TRUE, titleY = TRUE, which_layout = 1
    )
  })
  
  # Output for tab "Adjustments"
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
    recommendation <- tdmore::findDose(fit, doseRows=which(is.na(newRegimen$AMT)),regimen = newRegimen, target = Target)
    #browser()
    newRegimen$AMT[ is.na(newRegimen$AMT)] <- recommendation$dose
    ipredNew <- fit %>%
      predict(newdata = data.frame(TIME=seq(max(regimen$TIME), max(newRegimen$TIME)+12, length.out=300), CONC=NA), 
              regimen=newRegimen, 
              se=TRUE) %>%
      mutate(TIME = min(db$t) + TIME*60*60)
    #stop(newRegimen)
    
    fulldb <- bind_rows(db%>%mutate(TIME=t,OCC=row_number(),eventType="TMT")%>%select(-t),
                        pred%>%mutate(eventType="PRED"),
                        tdm%>%mutate(TIME=t,eventType="Sample")%>%select(-t),
                        ipredNew%>%mutate(eventType="Recommendation")) %>%
      bind_rows(ipred%>%mutate(eventType="IPRED"))%>%
      bind_rows(recommendation$regimen%>%mutate(TIME=min(db$t) + TIME*60*60,dose=AMT,eventType="TMT Recommended",dose=round(dose,digits=2))%>%select(-AMT))%>%
      arrange(TIME)%>%fill(OCC)

    df1 <- fulldb%>%filter(eventType=="TMT")%>%select(dose,TIME,OCC)
    df2 <- fulldb%>%filter(eventType=="PRED")%>%select(-dose)
    df3 <- fulldb%>%filter(eventType=="IPRED")%>%select(-dose)
    df4 <- fulldb%>%filter(eventType=="Sample")%>%select(-dose)
    df5 <- fulldb%>%filter(eventType=="Recommendation")%>%select(-dose)%>%filter(OCC==max(OCC))%>%mutate(OCC=OCC+1)
    df6 <- fulldb%>%filter(eventType=="TMT Recommended")%>%select(dose,TIME,OCC)%>%filter(OCC==max(OCC))%>%mutate(OCC=OCC+1)

    shared_df1 <- SharedData$new(df1, ~OCC, group = "Choose dosing")
    shared_df2 <- SharedData$new(df2, ~OCC, group = "Choose dosing")
    shared_df3 <- SharedData$new(df3, ~OCC, group = "Choose dosing")
    shared_df4 <- SharedData$new(df4, ~OCC, group = "Choose dosing")
    shared_df5 <- SharedData$new(df5, ~OCC, group = "Choose dosing")
    shared_df6 <- SharedData$new(df6%>%filter(row_number()!=1), ~OCC, group = "Choose dosing")

    p1 <- ggplot(shared_df3) +
      geom_line(aes(x=TIME, y=CONC, color="Population"), data=shared_df2) +
      geom_line(aes(x=TIME, y=CONC, color="Individual"), data=shared_df3) +
      geom_line(aes(x=TIME, y=CONC, color="Recommendation"), data=shared_df5) +
      geom_ribbon(aes(x=TIME, ymin=CONC.lower, ymax=CONC.upper, color="Recommendation", fill="Recommendation"), data=shared_df5, alpha=0.3) +
      geom_text(aes(x=TIME, y=measure,label=measure, color="Sample"), nudge_x = 10000, nudge_y = 0, check_overlap = T, show.legend = FALSE, data=shared_df4) +
      geom_point(aes(x=TIME, y=measure, color="Sample"),shape=4, size=3, data=shared_df4) +
      geom_rect(data=targets, aes(xmin=t1, xmax=t2, ymin=lower, ymax=upper, fill="Target"), alpha=0.3) +
      labs(title="Individual prediction", y="Concentration (mg/L)")+
      scale_fill_discrete(guide=FALSE)+
      scale_colour_discrete(name="Data",
                            breaks=c("Population", "Individual", "Samples"),
                            labels=c("Population", "Individual", "Samples"))

    pt1 <- ggplotly(p1) %>% config(scrollZoom=TRUE, collaborate=FALSE, displayModeBar=FALSE, displaylogo=FALSE)

    mytext=paste("Time = ", shared_df2$data()$TIME, "\n" , "Conc = ", shared_df2$data()$CONC, "\n", "Data: ",shared_df2$data()$eventType,  sep="")
    mytext2=paste("Time = ", shared_df3$data()$TIME, "\n" , "Conc = ", shared_df3$data()$CONC, "\n", "Data: ",shared_df3$data()$eventType,  sep="")
    mytext3=paste("Time = ", shared_df4$data()$TIME, "\n" , "Conc = ", shared_df4$data()$measure, "mg/L \n", "Data: Observation",  sep="")
    mytext4=paste("Time = ", shared_df1$data()$TIME, "\n" , "Dose = ", shared_df1$data()$dose, "mg \n", sep="")
    mytext5=paste("Targets: \n Min ", targets$lower, "mg/L \n Max " , targets$upper, "mg/L", sep="")


    p2 <- ggplot(shared_df1,aes(x=TIME, y=dose))+
      geom_linerange(ymin=0, aes(ymax=dose))+
      geom_text(aes(x=TIME, y=dose,label=dose),size=3, check_overlap = T, show.legend = FALSE) +
      geom_linerange(data=shared_df6, ymin=0, aes(ymax=dose))+
      geom_text(data=shared_df6, aes( x=TIME, y=dose,label=dose),size=3, check_overlap = T, show.legend = FALSE) +
      coord_cartesian(xlim = c(min(db$t),max(shared_df5$data()$TIME)), ylim = c(0,max(shared_df6$data()$dose)+2, expand = FALSE)) +
      labs(x="Time", y="Dose (mg)")

    pt2 <- ggplotly(p2) %>% config(scrollZoom=TRUE, collaborate=FALSE, displayModeBar=FALSE, displaylogo=FALSE)
    subplot(
      style(pt1, text=mytext, hoverinfo = "text", traces = c(1)) %>%
        style(text=mytext2, hoverinfo = "text", traces = c(2)) %>%
        style(hoverinfo = "none", traces = c(3,4,5)) %>%
        style(text=mytext4, hoverinfo = "text", traces = c(6)) %>%
        style(text=mytext5, hoverinfo = "text", traces = c(7)),
      style(pt2),#, text=mytext4, hoverinfo = "text"),
      nrows = 2, heights = c(0.8, 0.2), widths = c(1),
      shareX = TRUE, shareY = FALSE, titleX = TRUE, titleY = TRUE, which_layout = 1
    )
    
  })
  
  
  }

# Run the application 
shinyApp(ui = ui, server = server)
