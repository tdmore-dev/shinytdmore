#'
#' Get the prediction tab panel.
#'
#' @return a panel
#'
getPredictionTabPanel <- function() {
  panel <- tabPanel(
    "Prediction",
    icon = icon("address-card"),
    textOutput(outputId="tab_title"),
    fluidRow(
      column(
        3,
        conditionalPanel(
          condition = "output.plot_type == 'population' || output.plot_type == 'fit'",
          fluidRow(
            column(10, h4("Measures")),
            column(2, actionButton("addObs", "Add", style="float:right"))
          ),
          rHandsontableOutput('hotobs'),
          hr(),
          fluidRow(
            column(10, h4("Doses")),
            column(2, actionButton("addDose", "Add", style="float:right"))
          ),
          rHandsontableOutput('hotdose'),
          hr(),
          h4("Target"),
          numericInput("targetDown", "Lower limit", 10),
          numericInput("targetUp", "Upper limit", 15)
        ),
        
        conditionalPanel(
          condition = "output.plot_type == 'recommendation'",
          h3("Dosing future"),
          dataTableOutput('hotdosefuture')
          #actionButton("getComputerPrediction", "Computer prediction")
        )
      ),
      column(
        9,
        fluidRow(
          column(1,
                 actionButton("previous_plot", label="", icon=icon("backward"))),
          column(10,
                 textOutput(outputId="plot_title"),
                 tags$head(tags$style("#plot_title{font-size: 20px;text-align: center;justify-content: center;}"))),
          column(1,
                 actionButton("next_plot", label="", icon=icon("forward"), style="float:right"))
        ),
        conditionalPanel(condition = "output.plot_type == 'population'",
                         plotlyOutput('populationPlot', height = "600px")),
        
        conditionalPanel(condition = "output.plot_type == 'fit'",
                         plotlyOutput('fitPlot', height = "600px")),
        
        conditionalPanel(condition = "output.plot_type == 'recommendation'",
                         plotlyOutput('recommendationPlot', height = "600px"))
      )
    ),
    tags$head(tags$style(HTML(".handsontable {overflow-x:hidden;}"), "#tab_title{font-size: 30px; margin-top: 10px; margin-bottom: 10px;}"))
  )
  return(panel)
}

#'
#' Previous/Next button logic.
#'
#' @param input shiny input
#' @param output shiny output
#' @param val main reactive container
#'
previousNextLogic <- function(input, output, val) {
  plotTypes <- c("population", "fit", "recommendation")
  plotTitles <- c("Population prediction", "Individual prediction", "Recommendation")
  
  observeEvent(input$previous_plot, {
    plotTypeIndex <- which(plotTypes==val$plot_type) - 1
    if (plotTypeIndex >= 1) {
      val$plot_type <- plotTypes[plotTypeIndex]
    }
  })
  
  observeEvent(input$next_plot, {
    plotTypeIndex <- which(plotTypes==val$plot_type) + 1
    if (plotTypeIndex <= length(plotTypes)) {
      val$plot_type <- plotTypes[plotTypeIndex]
    }
  })
  
  output$plot_title <- renderText({
    val$plot_title <- plotTitles[which(plotTypes==val$plot_type)]
    return(val$plot_title)
  })
  
  output$plot_type <- reactive({
    return(val$plot_type)
  })
  
  outputOptions(output, "plot_type", suspendWhenHidden = FALSE)
}

#'
#' Prediction tab server.
#'
#' @param input shiny input
#' @param output shiny output
#' @param session shiny session
#' @param val main reactive container
#'
predictionTabServer <- function(input, output, session, val) {

  # Previous/Next button logic
  previousNextLogic(input, output, val)
  
  # Update tab title according to patient's name
  output$tab_title <- renderText({return(paste(val$patient$firstname, val$patient$lastname))})
  
  observe({
    ## Update `regimen` based on changes in db_dose
    ## TODO: tdmore should be able to work with POSIXct directly
    pos <- as.POSIXct(strptime(paste(val$db_dose$date,val$db_dose$time),format = "%Y-%m-%d %H:%M"))
    regimen <- data.frame(
      TIME=as.numeric(difftime(pos, min(pos), units="hour")),
      AMT=val$db_dose$dose
    )
    attr(regimen, "start") <- min(pos)
    val$regimen <- regimen
  })
  observe({
    pred <- predict(
      val$model,
      newdata = data.frame(TIME=seq(0, max(val$regimen$TIME)+24, length.out=300), CONC=NA),
      regimen=val$regimen,
      covariates=val$covs,
      se=TRUE) %>% as.data.frame()
    ## TODO: tdmore should be able to work with POSIXct directly
    pred$TIME <- attr(val$regimen, "start") + pred$TIME*60*60
    val$pred <- pred
  })
  
  # Output for tab "Prediction"
  # Handsontable Dose
  output$hotdose <- renderRHandsontable({
    ## HOURS select option
    str1 <- paste0("0",as.character(seq(0,23,1)))
    v1 <- substr(str1,(nchar(str1)+1)-2,nchar(str1))
    str2 <- paste0("0",as.character(seq(0,45,15)))
    v2 <- paste0(":",substr(str2,(nchar(str2)+1)-2,nchar(str2)))
    HOURS <- sort(apply(expand.grid(v1, v2), 1, paste, collapse = "", sep = "")) 
    
    rhandsontable(val$db_dose, useTypes = TRUE, stretchH = "all", rowHeaders = NULL,
      colHeaders = c("Date", "Time", getDoseColumnLabel(val$model))) %>%
      hot_context_menu(allowRowEdit = TRUE, allowColEdit = FALSE) %>%
      hot_col(col = "Time", type = "dropdown", source = HOURS)
  })
  observeEvent(input$hotdose, {
    val$db_dose <- hot_to_r(input$hotdose)
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
  
  observeEvent(input$addDose, {
    if(nrow(val$db_dose) > 0) {
      newdose <- val$db_dose[ nrow(val$db_dose), ]
      lastdose <- as.POSIXct(strptime(paste(newdose$date, newdose$time),format = "%Y-%m-%d %H:%M"))
      lastdose <- lastdose + 12*60*60
    } else {
      newdose <- data.frame(date=NULL, time=NULL, dose=5)
      lastdose <- now()
    }
    newdose$date <- format(lastdose, "%Y-%m-%d")
    newdose$time <- format(lastdose, "%H:%M")
    
    val$db_dose <- rbind(val$db_dose, newdose)
    #showModal(dataModalDose())
  })
  
  observeEvent(input$okDose, {
    # Check that data object exists and is data frame.
    if (!is.null(input$newdose) & !is.null(input$newtimedose) & !is.null(input$newdatedose)) {
      db_dose <- isolate(val[["db_dose"]])
      newrow <- tail(db_dose,n=1)
      row.names(newrow) <- as.numeric(row.names(newrow))+1
      newrow$dose <- isolate(input$newdose)
      newrow$date <- isolate(input$newdatedose)
      newrow$time <- isolate(input$newtimedose)
      val[["db_dose"]] <- rbind(db_dose,newrow)
      removeModal()
    } else {
      showModal(dataModal(failed = TRUE))
    }
  })
  
  # Handsontable Obs
  output$hotobs <- renderRHandsontable({
    db_obs <- val$db_obs
    if (!is.null(db_obs))
      rhandsontable(data.frame(db_obs), useTypes = TRUE, stretchH = "all", rowHeaders = NULL,
        colHeaders = c("Date", "Time", getMeasureColumnLabel(val$model), "Use")) %>% hot_col("Use", halign = "htCenter")
  })
  observeEvent(input$hotobs, {
    if (!is.null(input$hotobs)) {
      val$db_obs = hot_to_r(input$hotobs)
    }
  })
  
  dataModalObs <- function(failed = FALSE) {
    modalDialog(
      numericInput("newobs", "Measure", 0),
      textInput("newtimeobs", "Time", "08:00"),
      #timeInput("time_input", "Enter time", value = strptime("08:00:00", "%T")),
      dateInput("newdateobs", "Date", tail(val$db_dose,n=1)$date, format = "yyyy/mm/dd"),
      tags$style(
        type = "text/css", ".datepicker{z-index: 1100 !important;}"),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("okObs", "OK")
      )
    )
  }
  
  observeEvent(input$addObs, {
    if(nrow(val$db_obs) > 0) {
      newobs <- val$db_obs[ nrow(val$db_obs), ]
      lastobs <- dateAndTimeToPOSIX(newobs$date, newobs$time)
      lastobs <- lastobs + 24*60*60
    } else {
      newobs <- data.frame(date=NULL, time=NULL, measure=0)
      lastobs <- now()
    }
    newobs$date <- format(lastobs, "%Y-%m-%d")
    newobs$time <- format(lastobs, "%H:%M")
    
    val$db_obs <- rbind(val$db_obs, newobs)
    #showModal(dataModalDose())
  })
  
  observeEvent(input$okObs, {
    # Check that data object exists and is data frame.
    if (!is.null(input$newobs) & !is.null(input$newtimeobs) & !is.null(input$newdateobs)) {
      db_obs <- isolate(val[["db_obs"]])
      newrow <- tail(db_obs,n=1)
      row.names(newrow) <- as.numeric(row.names(newrow))+1
      newrow$measure <- isolate(input$newobs)
      newrow$date <- isolate(input$newdateobs)
      newrow$time <- isolate(input$newtimeobs)
      newrow$use <- TRUE
      val[["db_obs"]] <- rbind(db_obs,newrow)
      removeModal()
    } else {
      showModal(dataModal(failed = TRUE))
    }
  })
  
  # 1 - Population prediction
  populationPlot <- reactive({
    progress <- shiny::Progress$new(max = 2)
    on.exit(progress$close())
    progress$set(message = "Preparing...", value = 0.5)
    target <- c(input$targetDown, input$targetUp)
    plots <- preparePredictionPlots(doses=val$db_dose, obs=val$db_obs, model=val$model, covs=val$covs, target=target, population=T)
    progress$set(message = "Rendering plot...", value = 1)
    if(!is.null(plots)) mergePlots(plots$p1, plots$p2)
  })
  output$populationPlot <- renderPlotly(populationPlot()) #renderPlotly(isolate(populationPlot()))
  # observe({
  #   populationPlot() %>% updatePlot("populationPlot")
  # })
  # NOTE: Animations work if plot is 'isolated'. However, this creates refresh issues (e.g. plot not well updated if new patient)
  
  # 2 - Individual prediction
  fitPlot <- reactive({
    progress <- shiny::Progress$new(max = 2)
    on.exit(progress$close())
    progress$set(message = "Preparing...", value = 0.5)
    target <- c(input$targetDown, input$targetUp)
    plots <- preparePredictionPlots(doses=val$db_dose, obs=val$db_obs, model=val$model, covs=val$covs, target=target, population=F)
    progress$set(message = "Rendering plot...", value = 1)
    if(!is.null(plots)) mergePlots(plots$p1, plots$p2)
  })
  output$fitPlot <- renderPlotly(fitPlot())
  
  # 3 - Recommendation
  recommendationPlot <- reactive({
    progress <- shiny::Progress$new(max = 2)
    on.exit(progress$close())
    progress$set(message = "Preparing...", value = 0.5)
    target <- c(input$targetDown, input$targetUp)
    plots <- prepareRecommendationPlots(doses=val$db_dose, obs=val$db_obs, model=val$model, covs=val$covs, target=target)
    progress$set(message = "Rendering plot...", value = 1)
    if(!is.null(plots)) mergePlots(plots$p1, plots$p2)
  })
  output$recommendationPlot <- renderPlotly(recommendationPlot())
  
  # Handsontable Dose Future
  output$hotdosefuture <- renderDataTable({
    db_dose <- val[["db_dose"]]
    db_obs <- val[["db_obs"]]
    db_obs_filtered <-  db_obs %>% filter(use==TRUE)
    
    if (!is.null(db_dose) | !is.null(db_obs_filtered)) {
      pos <- dateAndTimeToPOSIX(db_dose$date, db_dose$time)
      start <- min(pos)
      stop <- max(pos + 24*60*60)
      regimen <- data.frame(
        TIME=as.numeric(difftime(pos, start, units="hour")),
        AMT=db_dose$dose
      )
      
      pred <- predict(
        val$model,
        newdata = data.frame(TIME=seq(0, max(regimen$TIME)+24, length.out=300), CONC=NA),
        regimen=regimen,
        covariates=val$covs,
        se=TRUE) %>% as.data.frame()
      pred$TIME <- min(pos) + pred$TIME*60*60
      
      
      pos2 <- dateAndTimeToPOSIX(db_obs$date, db_obs$time)
      observed <- data.frame(
        TIME=as.numeric(difftime(pos2, start, units="hour")),
        CONC=db_obs_filtered$measure
      )
      #debugonce(tdmore:::predict.tdmore)
      fit <- estimate(val$model, observed=observed, regimen=regimen, covariates=val$covs)
      ipred <- fit %>%
        predict(newdata = data.frame(TIME=seq(0, max(regimen$TIME)+24, length.out=300), CONC=NA),
                regimen=regimen,
                covariates=val$covs,
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
                covariates=val$covs,
                se=TRUE) %>%
        mutate(TIME = start + TIME*60*60)
    }
    doseColumnName <- getDoseColumnLabel(val$model, breakLine=F)
    retValue <- recommendation$regimen %>%
      filter(TIME>lastDose) %>%
      mutate(Date=as.Date(start + TIME*60*60),
             Time=strftime(start + TIME*60*60,"%H:%M"),
             !!doseColumnName:=round(AMT, digits=2))
    return(retValue %>% select(-AMT,-TIME))
    })
}