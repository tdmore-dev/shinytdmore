#'
#' Prediction tab user interface.
#'
#' @param id namespace id
#' @return a panel
#' 
#' @export
#'
predictionTabUI <- function(id) {
  ns <- NS(id)
  panel <- tabPanel(
    "Prediction",
    icon = icon("address-card"),
    div(
      actionButton(ns("sidebarCollapse"), icon=icon("minus-square"), label="", style="display: inline-block; vertical-align: middle; font-size:120%;"),
      span(textOutput(outputId=ns("tab_title")), style="display: inline-block; vertical-align: middle; margin-left: 10px;")
    ),
    fluidRow(class="wrapper",
      div(id="sidebar",
        bsCollapse(id=ns("bsCollapse"), multiple=T, open=c("Doses", "Measures", "Now"),
          bsCollapsePanel(title="Doses", style="info",
            conditionalPanel(
              condition = "output.plot_type == 'population' || output.plot_type == 'fit'",
              rHandsontableOutput(ns("hotdose")),
              actionButton(ns("addDose"), "Add dose", style="margin-top: 5px;"),
              ns=ns
            ),
            conditionalPanel(
              condition = "output.plot_type == 'recommendation'",
              rHandsontableOutput(ns("hotdosefuture")),
              actionButton(ns("addDoseFuture"), "Add dose", style="margin-top: 5px;"),
              ns=ns
            )
          ),
          bsCollapsePanel(title="Measures", style="info",
            rHandsontableOutput(ns("hotobs")),
            actionButton(ns("addObs"), "Add measure", style="margin-top: 5px;")
          ),
          bsCollapsePanel(title="Now", style="info",
            editableInput(inputId=ns("nowDate"), type = "combodate", value="2000-01-01 00:00")
          ),
          bsCollapsePanel(title="Covariates", style="info",
            conditionalPanel(
              condition = "output.display_covariates == true",
              rHandsontableOutput(ns("hotcov")),
              actionButton(ns("addCovariate"), "Add covariate", style="margin-top: 5px;"),
              ns=ns
            ),
            conditionalPanel(
              condition = "output.display_covariates == false",
              h5("No covariate in model"),
              ns=ns
            )
          ),
          bsCollapsePanel(title="Target", style="info",
            numericInput(ns("targetDown"), "Lower limit", 0),
            numericInput(ns("targetUp"), "Upper limit", 0)
          )
        )
      ),
      div(id="content",
        fluidRow(
         column(1, actionButton(ns("previous_plot"), label="Previous", icon=icon("backward"))),
         tags$head(tags$style(HTML('#predictionTabId-previous_plot{background-color:#dde5eb}'), '#predictionTabId-previous_plot.attr("disabled", "true")')),
         
         column(10, textOutput(outputId=ns("plot_title")),
                tags$head(tags$style("#predictionTabId-plot_title{font-size: 20px;text-align: center;justify-content: center;}"))),
         
         column(1, actionButton(ns("next_plot"), label="Next", icon=icon("forward"), style="float:right")),
         tags$head(tags$style(HTML('#predictionTabId-next_plot{background-color:#dde5eb}')))
       ),
       conditionalPanel(condition = "output.plot_type == 'population'",
                        plotlyOutput(ns("populationPlot"), height="600px", width="100%"), ns=ns),

       conditionalPanel(condition = "output.plot_type == 'fit'",
                        plotlyOutput(ns("fitPlot"), height="600px", width="100%"), ns=ns),

       conditionalPanel(condition = "output.plot_type == 'recommendation'",
                        plotlyOutput(ns("recommendationPlot"), height="600px", width="100%"), ns=ns)
      )
    ),
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "sidebar.css"),
      tags$link(rel = "stylesheet", type = "text/css", href = "bootstrap.css"),
      tags$style(HTML(".handsontable {overflow-x:hidden;}"), "#predictionTabId-tab_title{font-size: 30px; margin-top: 10px; margin-bottom: 10px;}")
    ),
    singleton(tags$head(HTML(
      '
    <script type="text/javascript">
      $(document).ready(function() {
      
      // Disable previous_plot button after a click
      Shiny.addCustomMessageHandler("disableButton", function(message) {
      $("#"+message).attr("disabled", "true");
      });
      
      // Enable previous_plot button when computation is finished
      Shiny.addCustomMessageHandler("enableButton", function(message) {
      $("#"+message).removeAttr("disabled");
      });

      Shiny.addCustomMessageHandler("nowDate", function(message) {
      $("#predictionTabId-nowDate").editable("setValue", message);
      });

      $("#predictionTabId-sidebarCollapse").on("click", function() {
      $("#sidebar").toggleClass("active");
      $(this).toggleClass("active");
      });

      })
    </script>
    '
    )))
  )
  return(panel)
}

#'
#' Previous/Next button logic.
#'
#' @param input shiny input
#' @param output shiny output
#' @param session shiny session
#' @param val main reactive container
#' @param ns namespace
#'
previousNextLogic <- function(input, output, session, val, ns) {
  plotTypes <- c("population", "fit", "recommendation")
  plotTitles <- c("Population prediction", "Individual prediction", "Recommendation")

  enableDisableButtons <- function(plotTypeIndex) {
    if (plotTypeIndex <= 1) {
      session$sendCustomMessage("disableButton", ns("previous_plot"))
    } else {
      session$sendCustomMessage("enableButton", ns("previous_plot"))
    }
    if (plotTypeIndex >= 3) {
      session$sendCustomMessage("disableButton", ns("next_plot"))
    } else {
      session$sendCustomMessage("enableButton", ns("next_plot"))
    }
  }
  
  # Logic initialisation (when a new patient is loaded)
  observeEvent(val$set_patient_counter, {
    val$plot_type <- "population"
    enableDisableButtons(1)
  })
  
  observeEvent(input$previous_plot, {
    plotTypeIndex <- which(plotTypes==val$plot_type) - 1
    if (plotTypeIndex >= 1) {
      val$plot_type <- plotTypes[plotTypeIndex]
    }
    enableDisableButtons(plotTypeIndex)
  })
  
  observeEvent(input$next_plot, {
    plotTypeIndex <- which(plotTypes==val$plot_type) + 1
    if (plotTypeIndex <= length(plotTypes)) {
      val$plot_type <- plotTypes[plotTypeIndex]
    }
    enableDisableButtons(plotTypeIndex)
  })
  
  output$plot_title <- renderText({
    val$plot_title <- plotTitles[which(plotTypes==val$plot_type)]
    return(val$plot_title)
  })
  
  output$plot_type <- reactive({
    return(val$plot_type)
  })
  
  outputOptions(output, "plot_type", suspendWhenHidden=F)
}

forceUpdateNowDate <- function(session, val, date, ns) {
  minute <- lubridate::minute(date) %/% 5
  value <- paste0(format(date, format = paste(getDateFormat(), "%H"), tz=getAppTimeZone()), ":", pad(minute*5))
  updateTextInput(session=session, inputId="nowDate", value=value) # This updates the 'visual' text
  session$sendCustomMessage("nowDate", value) # This update the javascript value in the date picket
  val$now_date <- date
}

#'
#' Now date logic.
#'
#' @param input shiny input
#' @param output shiny output
#' @param session shiny session
#' @param val main reactive container
#' @param ns namespace
#' @importFrom lubridate ymd_hm
#'
nowDateLogic <- function(input, output, session, val, ns) {
  observeEvent(input$nowDate, {
    defaultDateInUI <- "2000-01-01 00:00"
    if (input$nowDate != val$now_date && input$nowDate != defaultDateInUI) {
      forceUpdateNowDate(session, val, lubridate::ymd_hm(input$nowDate, tz = Sys.timezone()), ns)
    }
  })
  observeEvent(val$set_patient_counter, {
    forceUpdateNowDate(session, val, val$now_date, ns)
  })
}

#'
#' Target logic.
#'
#' @param input shiny input
#' @param output shiny output
#' @param session shiny session
#' @param val main reactive container
#' @param ns namespace
#'
targetLogic <- function(input, output, session, val, ns) {
  observeEvent(input$targetDown, {
    val$target$min <- input$targetDown
  })
  observeEvent(input$targetUp, {
    val$target$max <- input$targetUp
  })
  observeEvent(val$set_patient_counter, {
    updateNumericInput(session=session, inputId="targetDown", value=val$target$min) # No need to ns()?
    updateNumericInput(session=session, inputId="targetUp", value=val$target$max) # No need to ns()?
  })
}

#'
#' Prediction tab server.
#'
#' @param input shiny input
#' @param output shiny output
#' @param session shiny session
#' @param val main reactive container
#' 
#' @export
#'
predictionTab <- function(input, output, session, val) {
  ns <- session$ns
  
  output$display_covariates <- reactive({
    return(length(val$model$covariates) > 0)
  })
  
  outputOptions(output, "display_covariates", suspendWhenHidden=F)
  
  # Previous/Next button logic
  previousNextLogic(input, output, session, val, ns)
  
  # Now date logic
  nowDateLogic(input, output, session, val, ns)
  
  # Target logic
  targetLogic(input, output, session, val, ns)
  
  # Update tab title according to patient's name
  output$tab_title <- renderText({return(paste(val$patient$firstname, val$patient$lastname))})
  
  # Observations/Measures table logic
  output$hotobs <- renderRHandsontable({
    if (!is.null(val$db_obs))
      borderRow <- getTableBorderIndex(val$db_obs, val$now_date, F)
      rhandsontable(val$db_obs, useTypes = TRUE, stretchH = "all", rowHeaders = NULL,
                    colHeaders = c("Date", "Time", getMeasureColumnLabel(val$model), "Use")) %>%
                    hot_col("Use", halign = "htCenter") %>%
                    hot_col(col="Time", type="dropdown", source=hoursList()) %>%
                    hot_table(customBorders = list(list(
                      range=list(from=list(row=borderRow-1, col=0), to=list(row=borderRow, col=ncol(val$db_obs)-1)),
                      top=list(width=2, color=nowColorHex()))))
  })
  
  observeEvent(input$hotobs, {
    if (!is.null(input$hotobs)) {
      val$db_obs <- autoSortByDate(hot_to_r(input$hotobs))
    }
  })
  
  observeEvent(input$addObs, {
    if(nrow(val$db_obs) > 0) {
      newobs <- val$db_obs[ nrow(val$db_obs), ]
      lastobs <- dateAndTimeToPOSIX(newobs$date, newobs$time)
      lastobs <- lastobs + 24*3600 # 24 hours is a good default value if no metadata
    } else {
      output <- getModelOutput(val$model)
      outputMetadata <- getMetadataByName(val$model, output)
      newobs <- data.frame(date="", time="", measure=if(is.null(outputMetadata)) {0} else {outputMetadata$default_value}, use=T)
      lastobs <- Sys.time()
    }
    newobs$date <- POSIXToDate(lastobs)
    newobs$time <- POSIXToTime(lastobs)
    val$db_obs <- rbind(val$db_obs, newobs)
  })
  
  # Doses table logic
  output$hotdose <- renderRHandsontable({
    borderRow <- getTableBorderIndex(val$db_dose, val$now_date, T)
    rhandsontable(val$db_dose, useTypes = TRUE, stretchH = "all", rowHeaders = NULL,
                  colHeaders = c("Date", "Time", getDoseColumnLabel(val$model))) %>%
                  hot_col(col="Time", type="dropdown", source=hoursList()) %>%
                  hot_table(customBorders = list(list(
                    range=list(from=list(row=borderRow-1, col=0), to=list(row=borderRow, col=ncol(val$db_dose)-1)),
                    top=list(width=2, color=nowColorHex()))))
  })
  
  observeEvent(input$hotdose, {
    val$db_dose <- autoSortByDate(hot_to_r(input$hotdose))
  })
  
  addDose <- function(val) {
    doseMetadata <- getMetadataByName(val$model, "DOSE")
    dosingInterval <- if(is.null(doseMetadata)) {24} else {doseMetadata$dosing_interval}
    
    if(nrow(val$db_dose) > 0) {
      newdose <- val$db_dose[ nrow(val$db_dose), ]
      lastdose <- dateAndTimeToPOSIX(newdose$date, newdose$time)
      lastdose <- lastdose + dosingInterval*3600
    } else {
      newdose <- data.frame(date="", time="", dose=if(is.null(doseMetadata)) {0} else {doseMetadata$default_value})
      lastdose <- Sys.time()
    }
    newdose$date <- POSIXToDate(lastdose)
    newdose$time <- POSIXToTime(lastdose)
    val$db_dose <- rbind(val$db_dose, newdose)
  }
  
  observeEvent(input$addDose, {
    addDose(val)
  })
  
  # Doses & Recommendations table logic
  observeEvent(input$addDoseFuture, {
    addDose(val)
  })
  
  renderHotDoseFuture <- function(data) {
    borderRow <- getTableBorderIndex(data, val$now_date, T)
    output$hotdosefuture <- renderRHandsontable({
      recColumnLabel <- getRecommendedDoseColumnLabel(val$model)
      rhandsontable(data, useTypes=TRUE, stretchH="all", rowHeaders=NULL, readOnly=FALSE,
                    colHeaders = c("Date", "Time", getDoseColumnLabel(val$model), recColumnLabel)) %>%
        hot_context_menu(allowRowEdit = FALSE, allowColEdit = FALSE ) %>%
        hot_col(col="Time", type="dropdown", source=hoursList()) %>%
        hot_col(col=recColumnLabel, readOnly = TRUE) %>%
        hot_table(customBorders = list(list(
          range=list(from=list(row=borderRow-1, col=0), to=list(row=borderRow, col=ncol(data)-1)),
          top=list(width=2, color=nowColorHex()))))
    })
  }
  
  observeEvent(input$hotdosefuture, {
    val$db_dose <- autoSortByDate(hot_to_r(input$hotdosefuture) %>% select(-rec))
  })
  
  # Covariates table logic
  output$hotcov <- renderRHandsontable({
    covsNames <- colnames(val$db_covs)
    covsNames <- covsNames[!(covsNames %in% c("date", "time"))]
    borderRow <- getTableBorderIndex(val$db_covs, val$now_date, T)
    rhandsontable(val$db_covs, useTypes = TRUE, stretchH = "all", rowHeaders = NULL,
                  colHeaders = c("Date", "Time", covsNames)) %>%
      hot_col(col="Time", type="dropdown", source=hoursList()) %>%
      hot_table(customBorders = list(list(
        range=list(from=list(row=borderRow-1, col=0), to=list(row=borderRow, col=ncol(val$db_covs)-1)),
        top=list(width=2, color=nowColorHex()))))
  })
  
  observeEvent(input$hotcov, {
    val$db_covs <- autoSortByDate(hot_to_r(input$hotcov))
  })
  
  addCovariate <- function(val) {
    doseMetadata <- getMetadataByName(val$model, "DOSE")
    dosingInterval <- if(is.null(doseMetadata)) {24} else {doseMetadata$dosing_interval}
    
    if(nrow(val$db_covs) > 0) {
      newRow <- val$db_covs[ nrow(val$db_covs), ]
      lastRow <- dateAndTimeToPOSIX(newRow$date, newRow$time)
      lastRow <- lastRow + dosingInterval*3600
    } else {
      newRow <- data.frame(date="", time="")
      lastRow <- Sys.time()
    }
    newRow$date <- POSIXToDate(lastRow)
    newRow$time <- POSIXToTime(lastRow)
    val$db_covs <- rbind(val$db_covs, newRow)
  }
  
  observeEvent(input$addCovariate, {
    addCovariate(val)
  })
  
  # 1 - Population prediction
  populationPlot <- reactive({
    progress <- shiny::Progress$new(max = 2)
    on.exit(progress$close())
    progress$set(message = "Preparing...", value = 0.5)
    plots <- preparePredictionPlots(doses=val$db_dose, obs=val$db_obs, model=val$model, covs=val$db_covs, target=val$target, population=T, now=val$now_date)
    progress$set(message = "Rendering plot...", value = 1)
    if(!is.null(plots)) mergePlots(plots$p1, plots$p2, getModelOutput(val$model))
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
    plots <- preparePredictionPlots(doses=val$db_dose, obs=val$db_obs, model=val$model, covs=val$db_covs, target=val$target, population=F, now=val$now_date)
    progress$set(message = "Rendering plot...", value = 1)
    if(!is.null(plots)) mergePlots(plots$p1, plots$p2, getModelOutput(val$model))
  })
  output$fitPlot <- renderPlotly(fitPlot())
  
  # 3 - Recommendation
  recommendationPlot <- reactive({
    progress <- shiny::Progress$new(max = 2)
    on.exit(progress$close())
    progress$set(message = "Preparing...", value = 0.5)
    recommendation <- prepareRecommendation(doses=val$db_dose, obs=val$db_obs, model=val$model, covs=val$db_covs, target=val$target, now=val$now_date)
    recommendedRegimen <- recommendation$recommendedRegimen
    data <- val$db_dose
    data$rec <- ifelse(recommendedRegimen$PAST, "/", round(recommendedRegimen$AMT, 2))
    renderHotDoseFuture(data)
    
    plots <- prepareRecommendationPlots(doses=val$db_dose, obs=val$db_obs, model=val$model, covs=val$db_covs, target=val$target, recommendation=recommendation, now=val$now_date)
    progress$set(message = "Rendering plot...", value = 1)
    if(!is.null(plots)) mergePlots(plots$p1, plots$p2, getModelOutput(val$model))
  })
  output$recommendationPlot <- renderPlotly(recommendationPlot())
  
  
  # Refresh plot when sidebar is collapsed/opened (make sure there is no transition time in CSS, otherwise not working)
  observeEvent(input$sidebarCollapse, {
    if (val$plot_type == "population") {
      output$populationPlot <- renderPlotly(populationPlot())
    } else if(val$plot_type == "fit") {
      output$fitPlot <- renderPlotly(fitPlot())
    } else if(val$plot_type == "recommendation") {
      output$recommendationPlot <- renderPlotly(recommendationPlot())
    }
    if (is.null(val$collapsed) || val$collapsed==F) {
      updateActionButton(session, ns("sidebarCollapse"), label=NULL, icon=icon("plus-square"))
      val$collapsed <- T
    } else {
      updateActionButton(session, ns("sidebarCollapse"), label=NULL, icon=icon("minus-square"))
      val$collapsed <- F
    }
  })
}