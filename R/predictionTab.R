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
        fluidRow(
          column(10, h4("Measures")),
          column(2, actionButton("addObs", "Add", style="float:right"))
        ),
        rHandsontableOutput('hotobs'),
        hr(),
        conditionalPanel(
          condition = "output.plot_type == 'population' || output.plot_type == 'fit'",
          fluidRow(
            column(10, h4("Doses")),
            column(2, actionButton("addDose", "Add", style="float:right"))
          ),
          rHandsontableOutput('hotdose')
        ),
        conditionalPanel(
          condition = "output.plot_type == 'recommendation'",
          fluidRow(
            column(10, h4("Doses & Recommendations"))
          ),
          rHandsontableOutput('hotdosefuture')
        ),
        fluidRow(
          column(2, h5("Now:")),
          column(10, editableInput(inputId="nowDate", type = "combodate", value="2000-01-01 00:00"), style="margin-top: 6px;")
        ),
        hr(),
        h4("Target"),
        numericInput("targetDown", "Lower limit", 0),
        numericInput("targetUp", "Upper limit", 0)
      ),
      column(
        9,
        fluidRow(
          column(1, actionButton("previous_plot", label="Previous", icon=icon("backward"))),
          tags$head(tags$style(HTML('#previous_plot{background-color:#dde5eb}'), '#previous_plot.attr("disabled", "true")')),
          
          column(10, textOutput(outputId="plot_title"),
          tags$head(tags$style("#plot_title{font-size: 20px;text-align: center;justify-content: center;}"))),
          
          column(1, actionButton("next_plot", label="Next", icon=icon("forward"), style="float:right")),
          tags$head(tags$style(HTML('#next_plot{background-color:#dde5eb}')))
        ),
        conditionalPanel(condition = "output.plot_type == 'population'",
                         plotlyOutput('populationPlot', height = "600px")),
        
        conditionalPanel(condition = "output.plot_type == 'fit'",
                         plotlyOutput('fitPlot', height = "600px")),
        
        conditionalPanel(condition = "output.plot_type == 'recommendation'",
                         plotlyOutput('recommendationPlot', height = "600px"))
      )
    ),
    tags$head(tags$style(HTML(".handsontable {overflow-x:hidden;}"), "#tab_title{font-size: 30px; margin-top: 10px; margin-bottom: 10px;}")),
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
      $("#nowDate").editable("setValue", message);
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
#'
previousNextLogic <- function(input, output, session, val) {
  plotTypes <- c("population", "fit", "recommendation")
  plotTitles <- c("Population prediction", "Individual prediction", "Recommendation")

  enableDisableButtons <- function(plotTypeIndex) {
    if (plotTypeIndex <= 1) {
      session$sendCustomMessage("disableButton", "previous_plot")
    } else {
      session$sendCustomMessage("enableButton", "previous_plot")
    }
    if (plotTypeIndex >= 3) {
      session$sendCustomMessage("disableButton", "next_plot")
    } else {
      session$sendCustomMessage("enableButton", "next_plot")
    }
  }
  
  # Necessary call for the previous button (should be disabled already at the beginning)
  session$sendCustomMessage("disableButton", "previous_plot")
  
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
  
  outputOptions(output, "plot_type", suspendWhenHidden = FALSE)
}

forceUpdateNowDate <- function(session, val, date) {
  minute <- lubridate::minute(date) %/% 5
  value <- paste0(format(date, format = "%Y-%m-%d %H"), ":", pad(minute*5))
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
#' @importFrom lubridate ymd_hm
#'
nowDateLogic <- function(input, output, session, val) {
  observeEvent(input$nowDate, {
    defaultDateInUI <- "2000-01-01 00:00"
    if (input$nowDate != val$now_date && input$nowDate != defaultDateInUI) {
      forceUpdateNowDate(session, val, lubridate::ymd_hm(input$nowDate, tz = Sys.timezone()))
    }
  })
  observeEvent(val$set_patient_counter, {
    forceUpdateNowDate(session, val, val$now_date)
  })
}

#'
#' Target logic.
#'
#' @param input shiny input
#' @param output shiny output
#' @param session shiny session
#' @param val main reactive container
#'
targetLogic <- function(input, output, session, val) {
  observeEvent(input$targetDown, {
    val$target$min <- input$targetDown
  })
  observeEvent(input$targetUp, {
    val$target$max <- input$targetUp
  })
  observeEvent(val$set_patient_counter, {
    updateNumericInput(session=session, inputId="targetDown", value=val$target$min)
    updateNumericInput(session=session, inputId="targetUp", value=val$target$max)
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
predictionTabServer <- function(input, output, session, val) {

  # Previous/Next button logic
  previousNextLogic(input, output, session, val)
  
  # Now date logic
  nowDateLogic(input, output, session, val)
  
  # Target logic
  targetLogic(input, output, session, val)
  
  # Update tab title according to patient's name
  output$tab_title <- renderText({return(paste(val$patient$firstname, val$patient$lastname))})
  
  output$hotdose <- renderRHandsontable({
    rhandsontable(val$db_dose, useTypes = TRUE, stretchH = "all", rowHeaders = NULL,
      colHeaders = c("Date", "Time", getDoseColumnLabel(val$model))) %>%
      hot_context_menu(allowRowEdit = TRUE, allowColEdit = FALSE) %>%
      hot_col(col="Time", type="dropdown", source=hoursList())
  })
  observeEvent(input$hotdose, {
    val$db_dose <- hot_to_r(input$hotdose)
  })
  
  observeEvent(input$addDose, {
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
    newdose$date <- format(lastdose, "%Y-%m-%d")
    newdose$time <- format(lastdose, "%H:%M")
    val$db_dose <- rbind(val$db_dose, newdose)
  })
  
  output$hotobs <- renderRHandsontable({
    if (!is.null(val$db_obs))
      rhandsontable(val$db_obs, useTypes = TRUE, stretchH = "all", rowHeaders = NULL,
        colHeaders = c("Date", "Time", getMeasureColumnLabel(val$model), "Use")) %>% hot_col("Use", halign = "htCenter")
  })
  
  observeEvent(input$hotobs, {
    if (!is.null(input$hotobs)) {
      val$db_obs = hot_to_r(input$hotobs)
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
    newobs$date <- format(lastobs, "%Y-%m-%d")
    newobs$time <- format(lastobs, "%H:%M")
    val$db_obs <- rbind(val$db_obs, newobs)
  })
  
  # 1 - Population prediction
  populationPlot <- reactive({
    progress <- shiny::Progress$new(max = 2)
    on.exit(progress$close())
    progress$set(message = "Preparing...", value = 0.5)
    plots <- preparePredictionPlots(doses=val$db_dose, obs=val$db_obs, model=val$model, covs=val$covs, target=val$target, population=T, now=val$now_date)
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
    plots <- preparePredictionPlots(doses=val$db_dose, obs=val$db_obs, model=val$model, covs=val$covs, target=val$target, population=F, now=val$now_date)
    progress$set(message = "Rendering plot...", value = 1)
    if(!is.null(plots)) mergePlots(plots$p1, plots$p2)
  })
  output$fitPlot <- renderPlotly(fitPlot())
  
  # 3 - Recommendation
  recommendationPlot <- reactive({
    progress <- shiny::Progress$new(max = 2)
    on.exit(progress$close())
    progress$set(message = "Preparing...", value = 0.5)
    recommendation <- prepareRecommendation(doses=val$db_dose, obs=val$db_obs, model=val$model, covs=val$covs, target=val$target, now=val$now_date)
    recommendedRegimen <- recommendation$recommendedRegimen
    temp_df <- val$db_dose
    temp_df$rec <- ifelse(recommendedRegimen$PAST, "/", round(recommendedRegimen$AMT, 2))

    output$hotdosefuture <- renderRHandsontable({
      rhandsontable(temp_df, useTypes=TRUE, stretchH="all", rowHeaders=NULL, readOnly=TRUE,
                    colHeaders = c("Date", "Time", getDoseColumnLabel(val$model), getRecommendedDoseColumnLabel(val$model))) %>%
        hot_context_menu(allowRowEdit = FALSE, allowColEdit = FALSE ) %>%
        hot_col(col="Time", type="dropdown", source=hoursList())
    })
    
    plots <- prepareRecommendationPlots(doses=val$db_dose, obs=val$db_obs, model=val$model, covs=val$covs, target=val$target, recommendation=recommendation, now=val$now_date)
    progress$set(message = "Rendering plot...", value = 1)
    if(!is.null(plots)) mergePlots(plots$p1, plots$p2)
  })
  output$recommendationPlot <- renderPlotly(recommendationPlot())
}