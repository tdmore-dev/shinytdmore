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
        shinyBS::bsCollapse(id=ns("bsCollapse"), multiple=T, open=c("Doses", "Measures", "Now"),
          shinyBS::bsCollapsePanel(title="Doses", style="primary",
            conditionalPanel(
              condition = "output.plot_type == 'population' || output.plot_type == 'fit'",
              rhandsontable::rHandsontableOutput(ns("hotdose")),
              actionButton(ns("addDose"), "Add dose", style="margin-top: 5px;"),
              ns=ns
            ),
            conditionalPanel(
              condition = "output.plot_type == 'recommendation'",
              rhandsontable::rHandsontableOutput(ns("hotdosefuture")),
              actionButton(ns("addDoseFuture"), "Add dose", style="margin-top: 5px;"),
              ns=ns
            )
          ),
          shinyBS::bsCollapsePanel(title="Measures", style="primary",
            rhandsontable::rHandsontableOutput(ns("hotobs")),
            actionButton(ns("addObs"), "Add measure", style="margin-top: 5px;")
          ),
          shinyBS::bsCollapsePanel(title="Now", style="primary",
            editableInput(inputId=ns("nowDate"), type = "combodate", value="2000-01-01 00:00")
          ),
          shinyBS::bsCollapsePanel(title="Covariates", style="primary",
            conditionalPanel(
              condition = "output.display_covariates == true",
              rhandsontable::rHandsontableOutput(ns("hotcov")),
              actionButton(ns("addCovariate"), "Add covariate", style="margin-top: 5px;"),
              ns=ns
            ),
            conditionalPanel(
              condition = "output.display_covariates == false",
              h5("No covariate in model"),
              ns=ns
            )
          ),
          shinyBS::bsCollapsePanel(title="Target", style="primary",
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
                        plotly::plotlyOutput(ns("populationPlot"), height="680px", width="100%"), ns=ns),

       conditionalPanel(condition = "output.plot_type == 'fit'",
                        plotly::plotlyOutput(ns("fitPlot"), height="800px", width="100%"), ns=ns),

       conditionalPanel(condition = "output.plot_type == 'recommendation'",
                        plotly::plotlyOutput(ns("recommendationPlot"), height="680px", width="100%"), ns=ns)
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
#' @param outputReact reactive value for the plot type
#' @param ns namespace
#'
previousNextLogic <- function(input, output, session, val, outputReact, ns) {
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
    outputReact$plot_type <- "population"
    enableDisableButtons(1)
  })
  
  observeEvent(input$previous_plot, {
    plotTypeIndex <- which(plotTypes==outputReact$plot_type) - 1
    if (plotTypeIndex >= 1) {
      outputReact$plot_type <- plotTypes[plotTypeIndex]
    }
    enableDisableButtons(plotTypeIndex)
  })
  
  observeEvent(input$next_plot, {
    plotTypeIndex <- which(plotTypes==outputReact$plot_type) + 1
    if (plotTypeIndex <= length(plotTypes)) {
      outputReact$plot_type <- plotTypes[plotTypeIndex]
    }
    enableDisableButtons(plotTypeIndex)
  })
  
  output$plot_title <- renderText({
    return(plotTitles[which(plotTypes==outputReact$plot_type)])
  })
  
  output$plot_type <- reactive({
    return(outputReact$plot_type)
  })
  
  outputOptions(output, "plot_type", suspendWhenHidden=F)
}

forceUpdateNowDate <- function(session, val, date, ns) {
  minute <- lubridate::minute(date) %/% 5
  value <- paste0(format(date, format = paste(getDateFormat(), "%H"), tz=getAppTimeZone()), ":", pad(minute*5))
  updateTextInput(session=session, inputId="nowDate", value=value) # This updates the 'visual' text
  session$sendCustomMessage("nowDate", value) # This update the javascript value in the date picket
  val$now <- date
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
    if (input$nowDate != val$now && input$nowDate != defaultDateInUI) {
      forceUpdateNowDate(session, val, lubridate::ymd_hm(input$nowDate, tz = Sys.timezone()), ns)
    }
  })
  observeEvent(val$set_patient_counter, {
    forceUpdateNowDate(session, val, val$now, ns)
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
  
  outputReact <- reactiveValues()
  outputReact$plot_type <- "population"
  
  outputOptions(output, "display_covariates", suspendWhenHidden=F)
  
  # Previous/Next button logic
  previousNextLogic(input, output, session, val, outputReact, ns)
  
  # Now date logic
  nowDateLogic(input, output, session, val, ns)
  
  # Target logic
  targetLogic(input, output, session, val, ns)
  
  # Update tab title according to patient's name
  output$tab_title <- renderText({return(paste(val$patient$firstname, val$patient$lastname))})
  
  # Observations/Measures table logic
  output$hotobs <- rhandsontable::renderRHandsontable({
    borderRow <- getTableBorderIndex(val$obs, val$now, F)
    rhandsontable(val$obs, useTypes = TRUE, stretchH = "all", rowHeaders = NULL,
                  colHeaders = c("Date", "Time", getMeasureColumnLabel(val$model), "Use")) %>%
      rhandsontable::hot_col("Use", halign = "htCenter") %>%
      rhandsontable::hot_col(col="Time", type="dropdown", source=hoursList()) %>%
      rhandsontable::hot_table(customBorders = list(list(
                    range=list(from=list(row=borderRow-1, col=0), to=list(row=borderRow, col=ncol(val$obs)-1)),
                    top=list(width=2, color=nowColorHex()))))
  })
  
  observeEvent(input$hotobs, {
    if (!is.null(input$hotobs)) {
      val$obs <- autoSortByDate(rhandsontable::hot_to_r(input$hotobs))
    }
  })
  
  observeEvent(input$addObs, {
    if(nrow(val$obs) > 0) {
      newobs <- val$obs[ nrow(val$obs), ]
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
    val$obs <- rbind(val$obs, newobs)
  })
  
  # Doses table logic
  output$hotdose <- renderRHandsontable({
    borderRow <- getTableBorderIndex(val$doses, val$now, T)
    rhandsontable::rhandsontable(val$doses, useTypes = TRUE, stretchH = "all", rowHeaders = NULL,
                  colHeaders = c("Date", "Time", getDoseColumnLabel(val$model))) %>%
      rhandsontable::hot_col(col="Time", type="dropdown", source=hoursList()) %>%
      rhandsontable::hot_table(customBorders = list(list(
                    range=list(from=list(row=borderRow-1, col=0), to=list(row=borderRow, col=ncol(val$doses)-1)),
                    top=list(width=2, color=nowColorHex()))))
  })
  
  observeEvent(input$hotdose, {
    val$doses <- autoSortByDate(rhandsontable::hot_to_r(input$hotdose))
  })
  
  addDose <- function(val) {
    doseMetadata <- getMetadataByName(val$model, "DOSE")
    dosingInterval <- if(is.null(doseMetadata)) {24} else {doseMetadata$dosing_interval}
    
    if(nrow(val$doses) > 0) {
      newdose <- val$doses[ nrow(val$doses), ]
      lastdose <- dateAndTimeToPOSIX(newdose$date, newdose$time)
      lastdose <- lastdose + dosingInterval*3600
    } else {
      newdose <- data.frame(date="", time="", dose=if(is.null(doseMetadata)) {0} else {doseMetadata$default_value})
      lastdose <- Sys.time()
    }
    newdose$date <- POSIXToDate(lastdose)
    newdose$time <- POSIXToTime(lastdose)
    val$doses <- rbind(val$doses, newdose)
  }
  
  observeEvent(input$addDose, {
    addDose(val)
  })
  
  # Doses & Recommendations table logic
  observeEvent(input$addDoseFuture, {
    addDose(val)
  })
  
  renderHotDoseFuture <- function(data) {
    borderRow <- getTableBorderIndex(data, val$now, T)
    output$hotdosefuture <- rhandsontable::renderRHandsontable({
      recColumnLabel <- getRecommendedDoseColumnLabel(val$model)
      rhandsontable::rhandsontable(data, useTypes=TRUE, stretchH="all", rowHeaders=NULL, readOnly=FALSE,
                    colHeaders = c("Date", "Time", getDoseColumnLabel(val$model), recColumnLabel)) %>%
        rhandsontable::hot_context_menu(allowRowEdit = FALSE, allowColEdit = FALSE ) %>%
        rhandsontable::hot_col(col="Time", type="dropdown", source=hoursList()) %>%
        rhandsontable::hot_col(col=recColumnLabel, readOnly = TRUE) %>%
        rhandsontable::hot_table(customBorders = list(list(
          range=list(from=list(row=borderRow-1, col=0), to=list(row=borderRow, col=ncol(data)-1)),
          top=list(width=2, color=nowColorHex()))))
    })
  }
  
  observeEvent(input$hotdosefuture, {
    val$doses <- autoSortByDate(rhandsontable::hot_to_r(input$hotdosefuture) %>% dplyr::select(-rec))
  })
  
  # Covariates table logic
  output$hotcov <- rhandsontable::renderRHandsontable({
    covsNames <- colnames(val$covs)
    covsNames <- covsNames[!(covsNames %in% c("date", "time"))]
    borderRow <- getTableBorderIndex(val$covs, val$now, T)
    rhandsontable::rhandsontable(val$covs, useTypes = TRUE, stretchH = "all", rowHeaders = NULL,
                  colHeaders = c("Date", "Time", covsNames)) %>%
      rhandsontable::hot_col(col="Time", type="dropdown", source=hoursList()) %>%
      rhandsontable::hot_table(customBorders = list(list(
        range=list(from=list(row=borderRow-1, col=0), to=list(row=borderRow, col=ncol(val$covs)-1)),
        top=list(width=2, color=nowColorHex()))))
  })
  
  observeEvent(input$hotcov, {
    val$covs <- autoSortByDate(rhandsontable::hot_to_r(input$hotcov))
  })
  
  addCovariate <- function(val) {
    doseMetadata <- getMetadataByName(val$model, "DOSE")
    dosingInterval <- if(is.null(doseMetadata)) {24} else {doseMetadata$dosing_interval}
    
    if(nrow(val$covs) > 0) {
      newRow <- val$covs[ nrow(val$covs), ]
      lastRow <- dateAndTimeToPOSIX(newRow$date, newRow$time)
      lastRow <- lastRow + dosingInterval*3600
    } else {
      newRow <- data.frame(date="", time="")
      lastRow <- Sys.time()
    }
    newRow$date <- POSIXToDate(lastRow)
    newRow$time <- POSIXToTime(lastRow)
    val$covs <- rbind(val$covs, newRow)
  }
  
  observeEvent(input$addCovariate, {
    addCovariate(val)
  })
  
  # Use the debounce function to decrease the number of redraws
  # This also fixes the annoying bug we have when the add buttons are clicked several times within a short time
  react <- debounce(r=reactive(list(doses=val$doses, obs=val$obs, covs=val$covs, target=val$target, now=val$now, model=val$model)), millis=1000)
  
  isUIReady <- function() {
    return(!is.null(react()$obs))
  }
  
  # 1 - Population prediction
  populationPlot <- reactive({
    if (!isUIReady()) return()
    progress <- shiny::Progress$new(max = 2)
    on.exit(progress$close())
    progress$set(message = "Preparing...", value = 0.5)
    plots <- preparePredictionPlots(doses=react()$doses, obs=react()$obs, model=react()$model,
                                    covs=react()$covs, target=react()$target, population=T, now=react()$now)
    progress$set(message = "Rendering plot...", value = 1)
    mergePlots(plots$p1, plots$p2, plots$p3, getModelOutput(react()$model))
  })
  output$populationPlot <- plotly::renderPlotly(populationPlot())
  
  # 2 - Individual prediction
  fitPlot <- reactive({
    if (!isUIReady()) return()
    progress <- shiny::Progress$new(max = 2)
    on.exit(progress$close())
    progress$set(message = "Preparing...", value = 0.5)
    plots <- preparePredictionPlots(doses=react()$doses, obs=react()$obs, model=react()$model,
                                    covs=react()$covs, target=react()$target, population=F, now=react()$now)
    progress$set(message = "Rendering plot...", value = 1)
    mergePlots(plots$p1, plots$p2, plots$p3, getModelOutput(react()$model))
  })
  output$fitPlot <- plotly::renderPlotly(fitPlot())
  
  # 3 - Recommendation
  recommendationPlot <- reactive({
    if (!isUIReady()) return()
    progress <- shiny::Progress$new(max = 2)
    on.exit(progress$close())
    progress$set(message = "Preparing...", value = 0.5)
    recommendation <- prepareRecommendation(doses=react()$doses, obs=react()$obs, model=react()$model,
                                            covs=react()$covs, target=react()$target, now=react()$now)
    recommendedRegimen <- recommendation$recommendedRegimen
    data <- react()$doses
    data$rec <- ifelse(recommendedRegimen$PAST, "/", round(recommendedRegimen$AMT, 2))
    renderHotDoseFuture(data)
    
    plots <- prepareRecommendationPlots(doses=react()$doses, obs=react()$obs, model=react()$model,
                                        covs=react()$covs, target=react()$target, recommendation=recommendation, now=react()$now)
    progress$set(message = "Rendering plot...", value = 1)
    mergePlots(plots$p1, plots$p2, plots$p3, getModelOutput(react()$model))
  })
  output$recommendationPlot <- plotly::renderPlotly(recommendationPlot())
  
  
  # Refresh plot when sidebar is collapsed/opened (make sure there is no transition time in CSS, otherwise not working)
  observeEvent(input$sidebarCollapse, {
    if (outputReact$plot_type == "population") {
      output$populationPlot <- plotly::renderPlotly(populationPlot())
    } else if(outputReact$plot_type == "fit") {
      output$fitPlot <- plotly::renderPlotly(fitPlot())
    } else if(outputReact$plot_type == "recommendation") {
      output$recommendationPlot <- plotly::renderPlotly(recommendationPlot())
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