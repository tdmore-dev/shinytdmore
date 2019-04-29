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
          column(10, editableInput(inputId="nowDate", type = "combodate", value="2019-04-29 12:00"), style="margin-top: 6px;")
        ),
        hr(),
        h4("Target"),
        numericInput("targetDown", "Lower limit", 10),
        numericInput("targetUp", "Upper limit", 15)
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
  
  # Update tab title according to patient's name
  output$tab_title <- renderText({return(paste(val$patient$firstname, val$patient$lastname))})
  
  observe({
    # Update `regimen` based on changes in db_dose
    pos <- dateAndTimeToPOSIX(val$db_dose$date, val$db_dose$time)
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
    if(nrow(val$db_dose) > 0) {
      newdose <- val$db_dose[ nrow(val$db_dose), ]
      lastdose <- dateAndTimeToPOSIX(newdose$date, newdose$time)
      lastdose <- lastdose + 12*60*60
    } else {
      newdose <- data.frame(date=NULL, time=NULL, dose=5)
      lastdose <- now()
    }
    newdose$date <- format(lastdose, "%Y-%m-%d")
    newdose$time <- format(lastdose, "%H:%M")
    
    val$db_dose <- rbind(val$db_dose, newdose)
  })
  
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
    recommendation <- prepareRecommendation(doses=val$db_dose, obs=val$db_obs, model=val$model, covs=val$covs, target=target)

    temp_df <- val$db_dose
    temp_df$rec <- "/"
    temp_df <- rbind(temp_df, recommendation$recommendedRegimenFiltered %>%
                     dplyr::transmute(date=as.Date(TIME), time=strftime(TIME,"%H:%M"), dose="/", rec=round(AMT, digits=2)))

    output$hotdosefuture <- renderRHandsontable({
      rhandsontable(temp_df, useTypes=TRUE, stretchH="all", rowHeaders=NULL, readOnly=TRUE,
                    colHeaders = c("Date", "Time", getDoseColumnLabel(val$model), getRecommendedDoseColumnLabel(val$model))) %>%
        hot_context_menu(allowRowEdit = FALSE, allowColEdit = FALSE ) %>%
        hot_col(col="Time", type="dropdown", source=hoursList())
    })
    
    plots <- prepareRecommendationPlots(doses=val$db_dose, obs=val$db_obs, model=val$model, covs=val$covs, target=target, recommendation=recommendation)
    progress$set(message = "Rendering plot...", value = 1)
    if(!is.null(plots)) mergePlots(plots$p1, plots$p2)
  })
  output$recommendationPlot <- renderPlotly(recommendationPlot())
}