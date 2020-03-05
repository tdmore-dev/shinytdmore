#'
#' Update an existing plot with new data using animation.
#' 
#' @param plot the given plot
#' @param outputId the output ID
#' @param ignoreAttrs attributes to ignore, i.e. to not update
#' @param ignoreDataAttrs data attributes to ignore
#' @param options options to perform the animation
#'
updatePlot <- function(outputId, plot, 
                       ignoreAttrs=c("attrs", "shinyEvents", "highlight", "config", "source", "visdat", "base_url", "cur_data"),
                       ignoreDataAttrs=c("text", "hoveron", "name", "legendgroup", "showlegend", "xaxis", "yaxis", "hoverinfo", "frame"),
                       options=list(transition=list(duration=1000, easing='cubic-in-out'), frame=list(duration=1000))
                       ) {
  p2 <- plot %>% plotly::plotly_build()
  p2 <- p2$x
  p2$layout <- list(datarevision=round(runif(1)*1000))
  p2[ignoreAttrs] <- NULL
  p2$data <- lapply(p2$data, function(x){
    x[ignoreDataAttrs] <- NULL
    x} )
  ## see https://codepen.io/plotly/pen/ZpWPpj
  ## and https://codepen.io/etpinard/pen/KrpMQa
  ## and also https://plot.ly/javascript/plotlyjs-function-reference/#plotlyreact
  plot_json <- attr(p2, "TOJSON_FUNC")(p2)
  proxy <- plotly::plotlyProxy(outputId)
  plotly::plotlyProxyInvoke(
    proxy, "animate", plot_json,
    options
  )
  plot #new plot
}

repaintDataOnly <- function() {
  function(old,new) {
    a <- plotly::plotly_build(old)
    a <- a$x
    b <- plotly::plotly_build(new)
    b <- b$x
    
    aLayout <- a$layout
    aLayout <- aLayout[ setdiff( names(aLayout), c("xaxis", "yaxis", "yaxis2", "yaxis3", "margin")) ]
    bLayout <- b$layout
    bLayout <- bLayout[ setdiff( names(bLayout), c("xaxis", "yaxis", "yaxis2", "yaxis3", "margin")) ]
    
    if( !isTRUE( all.equal(aLayout, bLayout) )) {
      cat("REPAINT\n")
      print(all.equal(aLayout, bLayout) )
      return(TRUE) #repaint required
    }
    cat("No repaint needed\n")
    FALSE
  }
}

renderUpdatePlotly <- function(output, outputId, expr, repaint=repaintDataOnly(), ..., label=paste0("output$",outputId)) {
  log <- function(...) {
    cat(label, "::", ...)
  }
  
  fun <- NULL #avoid warnings from CMD CHECK
  shiny::installExprFunction(substitute(expr), quoted=TRUE, eval.env=parent.frame(), name="fun")
  
  r <- reactive({
    log("Calculating reactive plot\n")
    plot <- captureStackTraces(fun())
    x <- plotly::plotly_build(plot)
    ## See https://codepen.io/plotly/pen/ZpWPpj; do not simplify lines when animating
    x$x$data <- lapply(x$x$data, function(x) {
      if(!is.null(x$line)) x$line <- c(x$line, list(simplify=FALSE))
      x
    })
    x
    
  }, label=paste0(label, "::Plot"))
  
  val <- NULL
  initPlot <- reactiveVal(0, label=paste0(label, "::InitPlot"))
  
  
  session <- getDefaultReactiveDomain()
  
  # update handler
  handler <- observeEvent(r(), {
    shiny::captureStackTraces({
      log("Should plot repaint? ")
      if(inherits(val, "try-error")) {
        log("INITIALIZE, previous plot was try-error!\n")
        initPlot(isolate({initPlot()})+1) #update by initializing
      } else if (repaint(val, r())) {
        log("INITIALIZE, repaint required!\n")
        initPlot(isolate({initPlot()})+1)
      } else {
        log("UPDATE sufficient\n")
        val <<- updatePlot(outputId, r(), ...)
      }
    })
  }, ignoreInit=TRUE, suspended=TRUE, label=paste0(label, "::UpdatePlotHandler"))
  #standard output renderers are suspended when the output is hidden
  #we do the same here, manually
  hidden <- function() {
    id <- paste0("output_", session$ns(outputId), "_hidden")
    value <- session$clientData[[ id ]]
    if(is.null(value)) return(TRUE)
    return(value)
  }
  observe({
    if( hidden() ) {
      log("Plot hidden, handler should suspend\n")
      handler$suspend() 
    } else {
      log("Plot active, handler should resume\n")
      handler$resume()
    }
  }, label=paste0(label, "::SuspendWhenHidden"))
  output[[outputId]] <- plotly::renderPlotly({
    initPlot()
    val <<- isolate({ try( r() ) })
    if(inherits(val, "try-error")) {
      r() #establish an actual link to the reactive, because the plot will be considered "hidden"
      stop(attr(val, "condition"))
    }
    
    val
  })
}

#' Get a nice Y-axis label.
#' 
#' @param model tdmore model
#' @return a label
#'
getYAxisLabel <- function(model) {
  output <- getModelOutput(model)
  outputMetadata <- tdmore::getMetadataByName(model, output)
  label <- if(!is.null(outputMetadata)) {toString(outputMetadata)} else {"Concentration"}
  return(label)
}

#'
#' Get measure column label.
#' 
#' @param model tdmore model
#' @param breakLine breakline or just space before the unit
#' @return a label
#'
getMeasureColumnLabel <- function(model, breakLine=T) {
  separator <- if(breakLine){"\n"} else{" "}
  outputMetadata <- tdmore::getMetadataByName(model, getModelOutput(model))
  label <- if(!is.null(outputMetadata)) {paste0("Measure", separator, "(", outputMetadata$unit, ")")} else {"Measure"}
  return(label)
}

#'
#' Merge plots.
#' 
#' @param p1 first plot
#' @param p2 second plot
#' @param p3 third plot
#' @param output main tdmore output
#' @param source source to pass to ggplotly as event source
#'
mergePlots <- function(p1, p2, p3, output, source) {
  tooltip1 <- c("TIME", output, paste0(output, ".lower"), paste0(output, ".upper"))
  tooltip2 <- c("TIME", "AMT")
  #tooltip3 <- c("TIME", "Parameter", "Population", "Individual", "Change")

  if (is.null(p3)) {
    plot <- plotly::subplot(
      plotly::ggplotly(p1, tooltip=tooltip1, source=source, dynamicTicks=TRUE) %>% 
        plotly::config(scrollZoom=T, displayModeBar=F, displaylogo=F),
      plotly::ggplotly(p2, tooltip=tooltip2, source=source, dynamicTicks=TRUE) %>% 
        plotly::config(scrollZoom=T, displayModeBar=F, displaylogo=F),
      nrows = 2, heights = c(0.8, 0.2), widths = c(1), shareX=T, shareY=F, titleX=T, titleY=T
    ) %>% plotly::layout(dragmode = "pan", autosize=TRUE )
  } else {
    plot <- plotly::subplot(
      plotly::ggplotly(p1, tooltip=tooltip1, source=source, dynamicTicks=TRUE) %>% 
        plotly::config(scrollZoom=T, displayModeBar=F, displaylogo=F),
      plotly::ggplotly(p2, tooltip=tooltip2, source=source, dynamicTicks=TRUE) %>% 
        plotly::config(scrollZoom=T, displayModeBar=F, displaylogo=F),
      plotly::ggplotly(p3, source=source, dynamicTicks=TRUE) %>% 
        plotly::config(scrollZoom=T, displayModeBar=F, displaylogo=F),
      nrows = 3, heights = c(0.7, 0.15, 0.15), widths = c(1), shareX=T, shareY=F, titleX=T, titleY=T
    ) %>% plotly::layout(dragmode = "pan", legend = list(orientation = "h", y=-250), autosize=TRUE )
  }
  
  return(plot)
}

#'
#' Add now label ('Past' and 'Future') and X intercept to an existing plot.
#' 
#' @param plot a plot
#' @param now now date
#' 
#' @return a new plot with the X intercept and 'past' and 'future' labels
#'
addNowLabelAndIntercept <- function(plot, now) {
  if(is.null(now) || is.na(now)) return(plot)
  
  xintercept <- as.POSIXct(now) # Must be POSIXct for plotly
  
  # Add X intercept (bug in plotly: as.numeric has to be called on the POSIX date)
  plot <- plot +
    geom_vline(xintercept=as.numeric(xintercept), color=nowColor(), size=0.5, alpha=0.3)
  
  # Add 'Past' and 'Future' label close to the X intercept
  yUpperRange <- layer_scales(plot)$y$range$range[2] #plotly ignores y=Inf
  shift <- 3600*8 #shift 8 hours; plotly ignores hjust parameter
  
  plot <- plot + 
    geom_text(aes(x=xintercept, y=yUpperRange, label=c()), label="Past", color=nowColor(), nudge_x=-shift, size=4) +
    geom_text(aes(x=xintercept, y=yUpperRange, label=c()), label="Future", color=nowColor(), nudge_x=shift, size=4)
  return(plot)
}

#'
#' Prepare population OR individual prediction plots.
#' This includes the prediction plot and the timeline plot.
#' 
#' @inheritParams shinytdmore-plot
#' @inheritParams shinytdmore-data
#' @return a list of plots (p1, p2, p3)
#' @export
#'
preparePredictionPlots <- function(populationPredict, individualPredict, observed, target, model, now, regimen) {
  regimen <- regimen %||% defaultData()$regimen
  observed <- observed %||% defaultData()$observed
  
  predictionList <- list(individualPredict, populationPredict) %>%
    stats::setNames(c(ipredColor(), predColor())) %>%
    purrr::compact()
  
  z <- list(
    p1=preparePredictionPlot(predictionList, observed=observed, target=target, model=model, now=now),
    p2=prepareTimelinePlot(populationPredict, individualPredict, regimen=regimen, recommendedRegimen=NULL, model=model, now=now),
    p3=prepareParametersPlot(populationPredict, individualPredict, parameters=tdmore::getObservedVariables(model))
  )
  #return(list(p1=preparePredictionPlot(data=data, obs=obs, target=target, population=population, model=selectedModel, now=now),
  z
}

#'
#' Prepare the prediction plot.
#' 
#' @param predictList list of prediction data.frames. The names determine the color that is plotted.
#' The first element is plotted with uncertainty interval, the rest is not.
#' @inheritParams shinytdmore-plot
#' @inheritParams shinytdmore-data
#'
preparePredictionPlot <- function(predictList, observed, target, model, now) {
  data <- predictList[[1]]
  
  color <- names(predictList[1])
  observed <- observed %||% defaultData()$observed
  data <- data %>% dplyr::mutate_if(is.numeric, round, 2) # Round dataframe for better hover tooltips
  
  if(is.null(target) || all(is.na(target))) {
    target <- tdmore::getMetadataByClass(model, "tdmore_target")
  }
  if(is.null(target)) target <- list(min=as.numeric(NA), max=as.numeric(NA))
  ggplotTarget <- tibble(lower=target$min, upper=target$max)
  defaultModel <- getDefaultModel(model)
  output <- getModelOutput(defaultModel)
  
  plot <- ggplot(mapping=aes_string(x="TIME", y=output))
  if (length(predictList) > 1) {
    for(i in seq(2, length(predictList))) {
      plot <- plot + geom_line(data=predictList[[i]], color=names(predictList[i]), alpha=0.25)
    }
  }
  
  plot <- plot +
    geom_line(data=data, color=color) +
    geom_hline(data=ggplotTarget, aes_string(yintercept="lower"), color=targetColor(), lty=2) +
    geom_hline(data=ggplotTarget, aes_string(yintercept="upper"), color=targetColor(), lty=2) +
    labs(y=getYAxisLabel(defaultModel))
  
  if(nrow(observed) > 0) #only add data if observed
    plot <- plot + geom_point(data=observed, aes_string(x="time", y="dv"), color=ifelse(observed$use, samplesColor(), samplesColorFuture()), shape=4, size=3)
  
  ribbonLower <- paste0(output, ".lower")
  ribbonUpper <- paste0(output, ".upper")
  if ((ribbonLower %in% colnames(data)) && (ribbonUpper %in% colnames(data))) {
    plot <- plot + geom_ribbon(fill=color, aes_string(ymin=ribbonLower, ymax=ribbonUpper), data=data, alpha=0.1)
  }
  
  plot <- addNowLabelAndIntercept(plot, now)
  
  return(plot)
}

#'
#' Prepare the timeline.
#' 
#' @inheritParams shinytdmore-plot
#' @inheritParams shinytdmore-data
#' 
#' @details The individual and population prediction dataframes are only required to ensure the same X axis format.
#'
prepareTimelinePlot <- function(populationPredict, individualPredict, regimen, recommendedRegimen, model, now) {
  timeRange <- range(populationPredict$TIME)
  regimen <- dplyr::bind_rows(regimen, tibble(time=timeRange, dose=NA))
  if(!is.null(recommendedRegimen)) recommendedRegimen <- dplyr::bind_rows(recommendedRegimen, tibble(time=timeRange, dose=NA))
  
  p2 <- ggplot(regimen, aes_(x=~time, y=~dose)) +
    geom_text(aes_(label=~dose)) +
    geom_linerange(ymin=0, aes_(ymax=~dose)) +
    #coord_cartesian(ylim=c(0, maxDose + addSpace)) +
    labs(x="Time", y="Dose")
  
  if(!is.null(recommendedRegimen)) {
    p2 <- p2 + 
      geom_text(aes_(label=~dose), show.legend=F, data=recommendedRegimen, color=recommendationColor(), nudge_x=1) +
      geom_linerange(ymin=0, aes_(ymax=~dose), data=recommendedRegimen, color=recommendationColor(), position=position_nudge(x=1))
  }
  
  return(p2)
}

#'
#' Prepare the parameters plot.
#' 
#' @inheritParams shinytdmore-plot 
#' @inheritParams shinytdmore-data
#'
prepareParametersPlot <- function(populationPredict, individualPredict, parameters) {
  if (is.null(individualPredict)) {
    return(NULL) # makes no sense to prepare this plot for population
  }
  if(length(parameters)==0)
    return(NULL) # no parameters, so no interest
  # Keep useful parameters and melt data (ids: TIME and PRED_ columns)
  
  individualPredict <- individualPredict[ , c("TIME", parameters) ] %>%
    tidyr::pivot_longer(cols=-.data$TIME, names_to="variable", values_to="IPRED")
  populationPredict <- populationPredict[ , c("TIME", parameters) ] %>%
    tidyr::pivot_longer(cols=-.data$TIME, names_to="variable", values_to="PRED")
  
  data <- individualPredict %>% 
    dplyr::left_join(populationPredict, by=c("TIME", "variable")) %>%
    transmute(TIME=.data$TIME,
              variable=.data$variable,
              change=(.data$IPRED / .data$PRED) - 1
    )

  plot <- ggplot(data=data, mapping=aes_(x=~TIME, y=~change, linetype=~variable)) +
    geom_line(color="slategray3") +
    labs(x="Time", y="Change") +
    scale_y_continuous(labels=scales::percent)
  return(plot)
}

#' Prepare recommendation plots (to be refactored).
#' This includes the recommendation plot and the timeline plot.
#' 
#' @inheritParams shinytdmore-plot
#' @inheritParams shinytdmore-data
#' 
#' @return a list of two plots
#'
prepareRecommendationPlots <- function(populationPredict, individualPredict, recommendationPredict, observed, target, model, now, regimen, recommendedRegimen) {
  predictList <- list(recommendationPredict, individualPredict) %>%
    stats::setNames(c(recommendationColor(), ipredColor())) %>%
    purrr::compact()
  p1 <- preparePredictionPlot(predictList, observed, target, model, now)
  
  newDoses <- regimen
  newDoses$dose <- round(recommendedRegimen$AMT, digits=2)
  p2 <- prepareTimelinePlot(populationPredict, individualPredict, regimen=regimen, recommendedRegimen=newDoses, model=model, now=now)
  
  return(list(p1=p1, p2=p2))
}



#' Default values for plot parameters
#'
#' @param individualPredict dataframe containing the population prediction data
#' @param populationPredict dataframe containing the individual prediction data
#' @param recommendationPredict recommendation data (contains namely ipred, ipredNew and the recommendation)
#' @param parameters parameters
#' @param recommendedRegimen regimen with the AMT column changed by the recommended doses
#' 
#' @name shinytdmore-plot
NULL