#'
#' Update plot using animation. NOT USED.
#' 
#' @param plot the given plot
#' @param outputId the output ID
#'
updatePlot <- function(plot, outputId) {
  p2 <- plot %>% plotly::plotly_build()
  p2 <- p2$x
  p2$layout <- list(datarevision=round(runif(1)*1000))
  p2[c("attrs", "shinyEvents", "highlight", "config", "source", "visdat")] <- NULL
  p2[c("base_url", "cur_data")] <- NULL
  p2$data <- lapply(p2$data, function(x){
    x[c("text", "hoveron", "name", "legendgroup", "showlegend", "xaxis", "yaxis", "hoverinfo", "frame")] <- NULL
    x} )
  ## see https://codepen.io/plotly/pen/ZpWPpj
  ## and https://codepen.io/etpinard/pen/KrpMQa
  ## and also https://plot.ly/javascript/plotlyjs-function-reference/#plotlyreact
  plot_json <- attr(p2, "TOJSON_FUNC")(p2)
  proxy <- plotly::plotlyProxy(outputId)
  plotly::plotlyProxyInvoke(
    proxy, "animate", plot_json,
    list(transition=list(duration=1000, easing='cubic-in-out'), frame=list(duration=1000))
  )
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
#'
mergePlots <- function(p1, p2, p3, output, source) {
  tooltip1 <- c("TIME", output, paste0(output, ".lower"), paste0(output, ".upper"))
  tooltip2 <- c("TIME", "AMT")
  #tooltip3 <- c("TIME", "Parameter", "Population", "Individual", "Change")

  if (is.null(p3)) {
    plot <- plotly::subplot(
      plotly::ggplotly(p1, tooltip=tooltip1, source=paste0(source, "-1")) %>% 
        plotly::config(scrollZoom=T, displayModeBar=F, displaylogo=F),
      plotly::ggplotly(p2, tooltip=tooltip2, source=paste0(source, "-2")) %>% 
        plotly::config(scrollZoom=T, displayModeBar=F, displaylogo=F),
      nrows = 2, heights = c(0.8, 0.2), widths = c(1), shareX=T, shareY=F, titleX=T, titleY=T
    ) %>% plotly::layout(dragmode = "pan", autosize=TRUE )
  } else {
    plot <- plotly::subplot(
      plotly::ggplotly(p1, tooltip=tooltip1, source=paste0(source, "-1")) %>% 
        plotly::config(scrollZoom=T, displayModeBar=F, displaylogo=F),
      plotly::ggplotly(p2, tooltip=tooltip2, source=paste0(source, "-2")) %>% 
        plotly::config(scrollZoom=T, displayModeBar=F, displaylogo=F),
      plotly::ggplotly(p3, source=paste0(source, "-3")) %>% 
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
  xintercept <- as.POSIXct(now) # Must be POSIXct for plotly
  
  # Add X intercept (bug in plotly: as.numeric has to be called on the POSIX date)
  plot <- plot +
    geom_vline(xintercept=as.numeric(xintercept), color=nowColor(), size=0.5, alpha=0.3)
  
  # Add 'Past' and 'Future' label close to the X intercept
  yUpperRange <- layer_scales(plot)$y$range$range[2]
  shift <- 3600*8
  
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
  regimen <- regimen %||% tibble(time=as.POSIXct(character(0)), dose=numeric(0), formulation=character(0), fix=logical(0))
  observed <- observed %||% tibble(time=as.POSIXct(character(0)), dv=numeric(0), use=logical(0))
  
  list(
    p1=preparePredictionPlot(populationPredict, individualPredict, observed=observed, target=target, model=model, now=now),
    p2=prepareTimelinePlot(populationPredict, individualPredict, regimen=regimen, recommendedRegimen=NULL, model=model, now=now),
    p3=prepareParametersPlot(populationPredict, individualPredict, parameters=tdmore::getObservedVariables(model))
  )
  
  #return(list(p1=preparePredictionPlot(data=data, obs=obs, target=target, population=population, model=selectedModel, now=now),
  
}

#'
#' Prepare the prediction plot.
#' 
#' @inheritParams shinytdmore-plot
#' @inheritParams shinytdmore-data
#'
preparePredictionPlot <- function(populationPredict, individualPredict, observed, target, model, now) {
  population <- is.null(individualPredict)
  data <- if(population) populationPredict else individualPredict
  
  color <- if(population) {predColor()} else {ipredColor()}
  observed <- observed %||% tibble(time=as.POSIXct(character(0)), dv=numeric(0), use=logical(0))
  observed <- observed %>% dplyr::filter(.data$use==TRUE) # Plot only 'used' observations
  data <- data %>% dplyr::mutate_if(is.numeric, round, 2) # Round dataframe for better hover tooltips
  
  if(is.null(target) || all(is.na(target))) {
    target <- tdmore::getMetadataByClass(model, "tdmore_target")
  }
  if(is.null(target)) target <- list(min=as.numeric(NA), max=as.numeric(NA))
  ggplotTarget <- tibble(lower=target$min, upper=target$max)
  defaultModel <- getDefaultModel(model)
  output <- getModelOutput(defaultModel)
  
  plot <- ggplot(mapping=aes_string(x="TIME", y=output))
  if (!population) plot <- plot + geom_line(data=populationPredict, color=predColor(), alpha=0.25)
  
  plot <- plot +
    geom_line(data=data, color=color) +
    geom_point(data=observed, aes_string(x="time", y="dv"), color=ifelse(observed$time <= now, samplesColor(), samplesColorFuture()), shape=4, size=3) +
    geom_hline(data=ggplotTarget, aes_string(yintercept="lower"), color=targetColor(), lty=2) +
    geom_hline(data=ggplotTarget, aes_string(yintercept="upper"), color=targetColor(), lty=2) +
    labs(y=getYAxisLabel(defaultModel))
  
  ribbonLower <- paste0(output, ".lower") # Not there in MPC fit
  ribbonUpper <- paste0(output, ".upper") # Not there in MPC fit
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
  
  colnames(populationPredict) <- paste0("PRED_", colnames(populationPredict))
  
  data <- individualPredict %>%
    cbind(populationPredict) %>%
    dplyr::select(c("TIME", parameters, paste0("PRED_", parameters))) %>% 
    tidyr::pivot_longer(parameters, names_to="variable", values_to="value")
  
  # As data is molten, only 1 population column is needed (get rid of all PRED_ columns)
  data$Population <- 0
  data$Parameter <- data$variable
  for (parameter in parameters) {
    data[data$Parameter==parameter, "Population"] <- data[data$Parameter==parameter, paste0("PRED_", parameter)]
  }
  
  # Compute percentage change
  data$Individual <- data$value
  data$Change <- (data$Individual - data$Population) / data$Population * 100

  # Get rid of other columns, round data
  data <- data %>% dplyr::select("TIME", "Parameter", "Population", "Individual", "Change")
  data <- data %>% dplyr::mutate_if(is.numeric, round, 3) # Round dataframe for better hover tooltips

  plot <- ggplot(data=data, mapping=aes_(x=~TIME, y=~Change, linetype=~Parameter)) +
    #TODO: originally included mapping=aes(text=sprintf("Population: %.3f<br>Individual: %.3f", .data$Population, .data$Individual))
    #but none of the geom's have an attribute 'text'
    geom_line(color="slategray3") +
    labs(x="Time", y="Change (%)")
  #print(plot)
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
  shiny::req(nrow(regimen) == nrow(recommendedRegimen)) #if not, wait for update first!
  regimen <- regimen %||% tibble(time=as.POSIXct(character(0)), dose=numeric(0), formulation=character(0), fix=logical(0))
  observed <- observed %||% tibble(time=as.POSIXct(character(0)), dv=numeric(0), use=logical(0))
  
  ipred <- individualPredict %>% dplyr::mutate_if(is.numeric, round, 2) # Round dataframe for better hover tooltips
  ipredNew <- recommendationPredict %>% dplyr::mutate_if(is.numeric, round, 2) # Round dataframe for better hover tooltips
  defaultModel <- getDefaultModel(model)
  observed <- observed %||% tibble(time=as.POSIXct(character(0)), dv=numeric(0), use=logical(0))
  observed <- observed %>% dplyr::filter(.data$use==TRUE) # Plot only used observations
  
  if(is.null(target) || all(is.na(target))) {
    target <- tdmore::getMetadataByClass(model, "tdmore_target")
  }
  if(is.null(target)) target <- list(min=as.numeric(NA), max=as.numeric(NA))
  ggplotTarget <- tibble(lower=target$min, upper=target$max)
  output <- getModelOutput(defaultModel)
  p1 <- ggplot(mapping=aes_string(x="TIME", y=output)) +
    geom_line(data=ipred, color=ipredColor(), alpha=0.2) +
    geom_line(data=ipredNew, color=recommendationColor()) +
    geom_point(data=observed, aes_(x=~time, y=~dv), color=ifelse(observed$time <= now, samplesColor(), samplesColorFuture()), shape=4, size=3) +
    geom_hline(data=ggplotTarget, aes_(yintercept=~lower), color=targetColor(), lty=2) +
    geom_hline(data=ggplotTarget, aes_(yintercept=~upper), color=targetColor(), lty=2) +
    labs(y=getYAxisLabel(defaultModel))
  
  ribbonLower <- paste0(output, ".lower") # Not there in MPC fit
  ribbonUpper <- paste0(output, ".upper") # Not there in MPC fit
  if ((ribbonLower %in% colnames(ipredNew)) && (ribbonUpper %in% colnames(ipredNew))) {
    p1 <- p1 + geom_ribbon(fill=recommendationColor(), aes_string(ymin=ribbonLower, ymax=ribbonUpper), data=ipredNew, alpha=0.2)
  }
  p1 <- addNowLabelAndIntercept(p1, now)
  
  # We have to be very careful with as.Date(), zone should be always taken into account
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