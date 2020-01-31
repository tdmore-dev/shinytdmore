#'
#' Update plot using animation. NOT USED.
#' 
#' @param plot the given plot
#' @param outputId the output ID
#'
updatePlot <- function(plot, outputId) {
  p2 <- plot %>% plotly_build %>% .$x
  p2$layout <- list(datarevision=round(runif(1)*1000))
  p2[c("attrs", "shinyEvents", "highlight", "config", "source", "visdat")] <- NULL
  p2[c("base_url", "cur_data")] <- NULL
  p2$data <- lapply(p2$data, function(x){
    x[c("text", "hoveron", "name", "legendgroup", "showlegend", "xaxis", "yaxis", "hoverinfo", "frame")] <- NULL
    x} )
  ## see https://codepen.io/plotly/pen/ZpWPpj
  ## and https://codepen.io/etpinard/pen/KrpMQa
  ## and also https://plot.ly/javascript/plotlyjs-function-reference/#plotlyreact
  plot_json <- p2 %>% attr(., "TOJSON_FUNC")(.)
  proxy <- plotlyProxy(outputId)
  plotlyProxyInvoke(
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
mergePlots <- function(p1, p2, p3, output) {
  tooltip1 <- c("TIME", output, paste0(output, ".lower"), paste0(output, ".upper"))
  tooltip2 <- c("TIME", "AMT")
  #tooltip3 <- c("TIME", "Parameter", "Population", "Individual", "Change")

  if (is.null(p3)) {
    plot <- plotly::subplot(
      plotly::ggplotly(p1, tooltip=tooltip1) %>% 
        plotly::config(scrollZoom=T, displayModeBar=F, displaylogo=F),
      plotly::ggplotly(p2, tooltip=tooltip2) %>% 
        plotly::config(scrollZoom=T, displayModeBar=F, displaylogo=F),
      nrows = 2, heights = c(0.8, 0.2), widths = c(1), shareX=T, shareY=F, titleX=T, titleY=T
    ) %>% plotly::layout(dragmode = "pan")
  } else {
    plot <- plotly::subplot(
      plotly::ggplotly(p1, tooltip=tooltip1) %>% 
        plotly::config(scrollZoom=T, displayModeBar=F, displaylogo=F),
      plotly::ggplotly(p2, tooltip=tooltip2) %>% 
        plotly::config(scrollZoom=T, displayModeBar=F, displaylogo=F),
      plotly::ggplotly(p3) %>% 
        plotly::config(scrollZoom=T, displayModeBar=F, displaylogo=F),
      nrows = 3, heights = c(0.7, 0.15, 0.15), widths = c(1), shareX=T, shareY=F, titleX=T, titleY=T
    ) %>% plotly::layout(dragmode = "pan", legend = list(orientation = "h", y=-250))
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
#' @param doses doses
#' @param obs observations
#' @param model tdmore/tdmore_mpc model
#' @param covs covariates
#' @param target numeric vector of size 2, min and max value
#' @param now now date, POSIXlt date
#' @return a list of two plots
#' @export
#'
preparePredictionPlots <- function(populationDf, individualDf, observed, target, model, now, regimen) {
  list(
    p1=preparePredictionPlot(populationDf, individualDf, obs=observed, target=target, model=model, now=now),
    p2=prepareTimelinePlot(regimen=regimen, model=model, now=now),
    p3=prepareParametersPlot(populationDf, individualDf, parameters=tdmore::getObservedVariables(model))
  )
  
  #return(list(p1=preparePredictionPlot(data=data, obs=obs, target=target, population=population, model=selectedModel, now=now),
  
}

#'
#' Prepare the prediction plot.
#' 
#' @param data dataframe containing the prediction data
#' @param obs dataframe containing the observations
#' @param target numeric vector of size 2, min and max value
#' @param population logical value, true for population, false for individual prediction
#' @param model tdmore model
#' @param now now date
#'
preparePredictionPlot <- function(populationDf, individualDf, obs, target, model, now) {
  population <- is.null(individualDf)
  data <- if(population) populationDf else individualDf
  
  color <- if(population) {predColor()} else {ipredColor()}
  obs <- obs %>% dplyr::filter(.data$use==TRUE) # Plot only 'used' observations
  obs$datetime <- obs$time
  data <- data %>% dplyr::mutate_if(is.numeric, round, 2) # Round dataframe for better hover tooltips
  
  if(is.null(target)) {
    target <- tdmore::getMetadataByClass(model, "tdmore_target")
  }
  ggplotTarget <- tibble(lower=target$min, upper=target$max)
  defaultModel <- getDefaultModel(model)
  output <- getModelOutput(defaultModel)
  
  plot <- ggplot(mapping=aes_string(x="TIME", y=output))
  if (!population) plot <- plot + geom_line(data=populationDf, color=predColor(), alpha=0.25)
  
  plot <- plot +
    geom_line(data=data, color=color) +
    geom_point(data=obs, aes_string(x="datetime", y="dv"), color=ifelse(obs$datetime <= now, samplesColor(), samplesColorFuture()), shape=4, size=3) +
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
#' @param doses dataframe containing the doses
#' @param model tdmore model
#' @param now now date, currently not used here
#'
prepareTimelinePlot <- function(regimen, model, now) {
  maxDose <- if(nrow(regimen) > 0) {max(regimen$dose)} else {0}
  addSpace <- maxDose*0.15 # Add 15% margin for dose number

  plot <- ggplot(regimen, aes(x=time, y=dose)) +
    geom_text(aes(label=dose), nudge_x=0, nudge_y=0, check_overlap=T, show.legend=F) +
    geom_linerange(ymin=0, aes(ymax=dose)) +
    coord_cartesian(ylim=c(0, maxDose + addSpace)) +
    labs(x="Time", y="Dose")

  return(plot)
}

#'
#' Prepare the parameters plot.
#' 
#' @param data dataframe containing the prediction data
#' @param parameters parameters
#' @param population logical value, true for population, false for individual prediction
#'
prepareParametersPlot <- function(populationData, individualDf, parameters) {
  if (is.null(individualDf)) {
    return(NULL) # makes no sense to prepare this plot for population
  }
  if(length(parameters)==0)
    return(NULL) # no parameters, so no interest
  # Keep useful parameters and melt data (ids: TIME and PRED_ columns)
  
  colnames(populationData) <- paste0("PRED_", colnames(populationData))
  
  data <- individualDf %>%
    cbind(populationData) %>%
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

  plot <- ggplot(data=data, mapping=aes(x=TIME, y=Change, linetype=Parameter)) +
    geom_line(color="slategray3", mapping=aes(text=sprintf("Population: %.3f<br>Individual: %.3f", Population, Individual))) +
    labs(x="Time", y="Change (%)")
  #print(plot)
  return(plot)
}

#'
#' Prepare the timeline with both original and recommended doses.
#' 
#' @param originalDoses dataframe containing the original entered doses
#' @param recommendedDoses dataframe containing the recommended doses calculated by the computer
#' @param xlim ggplot xlim argument
#' @param model tdmore model
#' @param now now date, currently not used here
#'
prepareRecommendedTimelinePlot <- function(regimen, newRegimen, model, now) {
  maxDose <- max(c(0, regimen$dose, newRegimen$dose))
  addSpace <- maxDose*0.15 # Add 15% margin for dose number
  #II <- getDosingInterval(model)
  #nudge_II <- II/24*60*60 #empirical ratio
  
  plot <- ggplot(regimen, aes(x=time, y=dose)) +
    geom_text(aes(label=dose), nudge_x=0, nudge_y=0, check_overlap=T, show.legend=F) +
    geom_linerange(ymin=0, aes(ymax=dose)) +
    geom_text(aes(label=dose), show.legend=F, data=newRegimen, color=recommendationColor(), nudge_x=1) +
    geom_linerange(ymin=0, aes(ymax=dose), data=newRegimen, color=recommendationColor(), nudge_x=1)+
    coord_cartesian(ylim=c(0, maxDose + addSpace)) +
    labs(x="Time", y="Dose")
  
    # geom_text(data=doses_copy %>% dplyr::filter(TIME>now),aes(x=TIME, y=AMT, label=AMT), nudge_x=nudge_II, nudge_y=0, check_overlap=T, show.legend=F,color=recommendationColor()) +
    # geom_linerange(data=doses_copy %>% dplyr::filter(TIME>now),ymin=0, aes(ymax=DOSE), position = position_nudge(x = nudge_II),color=recommendationColor()) +
    # geom_text(data=doses_copy2,aes(x=TIME, y=AMT, label=AMT), nudge_x=-nudge_II, nudge_y=0, check_overlap=T, show.legend=F, alpha=0.2) +
    # geom_linerange(data=doses_copy2,ymin=0, aes(ymax=DOSE), position = position_nudge(x = -nudge_II), alpha=0.2) +
  
  return(plot)
}

#'
#' Prepare recommendation plots (to be refactored).
#' This includes the recommendation plot and the timeline plot.
#' 
#' @param doses doses
#' @param obs observations
#' @param model tdmore model
#' @param covs covariates
#' @param target numeric vector of size 2, min and max value
#' @param recommendationData recommendation data (contains namely ipred, ipredNew and the recommendation)
#' @param now now date
#' 
#' @return a list of two plots
#'
prepareRecommendationPlots <- function(populationDf, individualDf, recommendationDf, observed, target, model, now, regimen, recommendedRegimen) {
  ipred <- individualDf %>% dplyr::mutate_if(is.numeric, round, 2) # Round dataframe for better hover tooltips
  ipredNew <- recommendationDf %>% dplyr::mutate_if(is.numeric, round, 2) # Round dataframe for better hover tooltips
  defaultModel <- getDefaultModel(model)
  obs <- observed %>% dplyr::filter(use==TRUE) # Plot only used observations
  obs$datetime <- obs$time
  
  if(is.null(target)) {
    target <- tdmore::getMetadataByClass(model, "tdmore_target")
  }
  ggplotTarget <- tibble(lower=target$min, upper=target$max)
  output <- getModelOutput(defaultModel)
  p1 <- ggplot(mapping=aes_string(x="TIME", y=output)) +
    geom_line(data=ipred, color=ipredColor(), alpha=0.2) +
    geom_line(data=ipredNew, color=recommendationColor()) +
    geom_point(data=obs, aes(x=datetime, y=dv), color=ifelse(obs$datetime <= now, samplesColor(), samplesColorFuture()), shape=4, size=3) +
    geom_hline(data=ggplotTarget, aes(yintercept=lower), color=targetColor(), lty=2) +
    geom_hline(data=ggplotTarget, aes(yintercept=upper), color=targetColor(), lty=2) +
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
  p2 <- prepareRecommendedTimelinePlot(regimen=regimen, newRegimen=newDoses, model=model, now=now)
  
  return(list(p1=p1, p2=p2))
}