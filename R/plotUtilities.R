#'
#' Update plot using animation.
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

#'
#' Get a nice Y-axis label.
#' 
#' @param model tdmore model
#' @return a label
#'
getYAxisLabel <- function(model) {
  output <- getModelOutput(model)
  outputMetadata <- getMetadataByName(model, output)
  label <- if(!is.null(outputMetadata)) {toString(outputMetadata)} else {"Concentration"}
  return(label)
}

#'
#' Get dose column label.
#' 
#' @param model tdmore model
#' @param breakLine breakline or just space before the unit
#' @return a label
#'
getDoseColumnLabel <- function(model, breakLine=T) {
  doseMetadata <- getMetadataByName(model, "DOSE")
  separator <- if(breakLine){"\n"} else{" "}
  label <- if(!is.null(doseMetadata)) {paste0("Dose", separator, "(", doseMetadata$unit, ")")} else {"Dose"}
  return(label)
}

#'
#' Get recommended dose column label.
#' 
#' @param model tdmore model
#' @param breakLine breakline or just space before the unit
#' @return a label
#'
getRecommendedDoseColumnLabel <- function(model, breakLine=T) {
  doseMetadata <- getMetadataByName(model, "DOSE")
  separator <- if(breakLine){"\n"} else{" "}
  label <- if(!is.null(doseMetadata)) {paste0("Rec. dose", separator, "(", doseMetadata$unit, ")")} else {"Dose"}
  return(label)
}

#'
#' Get model output variable name.
#' 
#' @param model tdmore model
#' @return the model output variable name, like 'CONC'
#'
getModelOutput <- function(model) {
  return(model$res_var[[1]]$var) # TODO: what if several outputs?
}

#'
#' Retrieve the dosing interval from the tdmore metadata.
#' If no dose metadata is defined in the model, 24 hours is returned by default.
#' 
#' @param model tdmore model
#' @return the dosing interval
#'
getDosingInterval <- function(model) {
  doseMetadata <- getMetadataByName(model, "DOSE")
  dosingInterval <- if(is.null(doseMetadata)) {24} else {doseMetadata$dosing_interval}
  return(dosingInterval)
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
  outputMetadata <- getMetadataByName(model, getModelOutput(model))
  label <- if(!is.null(outputMetadata)) {paste0("Measure", separator, "(", outputMetadata$unit, ")")} else {"Measure"}
  return(label)
}

#'
#' Get 'newdata' dataframe for tdmore predictions.
#' 
#' @param start start time of 'TIME' column
#' @param stop stop time of 'TIME' column
#' @param output output name (e.g. 'CONC')
#' @return a dataframe for tdmore
#'
getNewdata <- function(start, stop, output) {
  times <- seq(start, stop, by=0.5)
  minSamples <- 300
  if (length(times) < minSamples) {
    times <- seq(start, stop, length.out=minSamples)
  }
  newdata <- data.frame(TIME=times)
  newdata[,output] <- NA
  return(newdata)
}

#'
#' Merge plots.
#' 
#' @param p1 first plot
#' @param p2 second plot
#' @param output main tdmore output
#'
mergePlots <- function(p1, p2, output) {
  tooltip1 <- c("TIME", output, paste0(output, ".lower"), paste0(output, ".upper"))
  tooltip2 <- c("TIME", "AMT")
  subplot(
    ggplotly(p1, tooltip=tooltip1) %>% config(scrollZoom=T, displayModeBar=F, displaylogo=F, collaborate=F),
    ggplotly(p2, tooltip=tooltip2) %>% config(scrollZoom=T, displayModeBar=F, displaylogo=F, collaborate=F),
    nrows = 2, heights = c(0.8, 0.2), widths = c(1), shareX=T, shareY=F, titleX=T, titleY=T
  ) %>% layout(dragmode = "pan")
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


#' Convert main data to TDMore domain.
#'
#' @param doses doses
#' @param obs observations
#' @param output output name (e.g. 'CONC')
#' @param now now date, POSIXlt date
#' @param iov iov present in the model, logical value
#' 
#' @return to be described
#'
convertDataToTdmore <- function(doses, obs, output, now, iov) {
  # Important dates
  doseDates <- dateAndTimeToPOSIX(doses$date, doses$time)
  firstDoseDate <- min(doseDates)
  
  # Now but in hours compared to the reference time
  relativeNow <- POSIXToHours(now) - POSIXToHours(firstDoseDate)
  
  # Make regimen and filtered regimen dataframes
  regimen <- data.frame(
    TIME=as.numeric(difftime(doseDates, firstDoseDate, units="hour")),
    AMT=doses$dose
  )
  if (iov) {
    regimen$OCC <- seq_len(nrow(regimen))
  }
  regimen <- regimen %>% dplyr::mutate(PAST=TIME < relativeNow) # sign '<' used on purpose
  
  # Make observed and filtered observed dataframes
  if (nrow(obs) > 0) {
    obsDates <- dateAndTimeToPOSIX(obs$date, obs$time)
    observed <- data.frame(TIME=as.numeric(difftime(obsDates, firstDoseDate, units="hour")), USE=obs$use)
    observed[, output] <- obs$measure
    observed <- observed %>% dplyr::mutate(PAST=TIME <= relativeNow) # sign '<=' used on purpose (through concentration can be used for recommendation dose at same time)
    filteredObserved <- observed %>% dplyr::filter(PAST & USE) %>% dplyr::select(-c("PAST", "USE"))
  } else {
    observed <- NULL
    filteredObserved <- NULL
  }

  return(list(regimen=regimen, observed=observed, filteredObserved=filteredObserved, firstDoseDate=firstDoseDate))
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
#' @param population logical value, true for population, false for individual
#' @param now now date, POSIXlt date
#' @return a list of two plots
#'
preparePredictionPlots <- function(doses, obs, model, covs, target, population, now) {
  if(nrow(doses)==0) {
    stop("Please add a dose in the left panel")
  }
  
  data <- convertDataToTdmore(doses, obs, getModelOutput(model), now, !is.null(model$iov))
  regimen <- data$regimen %>% select(-PAST)
  filteredObserved <- data$filteredObserved
  firstDoseDate <- data$firstDoseDate
  isMpc <- inherits(model, "tdmore_mpc")
  
  if (population) {
    # Population 'fit'
    object <- estimate(model, regimen=regimen, covariates=covs)
  } else {
    # Fit
    object <- estimate(model, observed=filteredObserved, regimen=regimen, covariates=covs)
  }
  
  # Predictions
  dosingInterval <- getDosingInterval(model)
  maxTime <- if(nrow(regimen)==0){dosingInterval} else {max(regimen$TIME)+dosingInterval}
  newdata <- getNewdata(0, maxTime, getModelOutput(model))
  
  if (isMpc && !population) {
    data <- predict(object, newdata=newdata, regimen=regimen, covariates=object$covariates, se.fit=F)
  } else {
    data <- predict(object, newdata=newdata, regimen=regimen, covariates=covs, se.fit=T, level=0.95) # 95% CI by default
  }
  data$TIME <- firstDoseDate + data$TIME*60*60

  # In case of fit, compute PRED median as well
  if (!population) {
    pred <- predict(model, newdata=newdata, regimen=regimen, covariates=covs, se=F)
    data$PRED <- pred[, getModelOutput(model)]
  }
  
  return(list(p1=preparePredictionPlot(data=data, obs=obs, target=target, population=population, model=model, now=now),
              p2=prepareTimelinePlot(doses=doses, xlim=c(firstDoseDate, max(data$TIME)), model=model, now=now)))
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
preparePredictionPlot <- function(data, obs, target, population, model, now) {
  color <- if(population) {predColor()} else {ipredColor()}
  obs <- obs %>% filter(use==TRUE) # Plot only 'used' observations
  obs$datetime <- dateAndTimeToPOSIX(obs$date, obs$time)
  data <- data %>% mutate_if(is.numeric, round, 2) # Round dataframe for better hover tooltips
  
  ggplotTarget <- data.frame(lower=target$min, upper=target$max)
  output <- getModelOutput(model)
  
  plot <- ggplot(mapping=aes_string(x="TIME", y=output))
  if (!population) {
    plot <- plot + geom_line(data=data, mapping=aes(y=PRED), color=predColor(), alpha=0.25)
  }
  plot <- plot +
    geom_line(data=data, color=color) +
    geom_point(data=obs, aes(x=datetime, y=measure), color=ifelse(obs$datetime <= now, samplesColor(), samplesColorFuture()), shape=4, size=3) +
    geom_hline(data=ggplotTarget, aes(yintercept=lower), color=targetColor(), lty=2) +
    geom_hline(data=ggplotTarget, aes(yintercept=upper), color=targetColor(), lty=2) +
    labs(y=getYAxisLabel(model))
  
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
#' @param xlim ggplot xlim argument
#' @param model tdmore model
#' @param now now date, currently not used here
#'
prepareTimelinePlot <- function(doses, xlim, model, now) {
  doses_copy <- doses 
  doses_copy$TIME <- dateAndTimeToPOSIX(doses$date, doses$time) # Hover same as in prediction plot
  doses_copy$AMT <- doses$dose
  doses_copy$DOSE <- doses$dose # Duplicated so that 'AMT' does not appear twice in tooltip
  maxDose <- if(nrow(doses) > 0) {max(doses$dose)} else {0}
  addSpace <- maxDose*0.15 # Add 15% margin for dose number

  plot <- ggplot(doses_copy, aes(x=TIME, y=AMT)) +
    geom_text(aes(x=TIME, y=AMT, label=AMT), nudge_x=0, nudge_y=0, check_overlap=T, show.legend=F) +
    geom_linerange(ymin=0, aes(ymax=DOSE)) +
    coord_cartesian(xlim=xlim, ylim=c(0, maxDose + addSpace)) +
    labs(x="Time", y=getDoseColumnLabel(model, breakLine=F))

  return(plot)
}

#'
#' Prepare recommendation.
#' 
#' @param doses doses
#' @param obs observations
#' @param model tdmore model
#' @param covs covariates
#' @param target numeric vector of size 2, min and max value
#' @param now now date, POSIXlt date
#' 
#' @return a list with pred, ipred and the recommendation
#' @export
#'
prepareRecommendation <- function(doses, obs, model, covs, target, now) {
  if (nrow(doses)==0) {
    stop("Please add a dose in the left panel")
  }
  data <- convertDataToTdmore(doses, obs, getModelOutput(model), now, !is.null(model$iov))
  regimen <- data$regimen
  filteredObserved <- data$filteredObserved
  firstDoseDate <- data$firstDoseDate
  isMpc <- inherits(model, "tdmore_mpc")
  
  # Find dose rows to be adapted
  doseRows <- which(!regimen$PAST)
  if (length(doseRows)==0) {
    stop("There is no dose in the future")
  }

  # Compute fit
  fit <- estimate(model, observed=filteredObserved, regimen=regimen %>% dplyr::select(-PAST), covariates=covs)

  # Implementing the iterative process
  nextRegimen <- regimen %>% dplyr::select(-PAST)
  dosingInterval <- getDosingInterval(model)
  output <- getModelOutput(model)
  
  for (index in seq_along(doseRows)) {
    row <- regimen[doseRows[index],]
    last <- index == length(doseRows)
    
    if (last) {
      nextTime <- row$TIME + dosingInterval # By default, II
    } else {
      nextTime <- regimen[doseRows[index + 1],]$TIME - 0.001 # Just before the next dose
    }
    targetDf <- data.frame(TIME=nextTime)
    targetDf[, output] <- (target$min + target$max)/2
    recommendation <- findDose(fit, regimen=nextRegimen, doseRows=doseRows[(index:length(doseRows))], target=targetDf)
    nextRegimen <- recommendation$regimen
  }
  
  firstDoseInFutureTime <- regimen$TIME[doseRows[1]]
  covsToUse <- if(isMpc){fit$covariates}else{covs}
  
  # Predict ipred without adapting the dose
  newdata <- getNewdata(0, max(regimen$TIME) + dosingInterval, output)
  ipred <-  predict(fit, newdata = newdata, regimen=regimen %>% dplyr::select(-PAST), covariates=covsToUse, se.fit=F)
  ipred$TIME <- firstDoseDate + ipred$TIME*3600 # Plotly able to plot POSIXct
  
  # Predict ipred with the new recommendation
  newdata <- getNewdata(firstDoseInFutureTime, max(regimen$TIME) + dosingInterval, output)
  ipredNew <- predict(fit, newdata=newdata, regimen=nextRegimen, covariates=covsToUse, se.fit=!isMpc, level=0.95) # se.fit disabled if MPC model
  ipredNew$TIME <- firstDoseDate + ipredNew$TIME*3600 # Plotly able to plot POSIXct
  
  # Back compute to POSIXct
  recommendedRegimen <- recommendation$regimen %>% dplyr::mutate(TIME=firstDoseDate + TIME*3600, PAST=regimen$PAST)

  return(list(ipred=ipred, ipredNew=ipredNew, recommendedRegimen=recommendedRegimen, firstDoseDate=firstDoseDate))
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
#' @param recommendation recommendation (contains ipred, ipredNew and the recommendation)
#' @param now now date
#' 
#' @return a list of two plots
#'
prepareRecommendationPlots <- function(doses, obs, model, covs, target, recommendation, now) {
  firstDoseDate <- recommendation$firstDoseDate
  ipred <- recommendation$ipred %>% mutate_if(is.numeric, round, 2) # Round dataframe for better hover tooltips
  ipredNew <- recommendation$ipredNew %>% mutate_if(is.numeric, round, 2) # Round dataframe for better hover tooltips
  recommendedRegimen <- recommendation$recommendedRegimen
  recommendedRegimen$dose <- round(recommendedRegimen$AMT, digits=2)
  
  obs <- obs %>% filter(use==TRUE) # Plot only used observations
  obs$datetime <- dateAndTimeToPOSIX(obs$date, obs$time)
  
  ggplotTarget <- data.frame(lower=target$min, upper=target$max)
  output <- getModelOutput(model)
  
  p1 <- ggplot(mapping=aes_string(x="TIME", y=output)) +
    geom_line(data=ipred, color=ipredColor(), alpha=0.2) +
    geom_line(data=ipredNew, color=recommendationColor()) +
    geom_point(data=obs, aes(x=datetime, y=measure), color=ifelse(obs$datetime <= now, samplesColor(), samplesColorFuture()), shape=4, size=3) +
    geom_hline(data=ggplotTarget, aes(yintercept=lower), color=targetColor(), lty=2) +
    geom_hline(data=ggplotTarget, aes(yintercept=upper), color=targetColor(), lty=2) +
    labs(y=getYAxisLabel(model))
  p1 <- addNowLabelAndIntercept(p1, now)
  
  ribbonLower <- paste0(output, ".lower") # Not there in MPC fit
  ribbonUpper <- paste0(output, ".upper") # Not there in MPC fit
  if ((ribbonLower %in% colnames(ipredNew)) && (ribbonUpper %in% colnames(ipredNew))) {
    p1 <- p1 + geom_ribbon(fill=recommendationColor(), aes_string(ymin=ribbonLower, ymax=ribbonUpper), data=ipredNew, alpha=0.2)
  }
  
  # We have to be very careful with as.Date(), zone should be always taken into account
  newDoses <- recommendedRegimen %>% mutate(date=as.Date(TIME, tz=Sys.timezone()), time=strftime(TIME,"%H:%M"))
  p2 <- prepareTimelinePlot(doses=newDoses, xlim=c(firstDoseDate, max(ipredNew$TIME)), model=model, now=now)
  
  return(list(p1=p1, p2=p2))
}