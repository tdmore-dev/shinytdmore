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
#' Merge plots.
#' 
#' @param p1 first plot
#' @param p2 second plot
#'
mergePlots <- function(p1, p2) {
  subplot(
    ggplotly(p1) %>% config(scrollZoom=T, displayModeBar=F, displaylogo=F, collaborate=F),
    ggplotly(p2) %>% config(scrollZoom=T, displayModeBar=F, displaylogo=F, collaborate=F),
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
    geom_text(aes(x=xintercept, y=yUpperRange), label="Past", color=nowColor(), nudge_x=-shift, size=4) +
    geom_text(aes(x=xintercept, y=yUpperRange), label="Future", color=nowColor(), nudge_x=shift, size=4)
  
  return(plot)
}


#' Convert main data to TDMore domain.
#'
#' @param doses doses
#' @param obs observations
#' @param now now date, POSIXlt date
#' 
#' @return to be described
#'
convertDataToTdmore <- function(doses, obs, now) {
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
  regimen <- regimen %>% dplyr::mutate(PAST=TIME < relativeNow) # sign '<' used on purpose
  filteredRegimen <- regimen %>% dplyr::filter(PAST) %>% dplyr::select(-PAST)
  
  # Make observed and filtered observed dataframes
  if (nrow(obs) > 0) {
    obsDates <- dateAndTimeToPOSIX(obs$date, obs$time)
    observed <- data.frame(
      TIME=as.numeric(difftime(obsDates, firstDoseDate, units="hour")),
      CONC=obs$measure,
      USE=obs$use
    )
    observed <- observed %>% dplyr::mutate(PAST=TIME <= relativeNow) # sign '<=' used on purpose (through concentration can be used for recommendation dose at same time)
    filteredObserved <- observed %>% dplyr::filter(PAST && USE) %>% dplyr::select(-c("PAST", "USE"))
  } else {
    observed <- NULL
    filteredObserved <- NULL
  }

  return(list(regimen=regimen, filteredRegimen=filteredRegimen,
              observed=observed, filteredObserved=filteredObserved,
              firstDoseDate=firstDoseDate))
}

#'
#' Prepare population OR individual prediction plots.
#' This includes the prediction plot and the timeline plot.
#' 
#' @param doses doses
#' @param obs observations
#' @param model tdmore model
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
  
  data <- convertDataToTdmore(doses, obs, now)
  regimen <- data$regimen %>% select(-PAST)
  filteredObserved <- data$filteredObserved
  firstDoseDate <- data$firstDoseDate
  
  # Compute fit if individual prediction is asked
  object <- model
  if (!population) {
    object <- estimate(model, observed=filteredObserved, regimen=regimen, covariates=covs)
  }
  
  
  # Predictions
  maxTime <- if(nrow(regimen)==0){24} else {max(regimen$TIME)+24}
  data <- predict(
    object,
    newdata = data.frame(TIME=seq(0, maxTime, length.out=300), CONC=NA),
    regimen=regimen,
    covariates=covs,
    se=TRUE) %>% as.data.frame()
  data$TIME <- firstDoseDate + data$TIME*60*60
  
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
  obs <- obs %>% filter(use==TRUE) # Plot only used observations
  obs$datetime <- dateAndTimeToPOSIX(obs$date, obs$time)
  
  ggplotTarget <- data.frame(lower=target$min, upper=target$max)
  
  plot <- ggplot(mapping=aes(x=TIME, y=CONC)) +
    geom_line(data=data, color=color) +
    geom_ribbon(fill=color, aes(ymin=CONC.lower, ymax=CONC.upper), data=data, alpha=0.1) +
    geom_point(data=obs, aes(x=datetime, y=measure), color=ifelse(obs$datetime <= now, samplesColor(), samplesColorFuture()), shape=4, size=3) +
    geom_hline(data=ggplotTarget, aes(yintercept=lower), color=targetColor(), lty=2) +
    geom_hline(data=ggplotTarget, aes(yintercept=upper), color=targetColor(), lty=2) +
    labs(y=getYAxisLabel(model))
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
  times <- dateAndTimeToPOSIX(doses$date, doses$time)
  maxDose <- if(nrow(doses) > 0) {max(doses$dose)} else {0}
  addSpace <- maxDose*0.15 # Add 15% margin for dose number

  plot <- ggplot(doses, aes(x=times, y=dose)) +
    geom_text(aes(x=times, y=dose, label=dose), nudge_x=0, nudge_y=0, check_overlap=T, show.legend=F) +
    geom_linerange(ymin=0, aes(ymax=dose)) +
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
  data <- convertDataToTdmore(doses, obs, now)
  regimen <- data$regimen
  filteredRegimen <- data$filteredRegimen
  filteredObserved <- data$filteredObserved
  firstDoseDate <- data$firstDoseDate
  
  # Find dose rows to be adapted
  doseRows <- which(!regimen$PAST)
  if(length(doseRows)==0) {
    stop("There is no dose in the future")
  }

  # Compute fit
  fit <- estimate(model, observed = filteredObserved, regimen = filteredRegimen, covariates = covs)

  # Implementing the iterative process
  nextRegimen <- regimen %>% dplyr::select(-PAST)
  doseMetadata <- getMetadataByName(model, "DOSE")
  dosingInterval <- if(is.null(doseMetadata)) {24} else {doseMetadata$dosing_interval}
  
  for (index in seq_along(doseRows)) {
    row <- regimen[doseRows[index],]
    last <- index == length(doseRows)
    
    if (last) {
      nextTime <- row$TIME + dosingInterval # By default, II
    } else {
      nextTime <- regimen[doseRows[index + 1],]$TIME - 0.001 # Just before the next dose
    }
    recommendation <- findDose(fit, regimen=nextRegimen, doseRows=doseRows[(index:length(doseRows))],
                               target=data.frame(TIME=nextTime, CONC=(target$min + target$max)/2))
    nextRegimen <- recommendation$regimen
  }
  
  firstDoseInFutureTime <- regimen$TIME[doseRows[1]]
  
  # Predict ipred without adapting the dose
  ipred <- fit %>% predict(
            newdata = data.frame(TIME=seq(0, max(regimen$TIME) + dosingInterval, length.out=300), CONC=NA),
            regimen=regimen %>% dplyr::select(-PAST), se=F, covariates=covs)
  ipred$TIME <- firstDoseDate + ipred$TIME*3600 # Plotly able to plot POSIXct
  
  # Predict ipred with the new recommendation
  ipredNew <- fit %>% predict(
    newdata = data.frame(TIME=seq(firstDoseInFutureTime, max(regimen$TIME) + dosingInterval, length.out=300), CONC=NA),
    regimen=nextRegimen, se=TRUE, covariates=covs)
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
  if (is.null(doses)) {
    return(NULL)
  }

  firstDoseDate <- recommendation$firstDoseDate
  ipred <- recommendation$ipred
  ipredNew <- recommendation$ipredNew
  recommendedRegimen <- recommendation$recommendedRegimen
  recommendedRegimen$dose <- round(recommendedRegimen$AMT, digits=2)
  
  obs <- obs %>% filter(use==TRUE) # Plot only used observations
  obs$datetime <- dateAndTimeToPOSIX(obs$date, obs$time)
  
  ggplotTarget <- data.frame(lower=target$min, upper=target$max)
  
  p1 <- ggplot(mapping=aes(x=TIME, y=CONC)) +
    geom_line(data=ipred, color=ipredColor(), alpha=0.2) +
    geom_line(data=ipredNew, color=recommendationColor()) +
    geom_ribbon(fill=recommendationColor(), aes(ymin=CONC.lower, ymax=CONC.upper), data=ipredNew, alpha=0.2) +
    geom_point(data=obs, aes(x=datetime, y=measure), color=ifelse(obs$datetime <= now, samplesColor(), samplesColorFuture()), shape=4, size=3) +
    geom_hline(data=ggplotTarget, aes(yintercept=lower), color=targetColor(), lty=2) +
    geom_hline(data=ggplotTarget, aes(yintercept=upper), color=targetColor(), lty=2) +
    labs(y=getYAxisLabel(model))
  p1 <- addNowLabelAndIntercept(p1, now)
  
  # We have to be very careful with as.Date(), zone should be always taken into account
  newDoses <- recommendedRegimen %>% mutate(date=as.Date(TIME, tz=Sys.timezone()), time=strftime(TIME,"%H:%M"))
  p2 <- prepareTimelinePlot(doses=newDoses, xlim=c(firstDoseDate, max(ipredNew$TIME)), model=model, now=now)
  
  return(list(p1=p1, p2=p2))
}