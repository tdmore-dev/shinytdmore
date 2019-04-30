#'
#' Population prediction color (blue).
#'
#' @return a color string
#'
predColor <- function() {
  return("steelblue2")
}

#'
#' individual prediction color (red).
#'
#' @return a color string
#'
ipredColor <- function() {
  return("tomato1")
}

#'
#' Samples color (grey).
#'
#' @return a color string
#'
samplesColor <- function() {
  return("gray48")
}

#'
#' Target color (grey).
#'
#' @return a color string
#'
targetColor <- function() {
  return("gray48")
}

#'
#' Recommendation color (green).
#'
#' @return a color string
#'
recommendationColor <- function() {
  return("yellowgreen")
}

#'
#' Now color (green).
#'
#' @return a color string
#'
nowColor <- function() {
  return("palegreen4")
}

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
#' Convert date and time vectors to POSIX.
#' 
#' @param date date vector
#' @param time character vector with '\%H:\%M'-formatted times
#'
dateAndTimeToPOSIX <- function(date, time) {
  return(as.POSIXct(strptime(paste(date, time), format = "%Y-%m-%d %H:%M")))
}

#'
#' Generate the list of hours that can be picked in the 'hours' combobox.
#' 
#' @return a list of all the hours
#'
hoursList <- function() {
  grid <- expand.grid(pad(c(0,30)), pad(0:23))
  hours <- paste0(grid$Var2, ":", grid$Var1)
  return(hours)
}

#'
#' Pad integer with a zero if needed.
#' 
#' @param integer the integer to be padded
#' @return a string
#'
pad <- function(integer) {
  ifelse(integer < 10, paste0('0', integer), paste0(integer))
}

#'
#' Get a nice Y-axis label.
#' 
#' @param model tdmore model
#' @return a label
#'
getYAxisLabel <- function(model) {
  output <- model$res_var[[1]]$var # TODO: what if several outputs?
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
#' Prepare the prediction plot.
#' 
#' @param data dataframe containing the prediction data
#' @param obs dataframe containing the observations
#' @param target numeric vector of size 2, min and max value
#' @param population logical value, true for population, false for individual prediction
#' @param model tdmore model
#' @param xintercept xintercept corresponding to now date
#'
preparePredictionPlot <- function(data, obs, target, population, model, xintercept) {
  color <- if(population) {predColor()} else {ipredColor()}
  
  ggplotTarget <- data.frame(lower=target[1], upper=target[2])

  plot <- ggplot(mapping=aes(x=TIME, y=CONC)) +
    geom_line(data=data, color=color) +
    geom_ribbon(fill=color, aes(ymin=CONC.lower, ymax=CONC.upper), data=data, alpha=0.1) +
    geom_point(data=obs, aes(x=dateAndTimeToPOSIX(obs$date, obs$time), y=measure), color=samplesColor(), shape=4, size=3) +
    geom_hline(data=ggplotTarget, aes(yintercept=lower), color=targetColor(), lty=2) +
    geom_hline(data=ggplotTarget, aes(yintercept=upper), color=targetColor(), lty=2) +
    geom_vline(xintercept=as.numeric(xintercept), color=nowColor(), size=1, alpha=0.3) +
    labs(y=getYAxisLabel(model))
  return(plot)
}

#'
#' Prepare the timeline.
#' 
#' @param doses dataframe containing the doses
#' @param xlim ggplot xlim argument
#' @param model tdmore model
#' @param xintercept xintercept corresponding to now date
#'
prepareTimelinePlot <- function(doses, xlim, model, xintercept) {
  times <- dateAndTimeToPOSIX(doses$date, doses$time)
  maxDose <- max(doses$dose)
  addSpace <- maxDose*0.15 # Add 15% margin for dose number
  
  plot <- ggplot(doses, aes(x=times, y=dose)) +
    geom_text(aes(x=times, y=dose, label=dose), nudge_x=0, nudge_y=0, check_overlap=T, show.legend=F) +
    geom_linerange(ymin=0, aes(ymax=dose)) +
    coord_cartesian(xlim=xlim, ylim=c(0, maxDose + addSpace)) +
    labs(x="Time", y=getDoseColumnLabel(model, breakLine=F))
  return(plot)
}

#'
#' Merge plots.
#' 
#' @param p1 first plot
#' @param p2 second plot
#'
mergePlots <- function(p1, p2) {
  subplot(
    ggplotly(p1) %>% config(scrollZoom=T, displayModeBar=F, displaylogo=F),
    ggplotly(p2) %>% config(scrollZoom=T, collaborate=F, displayModeBar=F, displaylogo=F),
    nrows = 2, heights = c(0.8, 0.2), widths = c(1), shareX=T, shareY=F, titleX=T, titleY=T
  ) %>% layout(dragmode = "pan")
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
  if (is.null(doses)) {
    return(NULL)
  }
  pos <- dateAndTimeToPOSIX(doses$date, doses$time)
  start <- min(pos)
  stop <- max(pos + 24*60*60)
  xintercept <- as.POSIXct(now) # Must be POSIXct for plotly

  regimen <- data.frame(
    TIME=as.numeric(difftime(pos, start, units="hour")),
    AMT=doses$dose
  )
  
  object <- model
  if (!population) {
    pos2 <- dateAndTimeToPOSIX(obs$date, obs$time)
    observed <- data.frame(
      TIME=as.numeric(difftime(pos2, start, units="hour")),
      CONC=obs$measure
    )
    object <- estimate(model, observed=observed, regimen=regimen, covariates=covs)
  }
  
  data <- predict(
    object,
    newdata = data.frame(TIME=seq(0, max(regimen$TIME)+24, length.out=300), CONC=NA),
    regimen=regimen,
    covariates=covs,
    se=TRUE) %>% as.data.frame()
  data$TIME <- start + data$TIME*60*60
  
  return(list(p1=preparePredictionPlot(data=data, obs=obs %>% filter(use==TRUE), target=target, population=population, model=model, xintercept=xintercept),
              p2=prepareTimelinePlot(doses=doses, xlim=c(min(pos), max(data$TIME)), model=model, xintercept=xintercept)))
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
#'
prepareRecommendation <- function(doses, obs, model, covs, target, now) {
  pos <- dateAndTimeToPOSIX(doses$date, doses$time)
  start <- min(pos)
  stop <- max(pos + 24*60*60)
  regimen <- data.frame(
    TIME=as.numeric(difftime(pos, start, units="hour")),
    AMT=doses$dose
  )
  
  pred <- predict(
    model,
    newdata = data.frame(TIME=seq(0, max(regimen$TIME)+24, length.out=300), CONC=NA),
    regimen=regimen,
    covariates=covs,
    se=TRUE) %>% as.data.frame()
  pred$TIME <- min(pos) + pred$TIME*60*60
  
  pos2 <- dateAndTimeToPOSIX(obs$date, obs$time)
  observed <- data.frame(
    TIME=as.numeric(difftime(pos2, start, units="hour")),
    CONC=obs$measure
  )
  
  fit <- estimate(model, observed=observed, regimen=regimen, covariates=covs)
  ipred <- fit %>%
    predict(newdata = data.frame(TIME=seq(0, max(regimen$TIME)+24, length.out=300), CONC=NA),
            regimen=regimen,
            se=TRUE,
            covariates=covs)
  ipred$TIME <- start + ipred$TIME*60*60
  
  lastDose <- max(regimen$TIME)
  nextDose <- lastDose + 12
  targetDf <- data.frame(TIME=lastDose+48, CONC=target[1]) #trough after next dose
  
  newRegimen <- data.frame(
    TIME=c(regimen$TIME, lastDose+c(12,24,36,48)),
    AMT=c(regimen$AMT, c(NA,NA,NA,NA))
  )
  recommendation <- tdmore::findDose(fit, doseRows=which(is.na(newRegimen$AMT)),regimen = newRegimen, target = targetDf)
  
  newRegimen$AMT[ is.na(newRegimen$AMT)] <- recommendation$dose
  ipredNew <- fit %>%
    predict(newdata = data.frame(TIME=seq(nextDose, max(newRegimen$TIME)+12, length.out=300), CONC=NA),
            regimen=newRegimen,
            covariates=covs,
            se=TRUE) %>%
    mutate(TIME = start + TIME*60*60)
  
  recommendedRegimenFiltered <- recommendation$regimen %>% filter(TIME>=nextDose) %>% mutate(TIME=start + TIME*60*60)
  recommendedRegimen <- recommendation$regimen %>% mutate(TIME=start + TIME*60*60)
  
  return(list(pred=pred, ipred=ipred, ipredNew=ipredNew,
              recommendedRegimenFiltered=recommendedRegimenFiltered,
              recommendedRegimen=recommendedRegimen, start=start))
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
#' @param recommendation recommendation (contains pred, ipred and the recommendation)
#' 
#' @return a list of two plots
#'
prepareRecommendationPlots <- function(doses, obs, model, covs, target, recommendation) {
  if (is.null(doses)) {
    return(NULL)
  }
  
  start <- recommendation$start
  pred <- recommendation$pred
  ipred <- recommendation$ipred
  ipredNew <- recommendation$ipredNew
  recommendedRegimen <- recommendation$recommendedRegimen
  recommendedRegimen$dose <- round(recommendedRegimen$AMT, digits=2)
  
  ggplotTarget <- data.frame(t1=start, t2=start + 4*24*60*60, lower=target[1], upper=target[2])
  
  p1 <- ggplot(mapping=aes(x=TIME, y=CONC)) +
    geom_line(data=ipred, color=ipredColor(), alpha=0.2) +
    geom_line(data=ipredNew, color=recommendationColor()) +
    geom_ribbon(fill=recommendationColor(), aes(ymin=CONC.lower, ymax=CONC.upper), data=ipredNew, alpha=0.2) +
    geom_point(data=obs, aes(x=dateAndTimeToPOSIX(obs$date, obs$time), y=measure), color=samplesColor(), shape=4, size=3) +
    geom_hline(data=ggplotTarget, aes(yintercept=lower), color=targetColor(), lty=2) +
    geom_hline(data=ggplotTarget, aes(yintercept=upper), color=targetColor(), lty=2) +
    labs(y=getYAxisLabel(model))
  newDoses <- recommendedRegimen %>% mutate(date=as.Date(TIME), time=strftime(TIME,"%H:%M"))
  p2 <- prepareTimelinePlot(doses=newDoses, xlim=c(start, max(ipredNew$TIME)), model=model)
  
  return(list(p1=p1, p2=p2))
}