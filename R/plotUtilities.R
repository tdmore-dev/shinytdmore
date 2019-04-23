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
#' @return a label
#'
getDoseColumnLabel <- function(model, breakLine=T) {
  doseMetadata <- getMetadataByName(model, "DOSE")
  separator <- if(breakLine){"\n"} else{" "}
  label <- if(!is.null(doseMetadata)) {paste0("Dose", separator, "(", doseMetadata$unit, ")")} else {"Dose"}
  return(label)
}

#'
#' Get measure column label.
#' 
#' @param model tdmore model
#' @return a label
#'
getMeasureColumnLabel <- function(model, breakLine=T) {
  output <- model$res_var[[1]]$var # TODO: what if several outputs?
  separator <- if(breakLine){"\n"} else{" "}
  outputMetadata <- getMetadataByName(model, output)
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
#'
preparePredictionPlot <- function(data, obs, target, population, model) {
  color <- if(population) {predColor()} else {ipredColor()}
  
  ggplotTarget <- data.frame(lower=target[1], upper=target[2])

  plot <- ggplot(mapping=aes(x=TIME, y=CONC)) +
    geom_line(data=data, color=color) +
    geom_ribbon(fill=color, aes(ymin=CONC.lower, ymax=CONC.upper), data=data, alpha=0.1) +
    geom_point(data=obs, aes(x=dateAndTimeToPOSIX(obs$date, obs$time), y=measure), color=samplesColor(), shape=4, size=3) +
    geom_hline(data=ggplotTarget, aes(yintercept=lower), color=targetColor(), lty=2) +
    geom_hline(data=ggplotTarget, aes(yintercept=upper), color=targetColor(), lty=2) +
    labs(y=getYAxisLabel(model))
  return(plot)
}

#'
#' Prepare the timeline.
#' 
#' @param doses dataframe containing the doses
#' @param xlim ggplot xlim argument
#' @param model tdmore model
#'
prepareTimelinePlot <- function(doses, xlim, model) {
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
#' @return a list of two plots
#'
preparePredictionPlots <- function(doses, obs, model, covs, target, population) {
  if (is.null(doses)) {
    return(NULL)
  }
  pos <- dateAndTimeToPOSIX(doses$date, doses$time)
  start <- min(pos)
  stop <- max(pos + 24*60*60)
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
  data$TIME <- min(pos) + data$TIME*60*60
  
  return(list(p1=preparePredictionPlot(data=data, obs=obs %>% filter(use==TRUE), target=target, population=population, model=model),
              p2=prepareTimelinePlot(doses=doses, xlim=c(min(pos), max(data$TIME)), model=model)))
}

#'
#' Prepare recommendation.
#' 
#' @param doses doses
#' @param obs observations
#' @param model tdmore model
#' @param covs covariates
#' @param target numeric vector of size 2, min and max value
#' 
#' @return a list with pred, ipred and the recommendation
#'
prepareRecommendation <- function(doses, obs, model, covs, target) {
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
    predict(newdata = data.frame(TIME=seq(max(regimen$TIME), max(newRegimen$TIME)+12, length.out=300), CONC=NA),
            regimen=newRegimen,
            covariates=covs,
            se=TRUE) %>%
    mutate(TIME = start + TIME*60*60)
  
  recommendedRegimen <- recommendation$regimen %>% filter(TIME>lastDose) %>% mutate(TIME=start + TIME*60*60)
  
  return(list(pred=pred, ipred=ipred, ipredNew=ipredNew, recommendedRegimen=recommendedRegimen, start=start))
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
  recommendedRegimen$Dose <- round(recommendedRegimen$AMT, digits=2)
  
  ggplotTarget <- data.frame(t1=start, t2=start + 4*24*60*60, lower=target[1], upper=target[2])
  
  p1 <- ggplot(pred) +
    geom_line(aes(x=TIME, y=CONC, color="Individual"), data=ipred, alpha=0.2) +
    geom_line(aes(x=TIME, y=CONC, color="Recommendation"), data=ipredNew) +
    geom_ribbon(aes(x=TIME, ymin=CONC.lower, ymax=CONC.upper, color="Recommendation", fill="Recommendation"), data=ipredNew, alpha=0.3) +
    geom_point(aes(x=dateAndTimeToPOSIX(obs$date, obs$time), y=measure, color="Samples"),shape=4, size=3, data=obs)+
    geom_text(data=obs,aes(x=dateAndTimeToPOSIX(obs$date, obs$time), y=measure,label=measure, color="Samples"), check_overlap = T, show.legend = TRUE) +
    geom_hline(data=ggplotTarget, aes(yintercept=lower, color="Target"), lty=2)+
    geom_hline(data=ggplotTarget, aes(yintercept=upper, color="Target"), lty=2)+
    labs(y=getYAxisLabel(model))+
    scale_fill_discrete(guide=FALSE)+
    scale_colour_discrete(name="Data",
                          breaks=c("Individual","Recommendation", "Samples"),
                          labels=c("Individual","Recommendation", "Samples"))
  
  p2 <- ggplot(doses)+
    geom_text(aes(x=dateAndTimeToPOSIX(doses$date, doses$time), y=dose,label=dose), nudge_x = 0, nudge_y = 0, check_overlap = T, show.legend = FALSE, alpha=0.2) +
    geom_linerange(ymin=0, aes(x=dateAndTimeToPOSIX(doses$date, doses$time), ymax=dose), alpha=0.2)+
    geom_text(data=recommendedRegimen, aes(x=TIME, y=Dose, label=Dose) , nudge_x = 0, nudge_y = 0, check_overlap = T, show.legend = FALSE) +
    geom_linerange(data=recommendedRegimen, aes(x=TIME,ymax=Dose), ymin=0)+
    coord_cartesian(xlim = c(start,max(c(pred$TIME,recommendedRegimen$TIME)+12*60*60)), ylim = c(0,max(c(doses$dose,recommendedRegimen$Dose)+2)), expand = FALSE) +
    labs(x="Time", y=getDoseColumnLabel(model, breakLine=F))
  return(list(p1=p1, p2=p2))
}