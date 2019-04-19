#' 
#' Allow to provide HTML/javascript code in shiny app for custom inputs.
#' Example if FUN is actionButton:
#' <button id=\"removeButton_1\" type=\"button\" class=\"btn btn-default action-button\" onclick=\"Shiny.onInputChange(&quot;removePatientButton&quot;,  this.id)\">Remove</button>".
#' 
#' @param FUN a compatible shiny function (like actionButton or actionLink)
#' @param id id used to identify the inputs, character
#' @param label input label, character vector, determines output size
#' @param ... additional arguments
#' @return a character vector (length depends on label size)
#' 
#' 
shinyInput <- function(FUN, id, label, ...) {
  inputs <- character(length(label))
  for (i in seq_along(label)) {
    inputLabel <- label[i]
    inputs[i] <- as.character(FUN(paste0(id, "_", i), label=inputLabel, onclick=paste0("Shiny.onInputChange(\"", id, "\", this.id)"), ...))
  }
  return(inputs)
}