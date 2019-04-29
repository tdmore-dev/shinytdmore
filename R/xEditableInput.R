
#'
#' Get editable input.
#'
#' @param inputId inputId
#' @param type type
#' @param value value
#' @param options options
#' @return an editable input
#' @export
#'
editableInput <- function(inputId, type, value = "", options = list()) {
  type <- match.arg(
    type,
    c("text",
      "textarea",
      "select",
      "date",
      "combodate",
      "checklist")
  )

  element    <- buildElement(inputId, value)
  script     <- buildScript(inputId, type, options)
  dependency <- buildDependency()

  htmltools::tagList(element, script, dependency)

}

#'
#' Update an editable input.
#'
#' @param session inputId
#' @param inputId type
#' @param value value
#' @param options options
#' @export
#'
updateEditableInput <- function(session, inputId, value = NULL) {
  message <- dropNulls(list(value = value))
  session$sendInputMessage(inputId, message)
}

buildScript <- function(inputId, type, options) {
  if (type == "combodate") {
    # Hack: TODO not to implement that like this
    optionsJson <- '{"type":"combodate","placement":"auto","format":"YYYY-MM-DD HH:mm","template":"YYYY-MM-DD HH:mm","viewformat":"YYYY-MM-DD HH:mm"}'
  } else {
    options <- c(options, list(type = type, placement = "auto"))
    optionsJson <- jsonlite::toJSON(options, auto_unbox = TRUE, force = TRUE)
  }
  
  selector <- sprintf("[id='%s']", inputId)
  if (type == "combodate") {
    js <- sprintf("$(\"%s\").editable(%s);", selector, optionsJson)
    js <- paste0(js, "$.fn.combodate.defaults.minYear = 2010;\n")
    js <- paste0(js, "$.fn.combodate.defaults.maxYear = 2025;")
  } else {
    js <- sprintf("$(\"%s\").editable(%s);", selector, optionsJson)
  }
  
  htmltools::tags$script(js)
}

buildElement <- function(inputId, value) {
  htmltools::tags$a(
    id    = inputId,
    class = "shinyEditable-bound-input",
    value
  )
}

dropNulls <- function(x) {
  x[!vapply(x, is.null, FUN.VALUE=logical(1))]
}

buildDependency <- function() {
  
  jquery_dep <- htmltools::htmlDependency(
    name       = "jquery",
    version    = "1.12.4",
    src        = c(href = "shared"),
    script     = "jquery.min.js"
  )
  
  bs_dep <- htmltools::htmlDependency(
    name       = "bootstrap",
    version    = "3.3.7",
    src        = c(href = "shared/bootstrap"),
    script     = "js/bootstrap.min.js",
    stylesheet = "css/bootstrap.min.css"
  )
  
  editable_dep <- htmltools::htmlDependency(
    name       = "bootstrap-editable",
    version    = "1.5.1",
    src        = c(href = "//cdnjs.cloudflare.com/ajax/libs/x-editable/1.5.1/bootstrap3-editable"),
    script     = "js/bootstrap-editable.min.js",
    stylesheet = "css/bootstrap-editable.css"
  )
  
  shiny::addResourcePath(
    prefix = "shinyEditable",
    directoryPath = system.file("js", package = "shinytdmore")
  )
  editableInput_dep <- htmltools::htmlDependency(
    name       = "editableInput",
    version    = "0.1.0",
    src        = c(href = "shinyEditable"),
    script     = "editableInput.js"
  )
  
  htmltools::tagList(
    jquery_dep,
    bs_dep,
    editable_dep,
    editableInput_dep
  )
}

