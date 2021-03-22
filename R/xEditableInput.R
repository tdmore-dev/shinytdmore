## Inspired from https://github.com/Yang-Tang/shinyEditable/blob/master/R/editableInput.R
## MIT licensed by Yang Tang, 2018
## 
## This package allows the definition of X-editable inputs, and binds their value to shiny.
## See https://vitalets.github.io/x-editable/ for more information

#' Create an in-place editable input control
#'
#' @param inputId The `input` slot that will be used to access the value.
#' @param type Type of input. See [X-editable](http://github.com/vitalets/x-editable) for supported input types.
#' @param value Initial value.
#' @param options A list of general and input-type-dependent options for
#'   [X-editable](http://github.com/vitalets/x-editable). Details
#'   [here](http://vitalets.github.io/x-editable/docs.html#editable).
#'
#' @return An in-place editable input control that can be added to a UI definition.
#' 
#' @note Inspired from https://github.com/Yang-Tang/shinyEditable/blob/master/R/editableInput.R and adapted to include combodate
#' 
#' @export
#'
#' @examples
#' editableInput("foo", "text", "bar")
editableInput <- function(inputId, type, value = "", options = list()) {
  element    <- buildElement(inputId, value)
  script     <- buildScript(inputId, type, options)
  dependency <- buildDependency()

  htmltools::tagList(element, script, dependency)
}

#' Create an in-place editable combodate. This function adds some default options for the format.
#' 
#' @details 
#' This function adds a default format YYYY-MM-DD HH:mm, and also sets the template and viewformat arguments to the same value.
#' 
#' @inheritParams editableInput
#' @export
editableCombodate <- function(inputId, value="", options=list()) {
  defaultOptions <- list(format="YYYY-MM-DD HH:mm", template="YYYY-MM-DD HH:mm",viewformat="YYYY-MM-DD HH:mm",
                         combodate=list(minYear=1970, maxYear=2030))
  undefinedOptions <- setdiff(names(defaultOptions), names(options))
  options[undefinedOptions] <- defaultOptions[undefinedOptions]
  
  editableInput(inputId, type="combodate", value=value, options)
}

#' Create an in-place editable html5 element.
#' This creates an X-editable of type `text`, with the appropriate template for the input element.
#' 
#' @param type an allowed html5 [input type](https://developer.mozilla.org/en-US/docs/Web/HTML/Element/input)
#' @inheritParams editableInput
#' @export
editableHtml5 <- function(inputId, type="text", options=list()) {
  if(! "tpl" %in% options) options["tpl"] <- paste0('<input type="', type, '">')
  editableInput(inputId, type="text", options=options)
}

basicToJson <- function(value) {
  if(is.list(value)) {
    strings <- lapply(value, basicToJson)
    paste('{',
      paste('"', names(value), '":', strings, sep="", collapse=",")
            ,'}', sep="")
  } else if(is.numeric(value)){
    return(value)
  } else {
    return(paste0('"', as.character(value), '"'))
  }
}

buildScript <- function(inputId, type, options) {
  options <- c(options, list(type = type, placement = "auto"))
  if (requireNamespace("jsonlite", quietly = TRUE)) {
    optionsJson <- jsonlite::toJSON(options, auto_unbox = TRUE, force = TRUE)
  } else {
    #jsonlite not available
    warning("jsonlite not available, using basic toJson function")
    optionsJson <- basicToJson(options)
  }
  
  selector <- sprintf("[id='%s']", inputId)
  js <- sprintf("$(\"%s\").editable(%s);", selector, optionsJson)
  htmltools::tags$script(HTML(js)) #do not perform HTML escaping on the javascript!
}

buildElement <- function(inputId, value) {
  htmltools::tags$a(
    id    = inputId,
    class = "shinyEditable-bound-input",
    value
  )
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
  momentjs_dep <- htmltools::htmlDependency(
    name       = "momentJs",
    version    = "2.24.0",
    src        = c(href = "shinyEditable"),
    script     = "moment-with-locales.min.js"
  )
  
  htmltools::tagList(
    jquery_dep,
    bs_dep,
    editable_dep,
    editableInput_dep,
    momentjs_dep
  )
}


#' Change the value of an in-place editable  input on the client
#'
#' @param session The `session` object passed to function given to shinyServer.
#' @param inputId The id of the input object.
#' @param value The value to set for the input object.
#'
#' @export
#'
#' @examples
#' ## Only run examples in interactive R sessions
#' if (interactive()) {
#'
#' ui <- fluidPage(
#'   editableInput("foo", "text", ""),
#'   actionButton("update", "Update"),
#'   verbatimTextOutput("bar")
#' )
#'
#' server <- function(input, output, session) {
#'   output$bar <- renderPrint(input$foo)
#'   observeEvent(input$update, {
#'     updateEditableInput(session, "foo", sample(month.abb, 1))
#'   })
#' }
#'
#' shinyApp(ui, server)
#'
#' }
updateEditableInput <- function(session, inputId, value = NULL) {
  message <- dropNulls(list(value = value))
  session$sendInputMessage(inputId, message)
}

# Given a vector or list, drop all the NULL items in it
dropNulls <- function(x) {
  x[!vapply(x, is.null, FUN.VALUE=logical(1))]
}