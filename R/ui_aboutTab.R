#' This tab shows general information out the application.
#'
#' @param id namespace id
#' @param htmlFile the html file to use for the contents of the about tab. If missing, we use the default about.html from the package directory.
#' 
#' @return a tabPanel with HTML content
#' @export
#' 
aboutTabUI <- function(id, htmlFile) {
  ns <- NS(id)
  if(missing(htmlFile)) htmlFile <- system.file(package="shinytdmore", "about.html")
  htmlContent <- readLines(htmlFile, warn=FALSE)
  tabPanel(
    "About",
    icon = icon("question"),
    value=id,
    HTML(htmlContent),
    packageVersions()
  )
}

#' About tab server.
#'
#' @param input shiny input
#' @param output shiny output
#' @param session shiny session
#' 
#' @export
#' 
aboutTab <- function(input, output, session) {
  # Nothing to do
}

# Returns an HTML element that describes all installed 
packageVersions <- function() {
  htmltools::tagList(
    htmltools::tags$pre(
      paste(capture.output(print(utils::sessionInfo())), collapse="\n")
    ),
    htmltools::HTML(
      htmlTable::htmlTable(loadedPackages())
    )
  )
}

loadedPackages <- function() {
  df <- loadedNamespaces() %>%
    purrr::map_dfr(function(pkg) {
      desc <- utils::packageDescription(pkg)
      tibble::as_tibble(unclass(desc))
    })
  #df[, c("Package", "Version", "License", "Packaged", "Date/Publication", "Built") ]
  df[, c("Package", "Version", "License", "Built") ]
}
