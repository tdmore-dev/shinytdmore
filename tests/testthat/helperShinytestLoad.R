## This file searches for all directories within "shinytest"
## and executes an appropriate testApp
##
## Some caveats
## 1) PhantomJS uses the QT library to render
## Different platforms will render differently (e.g. buttons are rendered differently on Linux vs Windows)
## 2) PhantomJS will compare the content of plots
## Because of #1, the plotting area may have a different size.
## Also, fonts may be different as well, which results in a different SHA1 hash for the PNG image.
## We recommend to *ignore* all plots, but instead to do one of the following
##    a) export the source data of the plot using exportTestValues()
##    b) use a htmlwidget (like plotly) to render the plot
library(shinytest)
library(testthat)

tmp_lib <- NULL
getFilename <- function(reporter) {
  filenames <- lapply(reporter$reporters, getFilename)
  filename <- c(list(reporter$file_name), filenames) %>%
    purrr::flatten_chr(.data) %>%
    purrr::compact(.data) %>%
    na.omit()
  filename[1]
}

testShiny <- function(appDir) {
  if(is.null(tmp_lib)) {
    tmp_lib <<- tdmore::ensurePackagePresent("shinytdmore", quiet=F)
  }
  if(missing(appDir)) {
    filename <- getFilename(get_reporter())
    appDir <- sub("\\.[rR]$", "", filename)
  }
  test_that(paste0("shinytest for ", appDir, "..."), {
    appDir <- testthat::test_path("../shinytest/", appDir)
    results <- testApp(appDir=appDir)
    shinytest::expect_pass(results)
  })
}

testApp <- function(appDir) {
  withr::local_envvar(c(LC_COLLATE="C"))
  withr::local_libpaths(tmp_lib, action="prefix")
  results <- shinytest::testApp(appDir=appDir, compareImages=!testthat::is_testing(), quiet=FALSE)
  results
}

testShiny_dir <- function() {
  testpath <- testthat::test_path("../shinytest/")
  tests <- dir(testpath)
  for(appDir in tests) {
    testApp(appDir)
  }
}
