library(shinytest)
library(testthat)

context("Test the About panel")

test_that("About panel behaves as expected", {
  skip_on_cran()
  
  appDir <- "../testAboutPanel/"
  expect_true(file.exists(file.path(appDir, "app.R")))
  
  pkg <- devtools::as.package(".")
  tmp_lib <- tempfile("R_LIBS")
  dir.create(tmp_lib)
  on.exit({
    unlink(tmp_lib, recursive = TRUE)
  }, add=T)
  utils::install.packages(repos = NULL,
                          lib = tmp_lib,
                          pkg$path,
                          type = "source",
                          INSTALL_opts = c("--example",
                                           "--install-tests",
                                           "--with-keep.source",
                                           "--with-keep.parse.data",
                                           "--no-multiarch"),
                          quiet = T)
  expect_pass(
    withr::with_libpaths(tmp_lib, {
      testApp(appDir=appDir, compareImages=!testthat::is_testing())
    }, action="prefix")
  )
})
