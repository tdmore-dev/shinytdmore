Sys.setenv("R_TESTS" = "")
library(testthat)
library(shinytdmore)
test_check("shinytdmore")
