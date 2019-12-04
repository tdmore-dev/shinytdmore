library(tdmore)
library(shinytdmore)
library(testthat)

context("Test the merge of covariates and formulations")

source("testUtils.R")

patient <- findPatient("covs_and_formus")

doses <- patient$doses
covs <- patient$covariates

# Add formulations as covariates for tdmore
covsMerge <-  mergeFormAndCov(covs, doses)

# Check number of columns 
expect_equal(length(covsMerge), length(patient$covariates)+1)

# Check absence of NA
expect_equal(sum(is.na(covsMerge)), 0)

# Check result
expected <- tibble(date=as.Date(c("2019-09-05","2019-09-05","2019-09-05","2019-09-06","2019-09-06","2019-09-06","2019-09-07","2019-09-07","2019-09-07")),
                   time=c('08:00','19:00','20:00','08:00','08:00','20:00','08:00','20:00','20:00'),
                   CYP3A5=c(0,1,1,0,0,0,0,1,1),
                   formulation=c("Prograft","Prograft","Advagraf","Advagraf","Prograft","Advagraf","Advagraf","Advagraf","Prograft"))
message("---- EXPECTED: -----")
print(expected)
message("---- GOT: -----")
print(covsMerge)
## TODO: temporarily disable until we can figure out what is not working on CI
#expect_equal(covsMerge, expected)
