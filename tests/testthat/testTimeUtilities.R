library(shinytdmore)
library(testthat)
library(mongolite)
library(assertthat)
library(rjson)
library(tidyverse)

context("Test the time utilities functions")

Sys.setenv(TZ="Europe/Paris")

test_that("String to POSIX date, Belgian summer time", {
  dbDate <- "2019-05-08 09:00:00 +0200"
  
  posixDate <- stringToPOSIX(dbDate)
  expect_equal(POSIXToPrettyString(posixDate), "2019-05-08 09:00 CEST")
  
  backToDBDate <- POSIXToString(posixDate)
  expect_equal(dbDate, backToDBDate)
})


test_that("String to POSIX date, Belgian winter time", {
  dbDate <- "2018-12-08 09:00:00 +0100"
  
  posixDate <- stringToPOSIX(dbDate)
  expect_equal(POSIXToPrettyString(posixDate), "2018-12-08 09:00 CET")
  
  backToDBDate <- POSIXToString(posixDate)
  expect_equal(dbDate, backToDBDate)
})



Sys.setenv(TZ="America/New_York")

test_that("String to POSIX date, Belgian summer time, viewed in New-York", {
  dbDate <- "2019-05-08 09:00:00 +0200"
  
  posixDate <- stringToPOSIX(dbDate)
  expect_equal(POSIXToPrettyString(posixDate), "2019-05-08 03:00 EDT") # Easter Daylight Time (summer)
  
  backToDBDate <- POSIXToString(posixDate)
  expect_equal("2019-05-08 03:00:00 -0400", backToDBDate) # Makes sense to me
})


test_that("String to POSIX date, Belgian winter time, viewed in New-York", {
  dbDate <- "2018-12-08 09:00:00 +0100"
  
  posixDate <- stringToPOSIX(dbDate)
  expect_equal(POSIXToPrettyString(posixDate), "2018-12-08 03:00 EST") # Easter Daylight Savings Time (winter)
  
  backToDBDate <- POSIXToString(posixDate)
  expect_equal("2018-12-08 03:00:00 -0500", backToDBDate)
})


Sys.setenv(TZ="America/New_York")

test_that("dateAndTimeToPOSIX function works well, called in New-York", {
  date <- as.Date("2019-05-08")
  time <- "10:02"
  expect_equal(POSIXToPrettyString(dateAndTimeToPOSIX(date, time)), "2019-05-08 10:02 EDT") # Easter Daylight Time (summer)
})

Sys.setenv(TZ="Europe/Paris")

test_that("dateAndTimeToPOSIX function works well, called in Paris", {
  date <- as.Date("2019-05-08")
  time <- "10:02"
  expect_equal(POSIXToPrettyString(dateAndTimeToPOSIX(date, time)), "2019-05-08 10:02 CEST")
})

Sys.setenv(TZ="America/New_York")

test_that("POSIX to date/to time functions work well, called in New-York", {
  dbDate <- "2019-05-08 01:00:00 +0200" # 1AM in Paris
  posixDate <- stringToPOSIX(dbDate)
  
  date <- POSIXToDate(posixDate)
  expect_equal(date, as.Date("2019-05-07"))
  
  time <- POSIXToTime(posixDate)
  expect_equal(time, "19:00")
})

Sys.setenv(TZ="Europe/Paris")

test_that("POSIX to date/to time functions work well, called in Paris", {
  dbDate <- "2019-05-08 01:00:00 +0200" # 1AM in Paris
  posixDate <- stringToPOSIX(dbDate)
  
  date <- POSIXToDate(posixDate)
  expect_equal(date, as.Date("2019-05-08"))
  
  time <- POSIXToTime(posixDate)
  expect_equal(time, "01:00")
})


source("testUtils.R")

test_that("String dates correctly converted in patient model", {
  Sys.setenv(TZ="Europe/Paris")
  
  patient <- findPatient("reading_timezones")
  
  pos1 <- dateAndTimeToPOSIX(patient$doses$date[1], patient$doses$time[1])
  pos1Ref <- stringToPOSIX("2019-09-05 07:00:00 +0200")
  expect_equal(pos1, pos1Ref)
  
  pos1b <- dateAndTimeToPOSIX(patient$doses$date[2], patient$doses$time[2])
  pos1bRef <- stringToPOSIX("2019-09-06 07:00:00 +0200")
  expect_equal(pos1b, pos1bRef)
  
  pos2 <- dateAndTimeToPOSIX(patient$measures$date[1], patient$measures$time[1])
  pos2Ref <- stringToPOSIX("2019-09-06 06:00:00 +0200")
  expect_equal(pos2, pos2Ref)
  
  pos3 <- dateAndTimeToPOSIX(patient$covariates$date[1], patient$covariates$time[1])
  pos3Ref <- stringToPOSIX("2019-09-05 04:00:00 +0200")
  expect_equal(pos3, pos3Ref)
  
  pos4 <- dateAndTimeToPOSIX(patient$now_date, patient$now_date)
  pos4Ref <- stringToPOSIX("2019-09-06 08:00:00 +0200")
  expect_equal(pos4, pos4Ref)
})





