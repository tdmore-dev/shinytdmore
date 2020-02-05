## This is a really silly test
## It exists purely to test for C LC_COLLATE.
## 
## The issue is as follows:
## Running tests with devtools::test() uses the system locale, e.g. nl_BE
## Running tests with R CMD CHECK uses the C locale.
## 
## Alphabetical sorting order is different between the C locale (A, B, C, ..., a, b, c, ...) and nl_BE (a, b, c, ..., A, B, C, ...)
## 
## This influences the results from exported values saved to .json, but also the label order generated in ggplotly.
## 
## See ?Comparison

library(shinytest)

app <- ShinyDriver$new("..", loadTimeout = 30000, seed=1234)
app$snapshotInit("test-collation")
app$snapshot(filename = "start.json")

# teardown ----------------------------------------------------------------
p <- app$.__enclos_env__$private$shinyProcess
p$interrupt()
p$wait()