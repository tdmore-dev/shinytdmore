library(shinytdmore)

app <- defaultApp(model=tdmore::getModel("pheno", dir=system.file("models", package="shinytdmore")),
                  now=as.POSIXct("2000-01-01 00:00"))
app
