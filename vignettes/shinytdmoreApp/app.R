library(shinytdmore)

app <- defaultApp(model=tdmore::getModel("pheno", dir=system.file("models", package="shinytdmore")))
app
