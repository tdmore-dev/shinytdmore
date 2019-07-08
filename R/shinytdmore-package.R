loadModel <- function(modName, Rfile) {
  if(interactive()) cat("Loading model ", modName, " ...\n")
  result <- source(Rfile, keep.source = TRUE)
  result$value
}

.onAttach <- function(libname, pkgname) {

}

# Lazy load all models
.onLoad <- function(libname, pkgname) {
  modelDir <- system.file("models", package=pkgname)
  files <- dir(modelDir)
  modelLibrary <- c()
  for(i in tools::file_path_sans_ext(files)) {
    modelLibrary <- c(modelLibrary, i)
    e1 <- new.env()
    e1$Rfile = file.path(modelDir, paste0(i, ".R") )
    e1$modName = i
    delayedAssign(x=i,
                  value={
                    loadModel(modName, Rfile)
                  },
                  eval.env=e1,
                  assign.env=parent.env(environment()))
  }
  assign(x="modelLibrary", value=modelLibrary, envir=parent.env(environment()))
}