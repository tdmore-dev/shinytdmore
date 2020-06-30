ARG DOCKER_TAG=latest
# Use the pre-built tdmore image
FROM rfaelens/tdmore:$DOCKER_TAG
WORKDIR /app/shinytdmore

# prepare for installation of packages via apt-get
RUN apt-get update && apt-get install -y libv8-dev libssl-dev libsasl2-dev

## Initialize renv, see https://environments.rstudio.com/docker#r-packages
COPY renv.lock .
RUN R -e 'renv::restore(library=.libPaths()[1])'

## Ensure all required packages were installed through renv
COPY DESCRIPTION .
RUN R -e 'stopifnot( all(remotes::local_package_deps(dependencies=TRUE) %in% rownames( installed.packages() )) )'

## Install full package; dependencies were installed earlier
## This solves the issue that tdmore-dev/tdmore is a private repository,
## to which devtools will not have access...
## Devtools should use the pre-installed tdmore from the base Docker image
COPY . .
RUN R -e 'devtools::install(dependencies=FALSE)'
