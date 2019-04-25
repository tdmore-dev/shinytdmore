# Use the pre-built tdmore image
FROM rfaelens/tdmore:latest

# Set the working directory to /app
WORKDIR /app
COPY . /app

# prepare for installation of packages via apt-get
RUN apt-get update

# Install dev version of RxODE
RUN apt-get install -y libudunits2-dev
RUN R -e 'devtools::install_github("nlmixrdevelopment/RxODE@937597db213208ccef737f2bb532a88416ea139e")'

RUN apt-get install -y libv8-dev
RUN apt-get install -y libssl-dev
RUN apt-get install -y libsasl2-dev
RUN R -e 'install.packages("jsonvalidate")'
RUN R -e 'install.packages("rjson")'
RUN R -e 'install.packages("plumber")'
RUN R -e 'install.packages("mongolite")'

## Requirements for Shiny app
RUN R -e 'install.packages("DT")'
RUN R -e 'install.packages("plotly")'
RUN R -e 'install.packages("rhandsontable")'

# Do not consider dependencies here
# This solves the issue that tdmore-dev/tdmore is a private repository,
# to which devtools will not have access...
# Devtools should use the pre-installed tdmore from the base Docker image
RUN R -e 'devtools::install(dependencies=FALSE)'
