# Use the pre-built tdmore image
FROM rfaelens/tdmore:latest

# prepare for installation of packages via apt-get
RUN apt-get update

# install dependent packages
RUN apt-get install -y libv8-dev
RUN apt-get install -y libssl-dev
RUN apt-get install -y libsasl2-dev
RUN R -e 'install.packages("jsonvalidate")'
RUN R -e 'install.packages("rjson")'
RUN R -e 'install.packages("plumber")'
RUN R -e 'install.packages("mongolite")'
RUN R -e 'install.packages("R6")'

## Requirements for Shiny app
RUN R -e 'install.packages("DT")'
RUN R -e 'install.packages("plotly")'
RUN R -e 'install.packages("rhandsontable")'
RUN R -e 'install.packages("shinyBS")'

# Set the working directory to /app
WORKDIR /app
COPY . /app

# Do not consider dependencies here
# This solves the issue that tdmore-dev/tdmore is a private repository,
# to which devtools will not have access...
# Devtools should use the pre-installed tdmore from the base Docker image
RUN R -e 'devtools::install(dependencies=FALSE)'
