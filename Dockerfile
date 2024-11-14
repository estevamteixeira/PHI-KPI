# Base image https://hub.docker.com/u/rocker/
# Run in R version$major and version$minor
# To know more about the R version

FROM rocker/shiny:latest

# system libraries of general use
## install debian packages

RUN apt-get update -qq \
&& apt-get -y --no-install-recommends install \
libxml2-dev \
libcairo2-dev \
libsqlite3-dev \
libpq-dev \
libssh2-1-dev \
unixodbc-dev \
libcurl4-openssl-dev \
libssl-dev \
libpng-dev \
libicu-dev \
gdal-bin \
libgeos-dev \
libproj-dev \
libudunits2-dev \
lbzip2 \
&& rm -rf /var/lib/apt/lists/

## update system libraries

RUN apt-get update && \
    apt-get upgrade -y && \
    apt-get clean

# install required packages

RUN install2.r --error --skipinstalled --ncpus -1 \
arrow \
bslib \
bsicons \
cancensus \
data.table \
dplyr \
DT \
geojsonsf \
ggplot2 \
ggiraph \
leaflet \
lubridate \
mapview \
markdown \
modules \
plotly \
reactable \
reactablefmtr \
readr \
rintrojs \
sass \
sf \
shiny \
shinyBS \
shinydashboard \
shinyjs \
stringr \
tidyverse \
utils \
webshot \
&& rm -rf /tmp/downloaded_packages

# copy everything from R folder
## to the shiny_save folder
COPY . /app

# expose port
EXPOSE 3838

# run app on container start
CMD ["R", "-e", "shiny::runApp('/app', host='0.0.0.0', port=3838)"]
