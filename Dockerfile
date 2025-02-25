# Base image https://hub.docker.com/u/rocker/
FROM rocker/shiny:latest

# Set environment variables
ENV OPENSSL_CONF="/dev/null"
ENV R_SASS_CACHE=false

# Install required system libraries
RUN apt-get update -qq && apt-get -y --no-install-recommends install \
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
  lbzip2 && \
  rm -rf /var/lib/apt/lists/*

# Update system libraries
RUN apt-get update && apt-get upgrade -y && apt-get clean

# Install PhantomJS
RUN apt-get update && apt-get install -y --no-install-recommends \
    curl \
    bzip2 && \
    mkdir /tmp/phantomjs && \
    curl -L https://github.com/Medium/phantomjs/releases/download/v2.1.1/phantomjs-2.1.1-linux-x86_64.tar.bz2 \
    | tar -xj --strip-components=1 -C /tmp/phantomjs && \
    mv /tmp/phantomjs/bin/phantomjs /usr/local/bin && \
    apt-get purge --auto-remove -y \
    curl \
    bzip2 && \
    apt-get clean && \
    rm -rf /tmp/* /var/lib/apt/lists/*

# Install required R packages
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
htmlwidgets \
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
shinycustomloader \
shinydashboard \
shinyjs \
stringr \
tidyverse \
utils && \
rm -rf /tmp/downloaded_packages

# Copy everything from R folder to the app folder
COPY . /app

# Expose the app port
EXPOSE 8080

# Run the Shiny app on container start
CMD ["R", "-e", "shiny::runApp('/app', host='0.0.0.0', port=8080)"]
