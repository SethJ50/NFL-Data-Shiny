FROM rocker/shiny:4.3.1

# Install system dependencies for magick + tidyverse
RUN apt-get update && apt-get install -y \
    libmagick++-dev \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    && rm -rf /var/lib/apt/lists/*

# Install system dependencies
RUN mkdir /home/shiny-app

# Install required R packages
RUN R -e "install.packages(c('shiny', 'shinydashboard', 'shinyWidgets', 'bslib', 'dplyr', 'tidyverse', 'ggrepel', 'remotes', 'nflreadr', 'nflplotR'))"

# Copy the Shiny app into the default Shiny Server directory
COPY app.R /home/shiny-app/app.R

# Expose the Shiny Server port
EXPOSE 8180

# Run Shiny Server
CMD R -e "shiny::runApp('/home/shiny-app', host = '0.0.0.0', port = 8180)"
