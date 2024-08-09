# Base R Shiny image
FROM rocker/shiny::latest

# Set the working directory
WORKDIR /srv/shiny-server

# Copy the entire package to the working directory
COPY . .

# Install R dependencies from the DESCRIPTION file
RUN R -e "install.packages(c('remotes', BiocManager'))"
RUN R -e "remotes::install_local(dependencies = TRUE, upgrade = TRUE)"

# Expose the application port
EXPOSE 3838

# Command to run the Shiny app
CMD ["R", "-e", "app <- iSEEbug::iSEEbug(); shiny::runApp(app, host = '0.0.0.0', port = 3838)"]
