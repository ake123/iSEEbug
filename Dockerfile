# Base R Shiny image
FROM rocker/shiny:4.4.0

# Install system dependencies for GSL and GLPK
RUN apt-get update && apt-get install -y \
    libgsl-dev \
    libglpk-dev

# Install R dependencies early to leverage Docker layer caching
RUN R -e "install.packages(c('devtools', 'BiocManager'))"

RUN R -e "if (!requireNamespace('BiocManager', quietly = TRUE)) install.packages('BiocManager'); BiocManager::install(version='devel', ask=FALSE); BiocManager::install('iSEEtree')"

# Install necessary Bioconductor and CRAN packages
RUN R -e "BiocManager::install(c('iSEE', 'iSEEtree', 'mia', 'biomformat', 'SummarizedExperiment', 'SingleCellExperiment', 'TreeSummarizedExperiment', 'shiny', 'shinyjs', 'methods', 'utils'))"

# Set the working directory
WORKDIR /srv/shiny-server

# Copy the DESCRIPTION file first to leverage Docker cache if dependencies haven't changed
COPY DESCRIPTION .

# Install the dependencies based on the DESCRIPTION file
RUN R -e "remotes::install_deps(dependencies = TRUE)"

# Copy the rest of the package code
COPY . .

# Install the local iSEEbug package and output installation logs for debugging
RUN R -e "remotes::install_local('.', dependencies = TRUE, upgrade = TRUE, quiet = FALSE)"

# Verify that the iSEEbug package is installed

# Expose the application port
EXPOSE 8080

# Command to run the Shiny app
CMD ["R", "-e", "app <- iSEEbug::iSEEbug(); shiny::runApp(app, host = '0.0.0.0', port = 8080)"]


