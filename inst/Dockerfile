FROM ghcr.io/riborings/iseebug:devel

MAINTAINER giulio.benedetti@utu.fi
LABEL authors="giulio.benedetti@utu.fi" \
    description="Docker image containing the miaDash package."

# Set the working directory to /app
WORKDIR /app

# Copy the current directory contents into the container at /app
ADD . /app

# Make port 1234 available to the world outside this container
EXPOSE 1234

# Run the script that launches the Shiny app
CMD Rscript app.R
