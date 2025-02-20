# Use the official R base image
FROM rocker/r-ver:latest

# Install required R packages
RUN R -e "install.packages(c('plumber'))"
RUN R -e "devtools::install_github("Theo-qua/Bpliable")"

# Set the working directory in the container
WORKDIR /app

# Copy API files to the container
COPY . /app

# Expose the API port
EXPOSE 8000

# Run the Plumber API using Bpliable-API.R
CMD R -e "pr <- plumber::plumb('/app/Bpliable-API.R'); pr$run(host='0.0.0.0', 
port=8000)"

