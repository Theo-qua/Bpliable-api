# Use the official R base image
FROM rocker/r-ver:latest

# Install required R packages
RUN R -e "install.packages(c('plumber'))"

# Copy API files to the Docker container
WORKDIR /app
COPY . /app

# Expose API port
EXPOSE 8000

# Run the plumber API
CMD ["R", "-e", "pr <- plumber::plumb('/app/Bpliable_API.R'); 
pr$run(host='0.0.0.0', 
port=8000)"]
