# Use the official R base image
FROM rocker/r-ver:latest

# Install required R packages
RUN R -e "install.packages(c('plumber','remotes', 'readr', 'httr', 'magrittr','ggplot2','gapminder','graphics','plotly','aws.s3'))"
RUN R -e "remotes::install_github('Theo-qua/PliableBVS')"

# make app folder
RUN mkdir /app

# Set the working directory in the container
WORKDIR /app

# Copy API files to the container
COPY . /app

# Expose the API port
EXPOSE 8000

# Run the Plumber API using Bpliable-API.R
CMD R -e "pr <- plumber::plumb('/app/PliableBVS_API.R'); pr_run(pr, '0.0.0.0', 8000)"


