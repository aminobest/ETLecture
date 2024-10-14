FROM rocker/r-bspm:22.04

# Add the R2U repository
RUN apt-get update && \
    apt-get install --yes software-properties-common wget && \
    wget -q -O- https://eddelbuettel.github.io/r2u/assets/dirk_eddelbuettel_key.asc | tee -a /etc/apt/trusted.gpg.d/cranapt_key.asc && \
    echo "deb [arch=amd64] https://r2u.stat.illinois.edu/ubuntu jammy main" > /etc/apt/sources.list.d/cranapt.list && \
    apt-get update

# Install R packages via apt (binary installation)
RUN apt-get install --yes r-cran-arrow r-cran-plumber r-cran-data.table r-cran-zoo

WORKDIR /app
COPY . /app
EXPOSE 6789
ENTRYPOINT ["R", "-e", "plumber::plumb('scripts/main.R')$run(host = '0.0.0.0', port = 6789, swagger = TRUE)"]
