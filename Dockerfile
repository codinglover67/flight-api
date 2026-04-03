FROM rocker/r-ver:4.3.1

RUN apt-get update && apt-get install -y \
    libssl-dev \
    libcurl4-openssl-dev \
    libxml2-dev \
    libsodium-dev \
    libz-dev \
    zlib1g-dev \
    libgit2-dev \
    cmake \
    && rm -rf /var/lib/apt/lists/*

RUN R -e "install.packages(c('httpuv', 'sodium'), repos='https://cran.r-project.org')"
RUN R -e "install.packages('plumber', repos='https://cran.r-project.org')"
RUN R -e "install.packages(c('randomForest', 'xgboost'), repos='https://cran.r-project.org')"

WORKDIR /app
COPY . .

EXPOSE 8000
CMD ["Rscript", "entrypoint.R"]