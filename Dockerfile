FROM rocker/r-ver:4.3.1

RUN apt-get update && apt-get install -y libssl-dev libcurl4-openssl-dev libxml2-dev

RUN R -e "install.packages(c('plumber','randomForest','xgboost'), repos='https://cran.r-project.org')"

WORKDIR /app
COPY . .

EXPOSE 8000
CMD ["Rscript", "entrypoint.R"]