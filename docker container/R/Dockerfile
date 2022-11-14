FROM rstudio/plumber
WORKDIR .
RUN R -e "install.packages('data.table',,repos='http://cran.rstudio.com/', Ncpus=16)"
RUN R -e "install.packages('arrow', repos='http://cran.rstudio.com/', Ncpus=16)"
RUN R -e "install.packages('zoo', repos='http://cran.rstudio.com/', Ncpus=16)"
COPY . .
EXPOSE 6789
ENTRYPOINT ["R", "-e", "plumber::plumb('scripts/main.R')$run(host = '0.0.0.0', port = 6789, swagger = TRUE)"]"