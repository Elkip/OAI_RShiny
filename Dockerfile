FROM rocker/shiny-verse:latest

# RUN R -e "install.packages('shiny', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('shinydashboard', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('DT', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('nnet', repos='http://cran.rstudio.com/')"

COPY app /shiny
COPY data /data

EXPOSE 3838
RUN sudo chown -R shiny:shiny /shiny

CMD ["R", "-e", "shiny::runApp('/shiny')"]
