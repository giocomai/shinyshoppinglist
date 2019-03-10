FROM rocker/shiny-verse
MAINTAINER Giorgio Comai (g@giorgiocomai.eu)

RUN R -e "install.packages(c('DT'), repos='http://cran.rstudio.com/')"

COPY shiny-customized.config /etc/shiny-server/shiny-server.conf

COPY shiny/ /srv/shiny-server/


