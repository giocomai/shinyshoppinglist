FROM rocker/shiny-verse
MAINTAINER Giorgio Comai (g@giorgiocomai.eu)

RUN R -e "install.packages(c('DT'), repos='http://cran.rstudio.com/')"
RUN R -e "remotes::install_github('hadley/emo')"

COPY shiny-customized.config /etc/shiny-server/shiny-server.conf

RUN rm -rf /srv/shiny-server/
RUN mkdir -p /srv/shiny-server
COPY shiny/ /srv/shiny-server/
RUN mkdir -p /srv/shiny-server/items
RUN chown -R shiny:shiny /srv/shiny-server/items

VOLUME /srv/shiny-server/


