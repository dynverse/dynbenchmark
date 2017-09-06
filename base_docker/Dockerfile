# get the base image, the rocker/verse has R, RStudio and pandoc
FROM rocker/verse:latest
# install rstan on top of this
FROM jrnold/rstan

ENV GITHUB_PAT 1381e4efbbcc986c4601f8a457943f1b16e31f58

RUN . /etc/environment \
  && sudo apt-get update \
  && sudo apt-get install libudunits2-dev -y \
    # for devtools
  && sudo apt-get install subversion -y \
    # for a certain wrapper
  && sudo apt-get install libgsl-dev libsdl1.2-dev -y


RUN . /etc/environment \
  && R -e "devtools::install_github('rcannood/dyneval')" \
  && R -e "devtools::install_github('Zouter/dyngen')"
