# get the base image, the rocker/verse has R, RStudio and pandoc
FROM dynalysis_base

# required
MAINTAINER Wouter Saelens <wouter.saelens@ugent.be>
MAINTAINER Robrecht Cannoodt <robrecht.cannoodt@ugent.be>

COPY . dynalysis

ENV GITHUB_PAT 1381e4efbbcc986c4601f8a457943f1b16e31f58
ENV R_MAX_NUM_DLLS 300

# build package
RUN . /etc/environment \
  && R -e "devtools::install_github('rcannood/dyneval')" \
  && R -e "devtools::install_github('rcannood/dynmethods')" \
  && R -e "devtools::install('dynalysis')"

RUN . /etc/environment \
  && R -e "library(dyneval);dyneval::check_dependencies()"

RUN . /etc/environment \
  && R -e "rmarkdown::render('dynalysis/analysis/evaluate_toy/1-methods-toys.Rmd')"

RUN . /etc/environment \
  && R -e "rmarkdown::render('dynalysis/analysis/evaluate_toy/2-methods-plots.Rmd')"

# manuscript render
RUN . /etc/environment \
  && R -e "rmarkdown::render('dynalysis/analysis/paper/paper.Rmd')"
