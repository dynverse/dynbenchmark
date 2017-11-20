# get the base image, the rocker/verse has R, RStudio and pandoc
FROM dynalysis_base

# required
MAINTAINER Wouter Saelens <wouter.saelens@ugent.be>
MAINTAINER Robrecht Cannoodt <robrecht.cannoodt@ugent.be>

RUN . /etc/environment \
  && R -e "library(dynmethods);dynmethods::check_dependencies()"

RUN . /etc/environment \
  && R -e "rmarkdown::render('analysis/evaluate_toy/1-methods-toys.Rmd')"

RUN . /etc/environment \
  && R -e "rmarkdown::render('analysis/evaluate_toy/2-methods-plots.Rmd')"

# manuscript render
RUN . /etc/environment \
  && R -e "rmarkdown::render('analysis/paper/paper.Rmd')"
