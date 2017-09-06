# get the base image, the rocker/verse has R, RStudio and pandoc
FROM rocker/verse:3.4.1

# required
MAINTAINER Wouter Saelens <wouter.saelens@ugent.be>
MAINTAINER Robrecht Cannoodt <robrecht.cannoodt@ugent.be>

COPY . dynalysis

ENV GITHUB_PAT 1381e4efbbcc986c4601f8a457943f1b16e31f58

# go into the repo directory
RUN . /etc/environment \

  # Install linux depedendencies here
  # e.g. need this for ggforce::geom_sina
  && sudo apt-get update \
  && sudo apt-get install libudunits2-dev -y \
  && sudo apt-get install subversion -y \

  # build this compendium package
  && R -e "devtools::install('dynalysis', dependencies=TRUE)" \

 # render the manuscript into a docx, you'll need to edit this if you've
 # customised the location and name of your main Rmd file
  && R -e "rmarkdown::render('dynalysis/analysis/paper/paper.Rmd')"
