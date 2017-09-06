# get the base image, the rocker/verse has R, RStudio and pandoc
FROM dynalysis_base

# required
MAINTAINER Wouter Saelens <wouter.saelens@ugent.be>
MAINTAINER Robrecht Cannoodt <robrecht.cannoodt@ugent.be>

# python requirements, all else will be installed in virtual environments
RUN easy_install pip
RUN pip install virtualenv

RUN sudo apt-get install python3-tk -y # for pySCUBA

COPY . dynalysis

ENV GITHUB_PAT 1381e4efbbcc986c4601f8a457943f1b16e31f58

RUN . /etc/environment \
# build this compendium package
  && R -e "devtools::install('dynalysis', dependencies=FALSE)"

RUN . /etc/environment \
    # render the manuscript into a docx, you'll need to edit this if you've
    # customised the location and name of your main Rmd file
  && R -e "rmarkdown::render('dynalysis/analysis/1-methods-toys.Rmd')" \
  && R -e "rmarkdown::render('dynalysis/analysis/paper/paper.Rmd')"
