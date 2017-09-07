# get the base image, the rocker/verse has R, RStudio and pandoc
FROM rocker/verse:latest
# install rstan on top of this
FROM jrnold/rstan

ENV GITHUB_PAT 1381e4efbbcc986c4601f8a457943f1b16e31f58

RUN sudo apt-get update

# python requirements, all else will be installed in virtual environments
RUN sudo apt-get install python3-dev python3-pip -y
RUN pip3 install virtualenv

RUN sudo apt-get install python3-tk -y # for pySCUBA

RUN . /etc/environment \
  && sudo apt-get update \
  && sudo apt-get install libudunits2-dev -y \
    # for devtools
  && sudo apt-get install subversion -y \
    # for a certain wrapper
  && sudo apt-get install libgsl-dev libsdl1.2-dev -y


RUN . /etc/environment \
  && R -e "devtools::install_github('rcannood/dyneval', dependencies=TRUE)" \
  && R -e "devtools::install_github('Zouter/dyngen', dependencies=TRUE)"

# for rpy2
RUN sudo apt-get install libreadline-dev -y

# install some basic python packages used by all methods (do not have to be installed in individual virtual environments)
RUN python3 -m pip install numpy matplotlib pandas six jinja2 python-dateutil pytz pyparsing cycler tqdm python-igraph rpy2 Cython scipy statsmodels sklearn seaborn
