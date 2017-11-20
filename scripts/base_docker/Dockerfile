# get the base image, the rocker/verse has R, RStudio and pandoc
FROM rocker/verse:latest

# install rstan on top of this
FROM jrnold/rstan

# set maximum dlls to 250
ENV R_MAX_NUM_DLLS 250

# update aptitude
RUN sudo apt-get update

# install many python requirements, all else will be installed in virtual environments
RUN sudo apt-get install python3-dev python3-pip -y
RUN pip3 install virtualenv

RUN sudo apt-get install python3-tk -y # for pySCUBA

RUN . /etc/environment \
  && sudo apt-get update \
  && sudo apt-get install subversion -y \
  && sudo apt-get install libudunits2-dev libgsl-dev libsdl1.2-dev libreadline-dev imagemagick libfftw3-dev libudunits2-dev -y

RUN python3 -m pip install numpy matplotlib pandas six jinja2 python-dateutil pytz pyparsing cycler tqdm python-igraph rpy2 Cython scipy statsmodels sklearn seaborn

# copy all local dynalysis files to docker
COPY . dynalysis

# set up github path, given by an argument in the docker build
ARG GITHUB_PAT
ENV GITHUB_PAT=$GITHUB_PAT

# build package
RUN . /etc/environment \
  && R -e "devtools::install_github('rcannood/dyneval', dependencies=TRUE, upgrade_dependencies=TRUE)" \
  && R -e "devtools::install_github('rcannood/dynmethods', dependencies=TRUE, upgrade_dependencies=TRUE)" \
  && R -e "devtools::install_github('Zouter/dyngen', dependencies=TRUE, upgrade_dependencies=TRUE)" \
  && R -e "devtools::install('dynalysis', dependencies=TRUE, upgrade_dependencies=TRUE)"
