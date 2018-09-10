# Base image https://hub.docker.com/u/rocker/
FROM rocker/rstudio

## extra R packages on ./DockerConfig/requirements.R in
## Copy requirements.R to container directory /tmp
## COPY ./DockerConfig/requirements.R /tmp/requirements.R 
## install required libs on container
## RUN Rscript /tmp/requirements.R

# create an R user
ENV USER rstudio

## Copy your working files over
## The $USER defaults to `rstudio` but you can change this at runtime
COPY ./analysis /home/$USER/analysis
COPY ./data /home/$USER/data





