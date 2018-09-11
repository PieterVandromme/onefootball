# Base image https://hub.docker.com/u/rocker/
FROM rocker/rstudio

# create an R user
ENV USER rstudio

## Copy your working files over
## The $USER defaults to `rstudio` but you can change this at runtime
COPY ./analysis /home/$USER/analysis
COPY ./data /home/$USER/data





