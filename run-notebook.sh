# make sure you have Docker installed and that you are in the correct path

# build the docker image (don't forget the . at the end)
docker build -t onefootball .

# run the image
docker run -e PASSWORD=onefoot -p 8787:8787 onefootball