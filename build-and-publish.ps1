$version = "0.2"
# docker buildx build --platform linux/amd64,linux/arm64,linux/arm/v7 -t johannesegger/musiorder:$version -t johannesegger/musiorder:latest --push .
docker build -t johannesegger/musiorder:$version -t johannesegger/musiorder:latest .
docker push johannesegger/musiorder:$version
docker push johannesegger/musiorder:latest
