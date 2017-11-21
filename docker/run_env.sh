docker build -t dynalysis_env . --file docker/Dockerfile_env --build-arg GITHUB_PAT=$GITHUB_PAT | tee log
