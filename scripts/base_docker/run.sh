docker build -t dynalysis_base . --file scripts/base_docker/Dockerfile --build-arg GITHUB_PAT=$GITHUB_PAT > log
