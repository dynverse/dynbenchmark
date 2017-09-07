id=$(docker create dynalysis)
docker cp $id:/dynalysis/analysis/ - > analysis_output.tar
docker rm -v $id
 tar -xvf analysis_output.tar -C analysis_output --strip-components 1
