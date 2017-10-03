#!/bin/bash

DERIVED_DATA=analysis/data/derived_data/gource
RAW_DATA=analysis/data/raw_data/gource

cat $DERIVED_DATA/git_output/*.txt $DERIVED_DATA/google_drive.txt $RAW_DATA/deprecated_repositories/*.txt | \
  sort -n | \
  sed 's#Zouter#Wouter Saelens#' | \
  grep "Robrecht Cannoodt\|Wouter Saelens\|Helena Todorov" \
  > $DERIVED_DATA/combined_out.txt

gource -1920x1080 -s 3 $DERIVED_DATA/combined_out.txt --user-image-dir $RAW_DATA/avatar/ -o - | \
  ffmpeg -y -r 60 -f image2pipe -vcodec ppm -i - -vcodec libx264 -preset ultrafast -pix_fmt yuv420p -crf 1 -threads 0 -bf 0 $DERIVED_DATA/gource.mp4

