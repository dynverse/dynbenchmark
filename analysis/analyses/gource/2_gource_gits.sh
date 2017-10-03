#!/bin/bash
# in dynverse folder
ls -1 -d dyn*/ methods/*/ libraries/*/ | sed 's#\(.*\)/#echo Processing \1; cd \1; git pull; cd ~/Workspace/dynverse; gource --output-custom-log ../gource/\1.txt \1; sed -i -r "s~([^|]*)$~/\1\\1~" ../gource/\1.txt#' | sh
cp dynvaria/drive/gource.txt ../gource/googledrive.txt
cd ../gource
cat *.txt libraries/*.txt methods/*.txt | sort -n | sed 's#Zouter#Wouter Saelens#' | grep "Robrecht Cannoodt\|Wouter Saelens\|Helena Todorov" > combined.out
#gource combined.out --user-image-dir avatar/

gource -1920x1080 -s 3 combined.out --user-image-dir avatar/ -o - | ffmpeg -y -r 60 -f image2pipe -vcodec ppm -i - -vcodec libx264 -preset ultrafast -pix_fmt yuv420p -crf 1 -threads 0 -bf 0 gource.mp4

