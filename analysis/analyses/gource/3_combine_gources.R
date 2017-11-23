library(dynalysis)
library(tidyverse)
library(purrr)

experiment("gource")

files <- c(
  list.files(derived_file("git_output"), "*.txt", full.names = TRUE),
  derived_file("google_drive.txt"),
  list.files(raw_file("deprecated_repositories"), "*.txt", full.names = TRUE)
)

user_map <- c(Zouter = "Wouter Saelens")
changes <- files %>%
  map_df(~read_delim(., delim = "|", col_names = c("time", "user", "type", "path"))) %>%
  mutate(user = ifelse(user %in% names(user_map), user_map[user], user)) %>%
  filter(user %in% c("Wouter Saelens", "Robrecht Cannoodt", "Helena Todorov")) %>%
  group_by(path) %>%
  arrange(time) %>%
  mutate(type = c("A", type[-1])) %>%
  ungroup() %>%
  arrange(time)

write_delim(changes, derived_file("combined_out.txt"), delim = "|", col_names = FALSE)

system(pritt(
  "gource -1920x1080 -s 3 {derived_file('combined_out.txt')} --user-image-dir {raw_file('avatar/')} -o - | ",
  "ffmpeg -y -r 60 -f image2pipe -vcodec ppm -i - -vcodec libx264 -preset ultrafast -pix_fmt yuv420p ",
  "-crf 1 -threads 0 -bf 0 {derived_file('gource.mp4')}"
))
