library(dynbenchmark)
library(tidyverse)
library(purrr)

experiment("gource")

files <- c(
  list.files(derived_file("git_output"), "*.txt", full.names = TRUE),
  derived_file("google_drive.txt"),
  list.files(raw_file("deprecated_repositories"), "*.txt", full.names = TRUE)
)

user_map <- c(
  Zouter = "Wouter Saelens",
  zouter = "Wouter Saelens",
  falexwolf = "Alex Wolf",
  herrinca = "Chuck Herring",
  "Robrecht Cannoodt",
  "Helena Todorov",
  "Yvan Saeys",
  "Daniel C. Ellwanger" = "Daniel C Ellwanger",
  "Manu Setty",
  "Philipp A" = "Philipp Angerer",
  koenvandenberge = "Koen Van den Berge"
)
changes <-
  files %>%
  map_df(~read_delim(., delim = "|", col_names = c("time", "user", "type", "path"))) %>%
  filter(
    user %in% setdiff(unique(c(user_map, names(user_map))), ""),
    !grepl("docs/node_modules", path)
  ) %>%
  mutate(
    time = pmax(time, 1459502746),
    user = ifelse(user %in% names(user_map), user_map[user], user),
    path = path %>%
      gsub("^(/google_drive/.*)", "\\1.gdrive", .) %>%
      gsub("(.*)Dockerfile$", "\\1.Dockerfile", .) %>%
      gsub("(.*)Singularity\\.([^/]*)$", "\\1\\2.Singularity", .)
  ) %>%
  group_by(path) %>%
  arrange(time) %>%
  mutate(type = c("A", type[-1])) %>%
  ungroup() %>%
  arrange(time) %>%
  mutate(filename = gsub(".*/", "", path))

# remove duplicated dynmethods commits
dynmethods_commits <- changes %>% filter(grepl("^/dynmethods", path))
changes <- bind_rows(
  changes %>% anti_join(dynmethods_commits %>% select(-path), by = c("time", "user", "type", "filename")),
  dynmethods_commits
) %>%
  arrange(time)

write_delim(changes %>% select(-filename), derived_file("combined_out.txt"), delim = "|", col_names = FALSE)

captions <-
  read_delim(raw_file("gource_captions"), delim = "|", col_names = c("date", "caption")) %>%
  mutate(date = as.integer(as.POSIXct(date)))

write_delim(captions, derived_file("gource_captions"), delim = "|", col_names = FALSE)

system(stringr::str_glue(
  "gource -1920x1080 -s 1 -a 0.2 --multi-sampling --key {derived_file('combined_out.txt')} --caption-size 40 --user-image-dir {raw_file('avatar/')} --logo {raw_file('logo.png')} --caption-file {derived_file('gource_captions')} -o - | ",
  "ffmpeg -y -r 60 -f image2pipe -vcodec ppm -i - -vcodec libx264 -preset ultrafast -pix_fmt yuv420p ",
  "-crf 1 -threads 0 -bf 0 {derived_file('gource.mp4')}"
))

# changes_df <- changes %>%
#   filter(!grepl("automated_test", path)) %>%
#   mutate(
#     posix = as.POSIXct(time, origin = "1970-01-01"),
#     is_doc = grepl("google_drive|dyndocs", path),
#     tmp_first = str_replace(path, "^/([^/]*)/.*$", "\\1"),
#     tmp_second = str_replace(path, "^/([^/]*)/([^/]*)/.*$", "\\2"),
#     project = ifelse(grepl("^/[^/]*$", path), "dynverse", ifelse(grepl("^/[^/]*/dyn$", path), tmp_second, tmp_first))
#   ) %>%
#   filter(!is.na(posix))
# changes_df$project %>% table
#
# ggplot(changes_df, aes(x = as.factor(format(posix, format = "%y/%m")), alpha = is_doc)) +
#   geom_bar(aes(fill = user)) +
#   cowplot::theme_cowplot()
#
# ggplot(changes_df) +
#   ggridges::geom_density_ridges(aes(posix, y = user, fill = user)) +
#   cowplot::theme_cowplot()
#
# ggplot(changes_df) +
#   ggridges::geom_density_ridges(aes(posix, y = project, fill = project)) +
#   cowplot::theme_cowplot()
#
# posix_range <- changes_df$posix %>% range()
# dens_x <- seq(posix_range[[1]], posix_range[[2]], length.out = 1000)
# densities <- changes_df %>%
#   group_by(project) %>%
#   do({
#     dens <- density(as.numeric(.$posix))
#     data_frame(project = .$project[[1]], x = dens$x, y = dens$y, ynorm = y / max(y))
#   }) %>%
#   ungroup() %>%
#   mutate(x = as.POSIXct(x, origin = "1970-01-01"))
#
# den_summ <- densities %>% group_by(project) %>% arrange(x) %>% slice(1) %>% ungroup() %>% arrange(x)
#
# densities <- densities %>%
#   mutate(project_f = factor(project, levels = rev(den_summ$project)))
#
# ggplot(densities) + geom_point(aes(x, project_f, colour = ynorm)) + scale_colour_distiller(palette = "RdBu")
