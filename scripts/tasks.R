library(googlesheets)

tasks <- gs_key("13jHFuMwie7oIxGcyLO95-onFri4M_GJjqgMEUnZliQI") %>%
  gs_read(ws = "Tasks")

tasks <- tasks %>%
  mutate(task_id = seq_len(n())) %>%
  gather("person", "assigned", Y, H, R, W) %>%
  filter(!is.na(assigned)) %>%
  group_by(task_id) %>%
  mutate(n=n()) %>%
  ungroup()

tasks %>% filter(person == "W", `Version require` == "v1", Progress < 1) %>% arrange(`Deadline`, n) %>% View
