library(googlesheets)
library(tidyverse)

process_date <- function(dates, time="17:00:00") {
  dates %>% as.POSIXct(format = "%d/%m/%y", tz = "GMT") %>%
    strftime(format="%Y-%m-%d") %>%
    {ifelse(is.na(.), ., paste0(., " ", time))}
}

tasks <- gs_key("13jHFuMwie7oIxGcyLO95-onFri4M_GJjqgMEUnZliQI") %>%
  gs_read(ws = "Tasks") %>%
  mutate(Deadline = process_date(Deadline), Start = process_date(Start, "09:00:00"))

tasks <- tasks %>%
  mutate(task_id = seq_len(n())) %>%
  gather("person", "assigned", Y, H, R, W) %>%
  filter(!is.na(assigned)) %>%
  group_by(task_id) %>%
  mutate(n=n()) %>%
  ungroup()

todo <- tasks %>% filter(person == "W", `Version require` == "v1", Progress < 1) %>% arrange(`Deadline`, n)

todo %>% arrange(Deadline, n) %>% View

library(timevis)

timevis_data <- tasks %>% filter(person == "W", `Version require` == "v1") %>%
  filter(!is.na(Deadline)) %>%
  mutate(
    content=map_chr(Description, ~paste0(strwrap(., simplify=T, prefix="<br>", initial="", width=30), collapse="")),
    start=ifelse(!is.na(Start), Start, Deadline),
    end=ifelse(!is.na(Start), Deadline, NA),
    status=ifelse(Progress == 1, "Done", ifelse(is.na(WIP), "Todo", "WIP")),
    background_color = c("Done"="#CCCCCC", "WIP"="#0074D9", "Todo"="#7FDBFF")[status],
    color = c("Done"="#444444", "WIP"="hsla(210, 100%, 90%, 1.0)", "Todo"="hsla(197, 100%, 20%, 1.0)")[status],
    style = glue::glue("background-color: {background_color}; color: {color}"),
    group=`Related experiment`
  )

widget <- timevis_data %>%
  timevis(
    fit=FALSE,
    options=list(
      align="right",
      height=800,
      width=2000,
      hiddenDates = list(
        list(
          start = '2017-03-04 00:00:00',
          end = '2017-03-06 00:00:00',
          "repeat" = 'weekly'
        ),
        list(
          start = '2017-03-04 17:00:00',
          end = '2017-03-05 09:00:00',
          "repeat" = 'daily'
        )
      ),
      timeAxis = list(scale = 'weekday'),
      start = (as.Date(Sys.time()) - 2) %>% strftime(format="%Y-%m-%d"),
      end = (as.Date(Sys.time()) + 20) %>% strftime(format="%Y-%m-%d")
    ),
    groups= timevis_data %>% group_by(`Related experiment`) %>% summarise() %>% mutate(id=`Related experiment`, content = `Related experiment`)
  )

htmltools::browsable(
  htmltools::tagList(list(
    htmltools::tags$head(
      htmltools::tags$style(".vis-grid.vis-monday {background: #CCC !important;} .vis-grid.vis-tuesday {background: #D8D8D8 !important;} .vis-grid.vis-wednesday {background: #DDD !important;} .vis-grid.vis-thursday {background: #F8F8F8 !important;} .vis-grid.vis-friday {background: #FFF !important;}")
    ),
    widget
  ))
)
