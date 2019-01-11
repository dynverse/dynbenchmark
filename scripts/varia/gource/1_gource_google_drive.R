# open selenium server java -Dwebdriver.chrome.driver="./chromedriver" -jar ./selenium-serr-standalone.jar  -port 4449

library(dynbenchmark)
experiment("gource")

curl::curl_download("https://goo.gl/wz6ovx", derived_file("selenium-server-standalone.jar"))
curl::curl_download("https://chromedriver.storage.googleapis.com/2.35/chromedriver_linux64.zip", derived_file("chrome_driver.zip"))
system(glue::glue("unzip -o {derived_file('chrome_driver.zip')} -d {derived_file()}"))

command <- glue::glue("java -Dwebdriver.chrome.driver='{derived_file('chromedriver')}' -jar {derived_file('selenium-server-standalone.jar')} -port 4449")
system(glue::glue("tmux new-session -d '{command}'"))

# run server
# devtools::install_github("JBGruber/RSelenium")
library(RSelenium)
library(rvest)
library(tidyverse)

remDr <- remoteDriver(port = 4449L, browserName="chrome")
remDr$open()
remDr$navigate("https://docs.google.com")
stop("Login in google!!!")


links <- tribble(
  ~id, ~link,
  "claims", "https://docs.google.com/document/d/1Poe2NfubYgotXk8x2Wak_IY32-K6xCM5yJOgyax6vD0/edit",
  # "synthetic_data_methodology", "https://docs.google.com/document/d/1Wko6pMy0pl6DOY-pjiHXoM2zscqOi3pMFot26UfuopM/edit",
  "trajectory_structures", "https://docs.google.com/spreadsheets/d/1mGjj20cCk2OUTZ1oy1gkl-HVHEve6Kg7pRyprMpLIZ4/edit?usp=drive_web&ouid=105833423392613772145",
  "dyngen_data_generation", "https://docs.google.com/document/d/1khvUfXobom5Cdb_K-1QCXuU8EesSqI23iG8bbVQum3A/edit",
  "good_scientific_code", "https://docs.google.com/document/d/1hO0wuBfBvMfmFPSbhVsKolzi7J1M9wp3XgbD2QY4esU/edit",
  "quality_control_terms", "https://docs.google.com/spreadsheets/d/1QKeUlMWlnTYgejtZKQ6PBZaHbLmHhNX323cicHsAOxs/edit?usp=drive_web&ouid=105833423392613772145",
  "dynverse_ideas", "https://docs.google.com/document/d/18PuTgnpUtbGwZvbTyiTsJQCaGRXratjbmsjv9JsnYUc/edit",
  "manuscript_v0", "https://docs.google.com/document/d/1DNuhEHJR03yT8yaAXHc8LYpV7EJDOMOTMdGl76J3zLg/edit",
  "real_datasets", "https://docs.google.com/spreadsheets/d/1SALZ2jt7TZJQJMEvvOwSR2r5yIl50qcGAZ-K5AC4DJo/edit#gid=0",
  "hypotheses_and_datasets", "https://docs.google.com/spreadsheets/d/13jHFuMwie7oIxGcyLO95-onFri4M_GJjqgMEUnZliQI/edit#gid=0",
  "trajectory_inference_methods_public", "https://docs.google.com/spreadsheets/d/1xeRtfa4NZR-FBZOosPv6nY-alfoGE1ZCEP2ceQg6GH0/edit?usp=drive_web&ouid=105833423392613772145",
  "trajectory_inference_methods_private", "https://docs.google.com/spreadsheets/d/1Mug0yz8BebzWt8cmEW306ie645SBh_tDHwjVw4OFhlE/edit?usp=drive_web&ouid=105833423392613772145",
  "manuscript_v1_overview", "https://docs.google.com/document/d/12xrM1GDq4Lk3mHHQzazgL5eYq7yttrKFtUakgU2AvGs/edit",
  "manuscript_v1", "https://docs.google.com/document/d/1BCCaP21N2PXfzhj9H09yEpZz9lLY2HXd_LxTSsJ_wro/edit",
  "manuscript_v2", "https://docs.google.com/document/d/14ZzuesLq5u5l-Gp_r5tSkvwOpxxz9LuXS_HG6GAYfxw/edit",
  "manuscript_v3", "https://docs.google.com/document/d/1je6AaelApu2xcSNbYlvcuTzUeUJBOpTUPHz0L9Houfw/edit",
  "rebuttal", "https://docs.google.com/document/d/1KWgqwrv998yKLcyMv7Zm9o5LrwNRwbWu3gl8LTDVHpA/edit"
)

library(xml2)

warning("Leave this browser window alone! >:) You can do other stuff in the meantime, though.")
revisions_df <- map_df(seq_len(nrow(links)), function(i) {
  cat("Processing ", i, "\n", sep = "")
  id <- links$id[[i]]
  link <- links$link[[i]]

  remDr$navigate(link)
  Sys.sleep(5)

  # open up history
  webElem <- remDr$findElement(using = 'xpath',".//div[@id='docs-file-menu']")
  webElem$clickElement()
  Sys.sleep(1)
  webElem <- remDr$findElement(using = 'xpath',".//span[@aria-label='Version history h']")
  webElem$clickElement()
  Sys.sleep(1)
  webElem <- remDr$findElement(using = 'xpath',".//span[@aria-label='See version history s']")
  webElem$clickElement()

  Sys.sleep(20)
  # open arrows
  arrows <- remDr$findElements(using = 'css',".docs-revisions-icon-arrow")
  walk(rev(arrows), function(arrow) {
    arrow$clickElement()
    Sys.sleep(.5)
  })

  # download xml
  source <- remDr$getPageSource()[[1]]
  html <- read_html(source)
  revisions_container <-
    html %>%
    html_nodes("div.docs-revisions-sidebar-revisions-list-container") %>%
    first()

  rowgroups <- xml_find_all(revisions_container, "//div[@role='rowgroup']")

  map_df(rowgroups, function(rowgroup) {
    if (length(xml_children(rowgroup)) > 0 ) {
      history_specifier <- xml_node(rowgroup, ".docs-revisions-sidebar-date-group") %>% xml_text()

      tiles <- xml_nodes(rowgroup, ".docs-revisions-tile-content-wrapper")
      map_df(tiles, function(tile) {
        date <- tile %>% xml_node(".docs-revisions-tile-text-box") %>% xml_text()
        collaborators <- tile %>% xml_nodes(".docs-revisions-tile-collaborator-name") %>% xml_text()

        tibble(id, link, history_specifier, date, collaborator = unique(collaborators))
      })
    } else {
      NULL
    }
  }) %>%
    filter(!is.na(history_specifier)) %>%
    distinct()
})

rev_proc <- revisions_df %>%
  mutate(
    daymonth = str_replace(date, "^([0-9]+ [^ ,]+).*$", "\\1"),
    hourminute = str_replace(date, "^.*, ([0-9][0-9]:[0-9][0-9])$", "\\1"),
    year1 = ifelse(str_detect(date, "[0-9]+,"), str_replace(date, ".* ([0-9]+), .*", "\\1"), NA),
    year2 = ifelse(str_detect(history_specifier, "[0-9]+$"), str_replace_all(history_specifier, "[^\\d]", ""), NA),
    year3 = format(Sys.Date(), format = "%Y"),
    year = ifelse(!is.na(year1), year1, ifelse(!is.na(year2), year2, year3)),
    new_date = paste0(daymonth, " ", year, ", ", hourminute),
    posix = as.POSIXct(new_date, format = "%d %B %Y, %H:%M", origin = "1970-01-01"),
    time = as.integer(posix),
    docname = paste0("/google_drive/", id)
  ) %>%
  group_by(id) %>%
  arrange(time) %>%
  mutate(
    action = ifelse(seq_len(n()) == 1, "A", "M"),
    gource = paste0(time, "|", collaborator, "|", action, "|", docname)
  ) %>%
  ungroup() %>%
  arrange(time)

write_rds(rev_proc, derived_file("rev_proc.rds"))

write_lines(rev_proc$gource, derived_file("google_drive.txt"))

