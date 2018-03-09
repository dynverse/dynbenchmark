# open selenium server java -Dwebdriver.chrome.driver="./chromedriver" -jar ./selenium-serr-standalone-3.9.0.jar  -port 4449

library(dynalysis)
experiment("gource")

curl::curl_download("https://goo.gl/wz6ovx", derived_file("selenium-server-standalone.jar"))
curl::curl_download("https://chromedriver.storage.googleapis.com/2.35/chromedriver_linux64.zip", derived_file("chrome_driver.zip"))
system(glue::glue("unzip -o {derived_file('chrome_driver.zip')} -d {derived_file()}"))

command <- glue::glue("java -Dwebdriver.chrome.driver='{derived_file('chromedriver')}' -jar {derived_file('selenium-server-standalone.jar')} -port 4449")
system(glue::glue("tmux new-session -d '{command}'"))

# run server




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
  "synthetic_data_methodology", "https://docs.google.com/document/d/1Wko6pMy0pl6DOY-pjiHXoM2zscqOi3pMFot26UfuopM/edit",
  "trajectory_structures", "https://docs.google.com/spreadsheets/d/1mGjj20cCk2OUTZ1oy1gkl-HVHEve6Kg7pRyprMpLIZ4/edit?usp=drive_web&ouid=105833423392613772145",
  "dyngen_data_generation", "https://docs.google.com/document/d/1khvUfXobom5Cdb_K-1QCXuU8EesSqI23iG8bbVQum3A/edit",
  "good_scientific_code", "https://docs.google.com/document/d/1hO0wuBfBvMfmFPSbhVsKolzi7J1M9wp3XgbD2QY4esU/edit",
  "quality_control_terms", "https://docs.google.com/spreadsheets/d/1QKeUlMWlnTYgejtZKQ6PBZaHbLmHhNX323cicHsAOxs/edit?usp=drive_web&ouid=105833423392613772145",
  "dynverse_ideas", "https://docs.google.com/document/d/18PuTgnpUtbGwZvbTyiTsJQCaGRXratjbmsjv9JsnYUc/edit",
  "manuscript_v0", "https://docs.google.com/document/d/1DNuhEHJR03yT8yaAXHc8LYpV7EJDOMOTMdGl76J3zLg/edit",
  "real_datasets", "https://docs.google.com/spreadsheets/d/1SALZ2jt7TZJQJMEvvOwSR2r5yIl50qcGAZ-K5AC4DJo/edit#gid=0",
  "hypotheses_and_tasks", "https://docs.google.com/spreadsheets/d/13jHFuMwie7oIxGcyLO95-onFri4M_GJjqgMEUnZliQI/edit#gid=0",
  "trajectory_inference_methods_public", "https://docs.google.com/spreadsheets/d/1xeRtfa4NZR-FBZOosPv6nY-alfoGE1ZCEP2ceQg6GH0/edit?usp=drive_web&ouid=105833423392613772145",
  "trajectory_inference_methods_private", "https://docs.google.com/spreadsheets/d/1Mug0yz8BebzWt8cmEW306ie645SBh_tDHwjVw4OFhlE/edit?usp=drive_web&ouid=105833423392613772145",
  "manuscript_v1_overview", "https://docs.google.com/document/d/12xrM1GDq4Lk3mHHQzazgL5eYq7yttrKFtUakgU2AvGs/edit",
  "manuscript_v1", "https://docs.google.com/document/d/1BCCaP21N2PXfzhj9H09yEpZz9lLY2HXd_LxTSsJ_wro/edit"
)

warning("Now leave this browser window alone!!!!!! >:)")
links$source <- pmap(links %>% as.list, function(id, link, ...) {
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

  Sys.sleep(10)

  # open arrows
  arrows <- remDr$findElements(using = 'css',".docs-revisions-icon-arrow")
  walk(arrows, ~.$clickElement())

  # download xml
  source <- remDr$getPageSource()[[1]]
  html <- read_html(source)
  html %>% html_nodes("div.docs-revisions-sidebar-revisions-list-container") %>% first()
})


# todo: also parse years
process <- function(data, docname) {
  # per role="rowgroup", process class='docs-revisions-sidebar-date-group' separate from 'docs-revisions-tile'
  id_or_class_xp <- "//div[@class='docs-revisions-tile']//text()"
  values <- xpathSApply( data, id_or_class_xp, xmlValue)

  values <- xpathSApply( data, "//div[@class='docs-revisions-tile']//text()", function(xx) {
    zz <- xpathSApply( xx, "//div[@class='docs-revisions-sidebar-date-group']", xmlValue)
    paste0(paste0(xmlValue(xx), collapse = ";"), "||", paste0(zz, collapse = ";"))
  })


  ## almost
  values <- xpathApply( data, "//div[@role='rowgroup']/@valie", function(xx) {
    zz <- xpathSApply( xx, "//div[@class='docs-revisions-sidebar-date-group']/@value", xmlValue)
    print(zz)
    NULL
    # zz <- xpathSApply( xx, "//div[@class='docs-revisions-tile']//text()", xmlValue)
    # paste(zz, collapse="|")
  })

  names <- c("Helena Todorov", "Yvan Saeys", "Robrecht Cannoodt", "Wouter Saelens")
  names_ix <- which(values %in% names)
  dates_ix <- names_ix - 1
  while (any(values[dates_ix] %in% names)) {
    mehix <- values[dates_ix] %in% names
    dates_ix[mehix] <- dates_ix[mehix] - 1
  }

  df <- data_frame(
    person = values[names_ix],
    date = values[dates_ix]
  ) %>% mutate(
    #posix = as.POSIXct(date, format = "%d %B, %H:%M"),
    posix = as.POSIXct(date, format = "%B %d, %I:%M %p"),
    time = as.integer(posix),
    docname
  ) %>% arrange(time) %>% mutate(
    action = ifelse(seq_len(n()) == 1, "A", "M"),
    final = paste0(time, "|", person, "|", action, "|", docname)
  )
}

df <- seq_len(nrow(links)) %>% map_df(function(i) {
  process(links$source[[i]], links$name[[i]])
}) %>% na.omit

write_lines(df$final, derived_file("google_drive.txt"))
