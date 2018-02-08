# open selenium server java -Dwebdriver.chrome.driver="./chromedriver" -jar ./selenium-serr-standalone-3.9.0.jar  -port 4449

library(dynalysis)
experiment("gource")

curl::curl_download("https://goo.gl/wz6ovx", derived_file("selenium-server-standalone.jar"))
curl::curl_download("https://chromedriver.storage.googleapis.com/2.35/chromedriver_linux64.zip", derived_file("chrome_driver.zip"))
system(glue::glue("unzip -o {derived_file('chrome_driver.zip')} -d {derived_file()}"))

command <- glue::glue("java -Dwebdriver.chrome.driver='{derived_file('chromedriver')}' -jar {derived_file('selenium-server-standalone.jar')} -port 4449")
clipr::write_clip(command)
# system(glue::glue(command, " &"))

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
  "manuscript_1", "https://docs.google.com/document/d/1BCCaP21N2PXfzhj9H09yEpZz9lLY2HXd_LxTSsJ_wro/edit#heading=h.wngcelge211g",
  "methods", "https://docs.google.com/spreadsheets/d/1Mug0yz8BebzWt8cmEW306ie645SBh_tDHwjVw4OFhlE/edit?usp=sharing",
  "manuscript_0", "https://docs.google.com/document/d/1DNuhEHJR03yT8yaAXHc8LYpV7EJDOMOTMdGl76J3zLg/edit",
  "real_datasets", "https://docs.google.com/spreadsheets/d/1SALZ2jt7TZJQJMEvvOwSR2r5yIl50qcGAZ-K5AC4DJo/edit#gid=0",
  "hypotheses", "https://docs.google.com/spreadsheets/d/13jHFuMwie7oIxGcyLO95-onFri4M_GJjqgMEUnZliQI/edit?usp=drive_web&ouid=101203131788757899666"
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


links %>% write_rds(derived_file("google_drive_revisions.rds"))
