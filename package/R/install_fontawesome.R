#' @importFrom extrafont fonts font_import
#' @importFrom utils download.file unzip
install_fontawesome <- function() {
  if (!"Font Awesome 5 Free" %in% extrafont::fonts()) {
    # install fontawesome
    release <- "fontawesome-free-5.3.1-web"

    # download and unzip
    url <- paste0("https://github.com/FortAwesome/Font-Awesome/releases/download/5.3.1/", release, ".zip")
    zip_file <- tempfile(fileext = '.zip')
    utils::download.file(url, destfile = zip_file)

    temp_dir <- tempfile()
    dir.create(temp_dir)

    utils::unzip(zipfile = zip_file, exdir = temp_dir)

    font_dir <- paste0(temp_dir, "/", release, "/webfonts/")
    extrafont::font_import(paths = font_dir, pattern = "fa-solid-900\\.ttf", prompt = FALSE)
    extrafont::font_import(paths = font_dir, pattern = "fa-brands-400\\.ttf", prompt = FALSE)
  }
}
