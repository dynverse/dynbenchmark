library(dynbenchmark)
library(tidyverse)

library(htmltools)
library(glue)
library(whisker)

experiment("04-method_characterisation/method_testing/site")

#   ____________________________________________________________________________
#   Helper functions                                                        ####

# escape slashes
escape_slashes <- function(x) {gsub("/", "_", x) %>% gsub("-", "_", .)}

# produce clean stdout/stderr
get_output_collapsable <- function(x, name) {
  code <- str_replace_all(x, "\n+", "\n") %>%
    str_replace_all("\n", "<br>") %>%
    HTML %>%
    tags$code()

  tags$div(
    class = "card",
    tags$div(
      class = "card-header",
      `data-toggle` = "collapse",
      `data-target` = glue("#{name}"),
      style = "cursor: pointer",
      tags$h5(
        class = "mb-0",
        tags$button(
          class = "btn btn-link",
          fontawesome::fa("arrow-right"),
          name
        )
      )
    ),
    tags$div(
      class = "collapse",
      id = name,
      tags$div(
        class = "card-body",
        code
      )
    )
  )
}

# save ggplot image
get_image <- function(plot, name, width = 400, height = 400) {
  image_location <- derived_file(glue("img/{name}.png"))
  dir.create(dirname(image_location), showWarnings = FALSE)
  ggsave(image_location, plot)

  tags$img(
    src = gsub(derived_file(), "", image_location),
    style = glue("max-width: {width}px; max-height: {height}px")
  )
}

# method status colors
method_statuses <- tribble(
  ~method_status, ~color, ~icon,
  "method_error", "#FF4136", fontawesome::fa("times-circle", fill="white"),
  "execution_error", "#85144b", fontawesome::fa("times", fill="white"),
  "memory_limit", "#FF851B", fontawesome::fa("memory", fill="white"),
  "time_limit", "#FFDC00", fontawesome::fa("clock", fill="white"),
  "low_correlation", "#39CCCC", fontawesome::fa("thumbs-down", fill="white"),
  "success", "#2ECC40", fontawesome::fa("check-circle", fill="white")
)

generate_method_status_badge <- function(method_status, full = TRUE) {
  tags$span(
    class = "badge badge-secondary",
    style = glue("background-color: {method_statuses$color[match(method_status, method_statuses$method_status)]}; font-size: 1em"),
    HTML(method_statuses$icon[match(method_status, method_statuses$method_status)]),
    ifelse(full, label_long(method_status), ""),
    title = method_status
  )
}


#   ____________________________________________________________________________
#   Process outputs                                                         ####

# read outputs
output <- read_rds(derived_file("output.rds", experiment = "04-method_characterisation/method_testing"))

# generate method specific pages
dataset_ids <- unique(output$dataset_id)
datasets <- load_datasets(dataset_ids) %>% tmap(identity) %>% set_names(dataset_ids)

output_plots <- pmap_df(output, function(model, method_id, dataset_id,...) {
  dataset <- datasets[[dataset_id]]
  grouping <- dataset$prior_information$groups_id %>% deframe()

  plot_cells <- if (is_wrapper_with_trajectory(model)) {
    if (is_wrapper_with_dimred(model)) {
      plot_dimred(model, grouping = grouping)
    } else {
      plot_graph(model, grouping = grouping)
    }
  } else {
    NULL
  }

  plot_topology <- if (is_wrapper_with_trajectory(model)) {
    plot_topology(model)
  } else {
    NULL
  }

  tibble(
    plot_cells = list(plot_cells),
    plot_topology = list(plot_topology)
  )
})
output <- output[, setdiff(colnames(output), colnames(output_plots))] %>% bind_cols(output_plots)

# add number per method
output <- output %>%
  arrange(method_id) %>%
  group_by(method_id) %>%
  mutate(
    method_output_ix = row_number(),
    method_output_n = n()
  ) %>%
  ungroup()

methods_output <- output %>% group_by(method_id) %>% summarise() %>% left_join(dynmethods::methods, "method_id")


#   ____________________________________________________________________________
#   Create pages                                                            ####

# recreate directory
fs::dir_delete(derived_file())
fs::dir_copy(data_file(), derived_file("../"))

# base html template
template_base <- read_file(data_file("base.html"))

# side bar links
generate_sidebar <- function(active = NULL) {
    tags$div(
      tags$ul(
        class = "nav flex-column",
        tags$li(
          class = "nav-item",
          tags$a(
            class = "nav-link",
            href = "index.html",
            "Overview"
          )
        )
      ),
      tags$h6(
        class = "sizebar-heading d-flex justify-content-between align-items-center text-muted px-3 mt-4 mb-1",
        tags$span(
          "Methods"
        )
      ),
      pmap(methods_output, function(method_id, name, ...) {
        method_output <- output %>% filter(method_id == !!method_id)
        tags$li(
          class = "nav-item",
          tags$a(
            class = "nav-link",
            href = glue("{method_id}.html"),
            tags$div(
              name,
              tags$span(
                style = "float: right",
                map(method_output$method_status, generate_method_status_badge, full = FALSE)
              )
            )
          )
        )
      }) %>% tags$ul(class = "nav flex-column")
    )
}


##  ............................................................................
##  Index page                                                              ####

metric_ids <- intersect(metrics$metric_id, colnames(output))
dataset_ids <- sort(unique(output$dataset_id))

# generate overview table
overview_table <- c(
  tags$tr(
      tags$th("Method"),
      map(dataset_ids, ~tags$th(.))
  ) %>% list(),
  pmap(methods_output, function(method_id, name, ...) {
    method_output <- output %>% filter(method_id == !!method_id)

    tags$tr(
      class = "clickable-row",
      `data-href` = glue("{method_id}.html"),
      tags$td(name),
      map(dataset_ids, function(dataset_id) {
        dataset_output <- method_output %>% filter(dataset_id == !!dataset_id)
        if (nrow(dataset_output) == 1) {
          tags$td(generate_method_status_badge(dataset_output$method_status))
        } else {
          tags$td()
        }
      })

    )
  })
) %>% tags$table(class = "table table-sm")

# render overview
list(
  sidebar = as.character(generate_sidebar()),
  title = "Overview",
  content = as.character(overview_table)
) %>%
  whisker.render(template_base, .) %>%
  write_file(derived_file("index.html"))


##  ............................................................................
##  Method pages                                                            ####

generate_method_content <- function(method_output) {
  tmap(method_output, function(out) {
    print(out$method_id)

    if (!is.null(out$plot_cells) && !is.null(out$plot_topology)) {
      images <-
        tags$div(
          get_image(out$plot_cells, glue("{out$method_id}_{out$method_output_ix}_plot_cells")),
          get_image(out$plot_topology, glue("{out$method_id}_{out$method_output_ix}_plot_topology"))
        )

    } else {
      images = ""
    }

    tags$div(
      class = paste0("tab-pane fade", ifelse(out$method_output_ix == 1, " show active", "")),
      id = escape_slashes(out$dataset_id),
      tags$div(images),
      tags$div(get_output_collapsable(out$stdout, "stdout")),
      tags$div(get_output_collapsable(out$stderr, "stderr")),
      tags$div(get_output_collapsable(out$error_message, "error_message"))
    )
  })
}

pwalk(methods_output, function(method_id, name, ...) {
  method_output <- output %>% filter(method_id == !!method_id)

  tabs <- tags$nav(
    tags$div(
      class = "nav nav-tabs",
      pmap(method_output, function(dataset_id, method_status, method_output_ix, ...) {
        tags$a(
          class = paste0("nav-item nav-link", ifelse(method_output_ix == 1, " active", "")),
          `data-toggle` = "tab",
          href = glue("#{escape_slashes(dataset_id)}"),
          dataset_id,
          generate_method_status_badge(method_status)
        )
      })
    )
  )

  tabs_content <- tags$div(
    class = "tab-content",
    generate_method_content(method_output)
  )

  content <- tags$div(
    tabs,
    tabs_content
  )

  list(
    sidebar = as.character(generate_sidebar()),
    title = name,
    content = as.character(content)
  ) %>%
    whisker.render(template_base, .) %>%
    write_file(derived_file(glue("{method_id}.html")))
})



#   ____________________________________________________________________________
#   Deploy                                                                  ####
site_folder <- "../sites/dynmethods_automated_testing/"
system(glue("cp -R {derived_file()}* {site_folder}"))
