
# Real datasets

The generation of the real datasets is divided in two parts:

| \#  | script/folder                                                     | description                                                                                                                                                                                 |
|:----|:------------------------------------------------------------------|:--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| 1   | [📄`download_from_sources.R`](01-download_from_sources.R)          | Downloading the real datasets from their sources (eg. GEO), and constructing the gold standard model, using the helpers in [helpers-download\_from\_sources](helpers-download_from_sources) |
| 2   | [📄`filter_and_normalise.R`](02-filter_and_normalise.R)            | Filtering and normalising the real datasets using the `dynbenchmark::process_raw_dataset` function.                                                                                         |
|     | [📁`helpers-download_from_sources`](helpers-download_from_sources) |                                                                                                                                                                                             |
