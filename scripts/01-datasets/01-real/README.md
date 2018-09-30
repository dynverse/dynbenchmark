
# Real datasets

The generation of the real datasets is divided in two parts. We first
download all the (annotated) expression files from sites such as GEO.
Next, we filter and normalise all datasets, and wrap them into the
common trajectory format of
[dynwrap](https://www.github.com/dynverse/dynwrap).

| \# | script/folder                                                     | description                                                                                                                                                                                 |
| :- | :---------------------------------------------------------------- | :------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------ |
| 1  | [📄`download_from_sources.R`](01-download_from_sources.R)          | Downloading the real datasets from their sources (eg. GEO), and constructing the gold standard model, using the helpers in [helpers-download\_from\_sources](helpers-download_from_sources) |
| 2  | [📄`filter_and_normalise.R`](02-filter_and_normalise.R)            | Filtering and normalising the real datasets using `dynbenchmark::process_raw_dataset` All datasets are then saved into the dynwrap format.                                                  |
| 3  | [📄`gather_metadata.R`](03-gather_metadata.R)                      | Gathers some metadata about all the real datasets                                                                                                                                           |
| 4  | [📄`datasets_table.R`](04-datasets_table.R)                        | Creates a table of the datasets in, excuse me, excel (for supplementary material)                                                                                                           |
|    | [📁`helpers-download_from_sources`](helpers-download_from_sources) |                                                                                                                                                                                             |
