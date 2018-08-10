
Real datasets
-------------

The generation of the real datasets is divided in two parts:

1.  The datasets are downloaded from their respective sources (such as GEO) using sets of scripts specific for each dataset in [01-download\_from\_sources.R](01-download_from_sources.R) (these scripts are located in [helpers-download\_from\_sources](helpers-download_from_sources))
2.  These datasets are filtered and normalised in [02-filter\_and\_normalise.R](02-filter_and_normalise.R). This is done using the dynbenchmark::process\_raw\_dataset function.

Following datasets were ultimately generated:

``` r
# library(tidyverse)
# library(dynbenchmark)
# list_datasets() %>% 
#   filter(source == "real") %>% 
#   select(name)
```
