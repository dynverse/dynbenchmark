
# Gathering all the information we have about the methods

Most information of the methods are contained within their respective
containers (see the [dynmethods](https://github.com/dynverse/dynmethods)
repository, <https://github.com/dynverse/dynmethods>). We gather
additional information from our google sheets
(<https://docs.google.com/spreadsheets/d/1Mug0yz8BebzWt8cmEW306ie645SBh_tDHwjVw4OFhlE>),
which also contains the quality control for each
methods.

| \# | script/folder                                                  | description                                              |
| :- | :------------------------------------------------------------- | :------------------------------------------------------- |
| 1  | [📄`group_methods_into_tools.R`](01-group_methods_into_tools.R) | Grouping methods into tools                              |
| 2  | [📄`process_quality_control.R`](02-process_quality_control.R)   | Downloading and processing the quality control worksheet |
| 3  | [📄`add_quality_control.R`](03-add_quality_control.R)           | Add QC scores to methods and tools tibble                |
