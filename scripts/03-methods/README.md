
# Trajectory inference methods

Here we

  - Compile all the information we have about TI methods
  - Characterise the methods with regards to user and developer
    friendliness (method quality control)
  - Characterise the methods with regards to prior information,
    underlying algorithm, possible detectable trajectory types,
…

| \# | script/folder                                                  | description                                             |
| :- | :------------------------------------------------------------- | :------------------------------------------------------ |
| 1  | [📁`gather_methods_information`](01-gather_methods_information) | Gathering all the information we have about the methods |
| 2  | [📁`tool_qc`](02-tool_qc)                                       | Tool quality control                                    |
| 3  | [📁`method_characterisation`](03-method_characterisation)       | Method characterisation                                 |
|    | [📁`varia`](varia)                                              |                                                         |

The results of this experiment are available
[here](https://github.com/dynverse/dynbenchmark_results/tree/master/03-methods).

## [Gathering all the information we have about the methods](01-gather_methods_information)

Most information of the methods are contained within their respective
containers (see the [dynmethods](https://github.com/dynverse/dynmethods)
repository, <https://github.com/dynverse/dynmethods>). We gather
additional information from our google sheets
(<https://docs.google.com/spreadsheets/d/1Mug0yz8BebzWt8cmEW306ie645SBh_tDHwjVw4OFhlE>),
which also contains the quality control for each
methods.

| \# | script/folder                                                                                | description                                              |
| :- | :------------------------------------------------------------------------------------------- | :------------------------------------------------------- |
| 1  | [📄`group_methods_into_tools.R`](01-gather_methods_information/01-group_methods_into_tools.R) | Grouping methods into tools                              |
| 2  | [📄`process_quality_control.R`](01-gather_methods_information/02-process_quality_control.R)   | Downloading and processing the quality control worksheet |
| 3  | [📄`add_quality_control.R`](01-gather_methods_information/03-add_quality_control.R)           | Add QC scores to methods and tools tibble                |

## [Tool quality control](02-tool_qc)

Here we compare the user and developer friendliness of the different
trajectory inference
tools

| \# | script/folder                                                 | description                                      |
| :- | :------------------------------------------------------------ | :----------------------------------------------- |
| 1  | [📄`qc_aspects_table.R`](02-tool_qc/01-qc_aspects_table.R)     | Generate a table containing the qc scoresheet    |
| 2  | [📄`qc_scores_overview.R`](02-tool_qc/02-qc_scores_overview.R) | Create an overview figure of the quality control |

## [Method characterisation](03-method_characterisation)

Here we have a look at the diversity of TI
methods

| \# | script/folder                                                                       | description                                                                  |
| :- | :---------------------------------------------------------------------------------- | :--------------------------------------------------------------------------- |
| 1  | [📄`tool_characterisation.R`](03-method_characterisation/01-tool_characterisation.R) | Several figures for looking at the history and diversity of TI methods/tools |
| 2  | [📄`tools_table.R`](03-method_characterisation/02-tools_table.R)                     | Generate a table containing the methods                                      |
