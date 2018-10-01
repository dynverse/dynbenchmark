
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
