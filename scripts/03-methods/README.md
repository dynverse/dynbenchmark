
# Trajectory inference methods

Here we

-   Compile all the information we have about TI methods
-   Characterise the methods with regards to user and developer friendliness (method quality control)
-   Characterise the methods with regards to prior information, underlying algorithm, possible detectable trajectory types, ...

| \#  | script/folder                                                  | description                                             |
|:----|:---------------------------------------------------------------|:--------------------------------------------------------|
| 1   | [📁`gather_methods_information`](01-gather_methods_information) | Gathering all the information we have about the methods |
| 2   | [📁`tool_qc`](02-tool_qc)                                       | Tool quality control                                    |
| 3   | [📁`method_characterisation`](03-method_characterisation)       |                                                         |
|     | [📁`varia`](varia)                                              |                                                         |

## [Gathering all the information we have about the methods](01-gather_methods_information)

Most information of the methods are contained within their respective containers (see the [dynmethods](https://github.com/dynverse/dynmethods) repository, <https://github.com/dynverse/dynmethods>). We gather additional information from our google sheets (<https://docs.google.com/spreadsheets/d/1Mug0yz8BebzWt8cmEW306ie645SBh_tDHwjVw4OFhlE/edit#gid=1386788683>), which also contains the quality control for each methods.

<table>
<colgroup>
<col width="4%" />
<col width="50%" />
<col width="45%" />
</colgroup>
<thead>
<tr class="header">
<th align="left">#</th>
<th align="left">script/folder</th>
<th align="left">description</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td align="left">1</td>
<td align="left"><a href="01-gather_methods_information/01-group_methods_into_tools.R">📄<code>group_methods_into_tools.R</code></a></td>
<td align="left">Grouping methods into tools</td>
</tr>
<tr class="even">
<td align="left">2</td>
<td align="left"><a href="01-gather_methods_information/02-process_quality_control.R">📄<code>process_quality_control.R</code></a></td>
<td align="left">Downloading and processing the quality control worksheet</td>
</tr>
<tr class="odd">
<td align="left">3</td>
<td align="left"><a href="01-gather_methods_information/03-add_quality_control.R">📄<code>add_quality_control.R</code></a></td>
<td align="left">Add QC scores to methods and tools tibble</td>
</tr>
</tbody>
</table>
