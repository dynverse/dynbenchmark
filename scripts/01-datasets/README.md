
# Dataset processing and characterisation

The datasets are split in real datasets and synthetic datasets. The real datasets are downloaded and preprocessed first, and characteristics from these datasets (such as the number of cells and genes, library sizes, dropout probabilities, ...) are used to generate synthetic datasets. The datasets are then characterised, after which they are uploaded to Zenodo.

| \#  | script                                                     | description                                    |
|:----|:-----------------------------------------------------------|:-----------------------------------------------|
| 0   | [📄`download_from_zenodo.R`](00-download_from_zenodo.R)     | Downloading the processed datasets from zenodo |
| 1   | [📁`real`](01-real)                                         | Real datasets                                  |
| 2   | [📁`synthetic`](02-synthetic)                               | Synthetic datasets                             |
| 3   | [📄`download_from_prism.R`](03-download_from_prism.R)       | Download the datasets from the cluster         |
| 4   | [📁`dataset_characterisation`](04-dataset_characterisation) | Dataset characterisation                       |
| 5   | [📄`upload_to_zenodo.R`](05-upload_to_zenodo.R)             | Upload the datasets to Zenodo                  |
|     | [📄`hotfix_datasets.R`](hotfix_datasets.R)                  |                                                |

## [Real datasets](01-real)

The generation of the real datasets is divided in two parts:

<table>
<colgroup>
<col width="2%" />
<col width="25%" />
<col width="71%" />
</colgroup>
<thead>
<tr class="header">
<th align="left">#</th>
<th align="left">script</th>
<th align="left">description</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td align="left">1</td>
<td align="left"><a href="01-real/01-download_from_sources.R">📄<code>download_from_sources.R</code></a></td>
<td align="left">Downloading the real datasets from their sources (eg. GEO), and constructing the gold standard model, using the helpers in <a href="helpers-download_from_sources" class="uri">helpers-download_from_sources</a></td>
</tr>
<tr class="even">
<td align="left">2</td>
<td align="left"><a href="01-real/02-filter_and_normalise.R">📄<code>filter_and_normalise.R</code></a></td>
<td align="left">Filtering and normalising the real datasets using the <code>dynbenchmark::process_raw_dataset</code> function.</td>
</tr>
<tr class="odd">
<td align="left"></td>
<td align="left"><a href="helpers-download_from_sources">📁<code>helpers-download_from_sources</code></a></td>
<td align="left"></td>
</tr>
</tbody>
</table>

## [Synthetic datasets](02-synthetic)

Each synthetic dataset is based on some characteristics of some real datasets. These characteristics include:

-   The number of cells and features
-   The number of features which are differentially expressed in the trajectory
-   Estimates of the distribution of the library sizes, average expression, dropout probabilities, ... estimated by [Splatter](https://github.com/Oshlack/splatter).

Here we estimate the parameters of these "platforms" and use them to simulate datasets using different simulators. Each simulation script first creates a design dataframe, which links particular platforms, different topologies, seeds and other parameters specific for a simulator.

The data is then simulated using wrappers around the simulators (see [/package/R/simulators.R](/package/R/simulators.R)), so that they all return datasets in a format consistent with dynwrap.

<table>
<colgroup>
<col width="2%" />
<col width="34%" />
<col width="62%" />
</colgroup>
<thead>
<tr class="header">
<th align="left">#</th>
<th align="left">script</th>
<th align="left">description</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td align="left">1</td>
<td align="left"><a href="02-synthetic/01-estimate_platform.R">📄<code>estimate_platform.R</code></a></td>
<td align="left"></td>
</tr>
<tr class="even">
<td align="left">2a</td>
<td align="left"><a href="02-synthetic/02a-simulate_dyngen_datasets.R">📄<code>simulate_dyngen_datasets.R</code></a></td>
<td align="left"><a href="https://github.com/dynverse/dyngen">dyngen</a>, simulations of regulatory networks which will produce a particular trajectory</td>
</tr>
<tr class="odd">
<td align="left">2b</td>
<td align="left"><a href="02-synthetic/02b-simulate_prosstt_datasets.R">📄<code>simulate_prosstt_datasets.R</code></a></td>
<td align="left"><a href="https://github.com/soedinglab/prosstt">PROSSTT</a>, simulations of tree topologies using random walks</td>
</tr>
<tr class="even">
<td align="left">2c</td>
<td align="left"><a href="02-synthetic/02c-simulate_splatter_datasets.R">📄<code>simulate_splatter_datasets.R</code></a></td>
<td align="left"><a href="https://github.com/Oshlack/splatter">Splatter</a>, simulations of non-linear paths between different states</td>
</tr>
<tr class="odd">
<td align="left">2d</td>
<td align="left"><a href="02-synthetic/02d-simulate_dyntoy_datasets.R">📄<code>simulate_dyntoy_datasets.R</code></a></td>
<td align="left"><a href="https://github.com/dynverse/dyntoy">dyntoy</a>, simulations of toy data using random expression gradients in a reduced space</td>
</tr>
</tbody>
</table>

## [Dataset characterisation](04-dataset_characterisation)

| \#  | script                                                                          | description |
|:----|:--------------------------------------------------------------------------------|:------------|
| 1   | [📄`synthetic.R`](04-dataset_characterisation/1-synthetic.R)                     |             |
| 2   | [📄`real.R`](04-dataset_characterisation/2-real.R)                               |             |
| 3   | [📄`trajectory_type_dag.R`](04-dataset_characterisation/3-trajectory_type_dag.R) |             |
