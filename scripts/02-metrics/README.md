
# Metrics for comparing two trajectories

Good metrics are crucial for an unbiased and comprehensive benchmarking. Here we test several possible metrics for comparing two trajectories.

## [Metric characterisation](01-metric_characterisation)

A first characterisation of the metrics. For each metric we:

-   generate some examples to get some intuition on how the metric works
-   test the robustness to a metric to stochasticity or parameters when appropriate

| \#  | script                                                          | description                                                                |
|:----|:----------------------------------------------------------------|:---------------------------------------------------------------------------|
| 1   | [📄`correlation.R`](01-metric_characterisation/01-correlation.R) | Characterisation of the cor<sub>dist</sub>                                 |
| 2   | [📄`topology.R`](01-metric_characterisation/02-topology.R)       | Characterisation of the Isomorphic, edgeflip and HIM                       |
| 3   | [📄`clustering.R`](01-metric_characterisation/03-clustering.R)   | Characterisation of the F1<sub>branches</sub> and F1<sub>milestones</sub>  |
| 4   | [📄`featureimp.R`](01-metric_characterisation/04-featureimp.R)   | Characterisation of the cor<sub>features</sub> and wcor<sub>features</sub> |

## [Metric conformity](02-metric_conformity)

Differences between two datasets should be reflected in certain changes in the metrics. This can be formalised in a set of rules, for example:

-   If the position of some cells are different than in the gold standard, the score should decrease.
-   If the topology of the network is different than that in the gold standard, the score should not be perfect.
-   The more cells are filtered from the trajectory, the more the score should decrease.

Here, we assess whether metrics conforms such rules empirically:

<table>
<colgroup>
<col width="2%" />
<col width="38%" />
<col width="58%" />
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
<td align="left"><a href="02-metric_conformity/01-generate_reference_datasets.R">📄<code>generate_reference_datasets.R</code></a></td>
<td align="left">Generation of toy datasets used as reference to assess metric conformity</td>
</tr>
<tr class="even">
<td align="left">2</td>
<td align="left"><a href="02-metric_conformity/02-evaluate_perturbations.R">📄<code>evaluate_perturbations.R</code></a></td>
<td align="left">Perturbing the reference datasets (for example shuffling the cells) to check the effect of this perturbation on a metric</td>
</tr>
<tr class="odd">
<td align="left">3</td>
<td align="left"><a href="02-metric_conformity/03-assess_conformity.R">📄<code>assess_conformity.R</code></a></td>
<td align="left">Using the scores on perturbed datasets, assess whether the metrics follow certain rules</td>
</tr>
<tr class="even">
<td align="left">4</td>
<td align="left"><a href="02-metric_conformity/04-assess_similarity.R">📄<code>assess_similarity.R</code></a></td>
<td align="left">Assess the similarity between metrics</td>
</tr>
<tr class="odd">
<td align="left"></td>
<td align="left"><a href="helper-create_perturbation_images.R">📄<code>helper-create_perturbation_images.R</code></a></td>
<td align="left">Helper for creating small images for each rule</td>
</tr>
<tr class="even">
<td align="left"></td>
<td align="left"><a href="helper-perturbations.R">📄<code>helper-perturbations.R</code></a></td>
<td align="left">Helper functions for perturbations</td>
</tr>
<tr class="odd">
<td align="left"></td>
<td align="left"><a href="helper-rules.R">📄<code>helper-rules.R</code></a></td>
<td align="left">Helper functions containing the rules</td>
</tr>
<tr class="even">
<td align="left"></td>
<td align="left"><a href="helper-topologies.R">📄<code>helper-topologies.R</code></a></td>
<td align="left">Helper containing different topologies with the same number of milestones</td>
</tr>
</tbody>
</table>
