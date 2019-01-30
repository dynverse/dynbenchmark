
# Metrics for comparing two trajectories

Good metrics are crucial for an unbiased and comprehensive benchmarking.
Here we test several possible metrics for comparing two trajectories.

## [Metric characterisation](01-metric_characterisation)

A first characterisation of the metrics. For each metric we:

  - generate some examples to get some intuition on how the metric works
  - test the robustness to a metric to stochasticity or parameters when
    appropriate

| \# | script/folder                                                   | description                                                                |
| :- | :-------------------------------------------------------------- | :------------------------------------------------------------------------- |
| 1  | [📄`correlation.R`](01-metric_characterisation/01-correlation.R) | Characterisation of the cor<sub>dist</sub>                                 |
| 2  | [📄`topology.R`](01-metric_characterisation/02-topology.R)       | Characterisation of the Isomorphic, edgeflip and HIM                       |
| 3  | [📄`clustering.R`](01-metric_characterisation/03-clustering.R)   | Characterisation of the F1<sub>branches</sub> and F1<sub>milestones</sub>  |
| 4  | [📄`featureimp.R`](01-metric_characterisation/04-featureimp.R)   | Characterisation of the cor<sub>features</sub> and wcor<sub>features</sub> |

The results of this experiment are available
[here](https://github.com/dynverse/dynbenchmark_results/tree/master/02-metrics/01-metric_characterisation)

## [Metric conformity](02-metric_conformity)

Differences between two datasets should be reflected in certain changes
in the metrics. This can be formalised in a set of rules, for example:

  - If the position of some cells are different than in the reference,
    the score should decrease.
  - If the topology of the network is different than that in the
    reference, the score should not be perfect.
  - The more cells are filtered from the trajectory, the more the score
    should decrease.

Here, we assess whether metrics conforms such rules
empirically:

| \# | script/folder                                                                                      | description                                                                                                              |
| :- | :------------------------------------------------------------------------------------------------- | :----------------------------------------------------------------------------------------------------------------------- |
| 1  | [📄`generate_reference_datasets.R`](02-metric_conformity/01-generate_reference_datasets.R)          | Generation of toy datasets used as reference to assess metric conformity                                                 |
| 2  | [📄`evaluate_perturbations.R`](02-metric_conformity/02-evaluate_perturbations.R)                    | Perturbing the reference datasets (for example shuffling the cells) to check the effect of this perturbation on a metric |
| 3  | [📄`assess_conformity.R`](02-metric_conformity/03-assess_conformity.R)                              | Using the scores on perturbed datasets, assess whether the metrics follow certain rules                                  |
| 4  | [📄`assess_similarity.R`](02-metric_conformity/04-assess_similarity.R)                              | Assess the similarity between metrics                                                                                    |
|    | [📄`helper-create_perturbation_images.R`](02-metric_conformity/helper-create_perturbation_images.R) | Helper for creating small images for each rule                                                                           |
|    | [📄`helper-rules.R`](02-metric_conformity/helper-rules.R)                                           | Helper functions containing the rules                                                                                    |

The results of this experiment are available
[here](https://github.com/dynverse/dynbenchmark_results/tree/master/02-metrics/02-metric_conformity).

## [Aggregation](03-aggregation)

Here, we create some examples here for why and how we aggregate the
scores.

| \# | script/folder                                                               | description |
| :- | :-------------------------------------------------------------------------- | :---------- |
| 1  | [📄`normalisation_reasoning.R`](03-aggregation/01-normalisation_reasoning.R) |             |
| 2  | [📄`aggregation_example.R`](03-aggregation/02-aggregation_example.R)         |             |

The results of this experiment are available
[here](https://github.com/dynverse/dynbenchmark_results/tree/master/02-metrics/03-aggregation).

The results of this experiment are available
[here](https://github.com/dynverse/dynbenchmark_results/tree/master/02-metrics).
