
# Metric conformity

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

| \# | script/folder                                                                 | description                                                                                                              |
| :- | :---------------------------------------------------------------------------- | :----------------------------------------------------------------------------------------------------------------------- |
| 1  | [📄`generate_reference_datasets.R`](01-generate_reference_datasets.R)          | Generation of toy datasets used as reference to assess metric conformity                                                 |
| 2  | [📄`evaluate_perturbations.R`](02-evaluate_perturbations.R)                    | Perturbing the reference datasets (for example shuffling the cells) to check the effect of this perturbation on a metric |
| 3  | [📄`assess_conformity.R`](03-assess_conformity.R)                              | Using the scores on perturbed datasets, assess whether the metrics follow certain rules                                  |
| 4  | [📄`assess_similarity.R`](04-assess_similarity.R)                              | Assess the similarity between metrics                                                                                    |
|    | [📄`helper-create_perturbation_images.R`](helper-create_perturbation_images.R) | Helper for creating small images for each rule                                                                           |
|    | [📄`helper-rules.R`](helper-rules.R)                                           | Helper functions containing the rules                                                                                    |

The results of this experiment are available
[here](https://github.com/dynverse/dynbenchmark_results/tree/master/02-metrics/02-metric_conformity).
