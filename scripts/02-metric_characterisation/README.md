
Metric characterisation and testing
===================================

Good metrics are crucial for an unbiased and comprehensive benchmarking. Here we test several possible metrics for comparing two trajectories.

[Individual metrics](01-individual_metrics)
-------------------------------------------

|   \#| script                                    | description         |
|----:|:------------------------------------------|:--------------------|
|    1| \[`correlation.R`\](01-individual\_metric | s/01-correlation.R) |
|    2| \[`topology.R`\](01-individual\_metrics/0 | 2-topology.R)       |
|    3| \[`clustering.R`\](01-individual\_metrics | /03-clustering.R)   |
|    4| \[`featureimp.R`\](01-individual\_metrics | /04-featureimp.R)   |

[Metric conformity](02-metric_conformity)
-----------------------------------------

Differences between two datasets should be reflected in certain changes in the metrics. This can be formalised in a set of rules, for example:

-   If the position of some cells are different than in the gold standard, the score should decrease.
-   If the topology of the network is different than that in the gold standard, the score should not be perfect.
-   The more cells are filtered from the trajectory, the more the score should decrease.

Here, we assess whether metrics conforms such rules empirically:

| order | script                                                                               | description                                                                                                              |
|:------|:-------------------------------------------------------------------------------------|:-------------------------------------------------------------------------------------------------------------------------|
| 1     | \[`generate_reference_datasets.R`\](02-metric\_conformity/01-generate\_reference\_da | tasets.R) Generation of toy datasets used as reference to assess metric conformity                                       |
| 2     | [`evaluate_perturbations.R`](02-metric_conformity/02-evaluate_perturbations.R)       | Perturbing the reference datasets (for example shuffling the cells) to check the effect of this perturbation on a metric |
| 3     | [`assess_conformity.R`](02-metric_conformity/03-assess_conformity.R)                 | Using the scores on perturbed datasets, assess whether the metrics follow certain rules                                  |
| 4     | [`assess_similarity.R`](02-metric_conformity/04-assess_similarity.R)                 | Assess the similarity between metrics                                                                                    |
|       | \[`helper-create_perturbation_images.R`\](02-metric\_conformity/helper-create\_pert  | urbation\_images.R) Helper for creating small images for each rule                                                       |
|       | [`helper-perturbations.R`](02-metric_conformity/helper-perturbations.R)              | Helper functions for perturbations                                                                                       |
|       | [`helper-rules.R`](02-metric_conformity/helper-rules.R)                              | Helper functions containing the rules                                                                                    |
|       | [`helper-topologies.R`](02-metric_conformity/helper-topologies.R)                    | Helper containing different topologies with the same number of milestones                                                |
