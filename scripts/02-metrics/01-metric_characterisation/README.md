
# Metric characterisation

A first characterisation of the metrics. For each metric we:

  - generate some examples to get some intuition on how the metric works
  - test the robustness to a metric to stochasticity or parameters when
    appropriate

| \# | script/folder                        | description                                                                |
| :- | :----------------------------------- | :------------------------------------------------------------------------- |
| 1  | [📄`correlation.R`](01-correlation.R) | Characterisation of the cor<sub>dist</sub>                                 |
| 2  | [📄`topology.R`](02-topology.R)       | Characterisation of the Isomorphic, edgeflip and HIM                       |
| 3  | [📄`clustering.R`](03-clustering.R)   | Characterisation of the F1<sub>branches</sub> and F1<sub>milestones</sub>  |
| 4  | [📄`featureimp.R`](04-featureimp.R)   | Characterisation of the cor<sub>features</sub> and wcor<sub>features</sub> |

The results of this experiment are available
[here](https://github.com/dynverse/dynbenchmark_results/tree/master/02-metrics/01-metric_characterisation)
