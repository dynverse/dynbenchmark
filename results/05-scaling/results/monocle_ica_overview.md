# monocle_ica
![Overview](monocle_ica.svg)

## ERROR CLUSTER 1

 * Number of instances: 2
 * Dataset ids: scaling_0002, scaling_0050

Last 10 lines of scaling_0002:
```
Loading required package: splines
Loading required package: DDRTree
Loading required package: irlba
Removing 1 outliers
Error in parametricDispersionFit(disp_table[row.names(disp_table) %in%  : 
  Parametric dispersion fit failed. Try a local fit and/or a pooled estimation. (See '?estimateDispersions')
Calls: <Anonymous> ... estimateDispersionsForCellDataSet -> parametricDispersionFit
In addition: Warning message:
In dgamma(y, 1/disp, scale = mu * disp, log = TRUE) : NaNs produced
Execution halted
```

## ERROR CLUSTER 2

 * Number of instances: 4
 * Dataset ids: scaling_0003, scaling_0029, scaling_0030, scaling_0040

Last 10 lines of scaling_0003:
```
Loading required package: stats4
Loading required package: splines
Loading required package: DDRTree
Loading required package: irlba
Removing 2 outliers
Error in if (coefs[2] < 0) { : missing value where TRUE/FALSE needed
Calls: <Anonymous> ... estimateDispersionsForCellDataSet -> parametricDispersionFit
In addition: Warning message:
In dgamma(y, 1/disp, scale = mu * disp, log = TRUE) : NaNs produced
Execution halted
```

## ERROR CLUSTER 3

 * Number of instances: 2
 * Dataset ids: scaling_0011, scaling_0015

Last 10 lines of scaling_0011:
```
Loading required package: DDRTree
Loading required package: irlba
Error in glm.fit(x = numeric(0), y = numeric(0), weights = NULL, start = c(1e-06,  : 
  object 'fit' not found
Calls: <Anonymous> ... parametricDispersionFit -> glm -> eval -> eval -> glm.fit
In addition: Warning messages:
1: In glm.fit(x = numeric(0), y = numeric(0), weights = NULL, start = c(1e-06,  :
  no observations informative at iteration 1
2: glm.fit: algorithm did not converge 
Execution halted
```

## ERROR CLUSTER 4

 * Number of instances: 100
 * Dataset ids: scaling_0024, scaling_0036, scaling_0039, scaling_0041, scaling_0051, scaling_0052, scaling_0065, scaling_0073, scaling_0074, scaling_0075, scaling_0076, scaling_0077, scaling_0088, scaling_0093, scaling_0102, scaling_0103, scaling_0105, scaling_0106, scaling_0107, scaling_0121, scaling_0138, scaling_0139, scaling_0146, scaling_0150, scaling_0151, scaling_0152, scaling_0163, scaling_0181, scaling_0195, scaling_0213, scaling_0214, scaling_0216, scaling_0226, scaling_0227, scaling_0238, scaling_0251, scaling_0255, scaling_0274, scaling_0277, scaling_0298, scaling_0327, scaling_0330, scaling_0339, scaling_0341, scaling_0361, scaling_0382, scaling_0387, scaling_0388, scaling_0390, scaling_0391, scaling_0417, scaling_0444, scaling_0451, scaling_0452, scaling_0453, scaling_0456, scaling_0476, scaling_0502, scaling_0503, scaling_0504, scaling_0506, scaling_0522, scaling_0526, scaling_0550, scaling_0554, scaling_0556, scaling_0594, scaling_0598, scaling_0601, scaling_0602, scaling_0666, scaling_0676, scaling_0677, scaling_0681, scaling_0682, scaling_0750, scaling_0762, scaling_0763, scaling_0764, scaling_0766, scaling_0767, scaling_0839, scaling_0841, scaling_0850, scaling_0851, scaling_0853, scaling_0856, scaling_0907, scaling_0908, scaling_0909, scaling_0911, scaling_0943, scaling_0949, scaling_0951, scaling_0975, scaling_0979, scaling_0981, scaling_1047, scaling_1048, scaling_1049

Last 10 lines of scaling_0024:
```
Loading required package: ggplot2
Loading required package: VGAM
Loading required package: stats4
Loading required package: splines
Loading required package: DDRTree
Loading required package: irlba
Error in parametricDispersionFit(disp_table, verbose) : 
  Parametric dispersion fit failed. Try a local fit and/or a pooled estimation. (See '?estimateDispersions')
Calls: <Anonymous> ... estimateDispersionsForCellDataSet -> parametricDispersionFit
Execution halted
```

## ERROR CLUSTER 5

 * Number of instances: 6
 * Dataset ids: scaling_0031, scaling_0149, scaling_0225, scaling_0275, scaling_0321, scaling_0600

Last 10 lines of scaling_0031:
```
Loading required package: stats4
Loading required package: splines
Loading required package: DDRTree
Loading required package: irlba
Error in parametricDispersionFit(disp_table, verbose) : 
  Parametric dispersion fit failed. Try a local fit and/or a pooled estimation. (See '?estimateDispersions')
Calls: <Anonymous> ... estimateDispersionsForCellDataSet -> parametricDispersionFit
In addition: Warning message:
glm.fit: algorithm did not converge 
Execution halted
```

## ERROR CLUSTER 6

 * Number of instances: 21
 * Dataset ids: scaling_0035, scaling_0063, scaling_0135, scaling_0177, scaling_0211, scaling_0217, scaling_0272, scaling_0326, scaling_0450, scaling_0455, scaling_0501, scaling_0505, scaling_0553, scaling_0680, scaling_0760, scaling_0848, scaling_0849, scaling_0910, scaling_0947, scaling_0948, scaling_1045

Last 10 lines of scaling_0035:
```
Loading required package: VGAM
Loading required package: stats4
Loading required package: splines
Loading required package: DDRTree
Loading required package: irlba
Removing 1 outliers
Error in parametricDispersionFit(disp_table[row.names(disp_table) %in%  : 
  Parametric dispersion fit failed. Try a local fit and/or a pooled estimation. (See '?estimateDispersions')
Calls: <Anonymous> ... estimateDispersionsForCellDataSet -> parametricDispersionFit
Execution halted
```

## ERROR CLUSTER 7

 * Number of instances: 27
 * Dataset ids: scaling_0046, scaling_0125, scaling_0162, scaling_0168, scaling_0179, scaling_0194, scaling_0200, scaling_0221, scaling_0237, scaling_0242, scaling_0297, scaling_0305, scaling_0306, scaling_0336, scaling_0360, scaling_0368, scaling_0426, scaling_0483, scaling_0542, scaling_0580, scaling_0644, scaling_0675, scaling_0727, scaling_0815, scaling_0885, scaling_1019, scaling_1046

Last 10 lines of scaling_0046:
```
Loading required package: DDRTree
Loading required package: irlba
Removing 2 outliers
Warning message:
In dgamma(y, 1/disp, scale = mu * disp, log = TRUE) : NaNs produced
[1] 5
Error in vector("list", gamma(n + 1)) : 
  vector size specified is too large
Calls: <Anonymous> ... extract_good_ordering -> order_p_node -> <Anonymous>
Execution halted
```

## ERROR CLUSTER 8

 * Number of instances: 19
 * Dataset ids: scaling_0104, scaling_0126, scaling_0140, scaling_0167, scaling_0199, scaling_0215, scaling_0267, scaling_0307, scaling_0328, scaling_0329, scaling_0385, scaling_0403, scaling_0448, scaling_0465, scaling_0475, scaling_0482, scaling_0531, scaling_0814, scaling_0852

Last 10 lines of scaling_0104:
```
Loading required package: irlba
Removing 1 outliers
[1] 4
 *** caught segfault ***
address 0xa1, cause 'memory not mapped'
Traceback:
 1: get.all.shortest.paths(mst, from = V(mst)[root_node])
 2: pq_helper(dp_mst, use_weights = FALSE, root_node = root_cell)
 3: monocle::orderCells(cds, num_paths = data$groups_n)
An irrecoverable exception occurred. R is aborting now ...
```

## ERROR CLUSTER 9

 * Number of instances: 1
 * Dataset ids: scaling_0145

Last 10 lines of scaling_0145:
```
Loading required package: splines
Loading required package: DDRTree
Loading required package: irlba
Error in parametricDispersionFit(disp_table, verbose) : 
  Parametric dispersion fit failed. Try a local fit and/or a pooled estimation. (See '?estimateDispersions')
Calls: <Anonymous> ... estimateDispersionsForCellDataSet -> parametricDispersionFit
In addition: Warning messages:
1: glm.fit: algorithm did not converge 
2: glm.fit: algorithm did not converge 
Execution halted
```

## ERROR CLUSTER 10

 * Number of instances: 30
 * Dataset ids: scaling_0180, scaling_0201, scaling_0243, scaling_0254, scaling_0308, scaling_0340, scaling_0369, scaling_0370, scaling_0389, scaling_0416, scaling_0427, scaling_0428, scaling_0429, scaling_0454, scaling_0484, scaling_0485, scaling_0517, scaling_0525, scaling_0543, scaling_0555, scaling_0581, scaling_0582, scaling_0599, scaling_0645, scaling_0646, scaling_0647, scaling_0678, scaling_0679, scaling_0728, scaling_0816

Last 10 lines of scaling_0180:
```
Loading required package: DDRTree
Loading required package: irlba
Removing 1 outliers
[1] 4
Error in vector("list", gamma(n + 1)) : vector size cannot be infinite
Calls: <Anonymous> ... extract_good_ordering -> order_p_node -> <Anonymous>
In addition: Warning message:
In combinat::permn(q_level_list, fun = order_q_node, dist_matrix) :
  value out of range in 'gammafn'
Execution halted
```

## ERROR CLUSTER 11

 * Number of instances: 1
 * Dataset ids: scaling_0276

Last 10 lines of scaling_0276:
```
Loading required package: irlba
Error in parametricDispersionFit(disp_table, verbose) : 
  Parametric dispersion fit failed. Try a local fit and/or a pooled estimation. (See '?estimateDispersions')
Calls: <Anonymous> ... estimateDispersionsForCellDataSet -> parametricDispersionFit
In addition: Warning messages:
1: In log(ifelse(y == 0, 1, y/mu)) : NaNs produced
2: step size truncated due to divergence 
3: In log(ifelse(y == 0, 1, y/mu)) : NaNs produced
4: In log(ifelse(y == 0, 1, y/mu)) : NaNs produced
Execution halted
```

## ERROR CLUSTER 12

 * Number of instances: 1
 * Dataset ids: scaling_0491

Last 10 lines of scaling_0491:
```
Loading required package: VGAM
Loading required package: stats4
Loading required package: splines
Loading required package: DDRTree
Loading required package: irlba
Removing 6 outliers
[1] 100
Error in as.igraph.vs(e1, toadd) : Invalid vertex names
Calls: <Anonymous> ... +.igraph -> add_edges -> as.igraph.vs -> as.igraph.vs
Execution halted
```

## ERROR CLUSTER 13

 * Number of instances: 21
 * Dataset ids: scaling_0495, scaling_0537, scaling_0549, scaling_0630, scaling_0662, scaling_0694, scaling_0711, scaling_0745, scaling_0779, scaling_0797, scaling_0833, scaling_0863, scaling_0874, scaling_0896, scaling_0941, scaling_0990, scaling_1004, scaling_1005, scaling_1018, scaling_1032, scaling_1033

Last 10 lines of scaling_0495:
```
Loading required package: ggplot2
Loading required package: VGAM
Loading required package: stats4
Loading required package: splines
Loading required package: DDRTree
Loading required package: irlba
Removing 1 outliers
[1] 10
Error: node stack overflow
Execution halted
```

## ERROR CLUSTER 14

 * Number of instances: 6
 * Dataset ids: scaling_0574, scaling_0633, scaling_0715, scaling_0802, scaling_0878, scaling_1009

Last 10 lines of scaling_0574:
```
Loading required package: splines
Loading required package: DDRTree
Loading required package: irlba
Error in parametricDispersionFit(disp_table, verbose) : 
  Parametric dispersion fit failed. Try a local fit and/or a pooled estimation. (See '?estimateDispersions')
Calls: <Anonymous> ... estimateDispersionsForCellDataSet -> parametricDispersionFit
In addition: Warning messages:
1: In log(ifelse(y == 0, 1, y/mu)) : NaNs produced
2: step size truncated due to divergence 
Execution halted
```

## ERROR CLUSTER 15

 * Number of instances: 17
 * Dataset ids: scaling_0616, scaling_0632, scaling_0664, scaling_0696, scaling_0713, scaling_0730, scaling_0747, scaling_0781, scaling_0799, scaling_0835, scaling_0875, scaling_0886, scaling_0897, scaling_0955, scaling_0961, scaling_0967, scaling_0973

Last 10 lines of scaling_0616:
```
Loading required package: ggplot2
Loading required package: VGAM
Loading required package: stats4
Loading required package: splines
Loading required package: DDRTree
Loading required package: irlba
Removing 2 outliers
[1] 10
Error: memory exhausted (limit reached?)
Execution halted
```

## ERROR CLUSTER 16

 * Number of instances: 1
 * Dataset ids: scaling_0761

Last 10 lines of scaling_0761:
```
Loading required package: irlba
Error in parametricDispersionFit(disp_table, verbose) : 
  Parametric dispersion fit failed. Try a local fit and/or a pooled estimation. (See '?estimateDispersions')
Calls: <Anonymous> ... estimateDispersionsForCellDataSet -> parametricDispersionFit
In addition: Warning messages:
1: In log(ifelse(y == 0, 1, y/mu)) : NaNs produced
2: step size truncated due to divergence 
3: In log(ifelse(y == 0, 1, y/mu)) : NaNs produced
4: step size truncated due to divergence 
Execution halted
```

## ERROR CLUSTER 17

 * Number of instances: 1
 * Dataset ids: scaling_0864

Last 10 lines of scaling_0864:
```
2: step size truncated due to divergence 
3: glm.fit: algorithm did not converge 
4: glm.fit: algorithm did not converge 
5: glm.fit: algorithm did not converge 
6: glm.fit: algorithm did not converge 
7: In parametricDispersionFit(disp_table[row.names(disp_table) %in%  :
  Dispersion fit did not converge.
[1] 38
Error: memory exhausted (limit reached?)
Execution halted
```

## ERROR CLUSTER 18

 * Number of instances: 1
 * Dataset ids: scaling_0991

Last 10 lines of scaling_0991:
```
Loading required package: ggplot2
Loading required package: VGAM
Loading required package: stats4
Loading required package: splines
Loading required package: DDRTree
Loading required package: irlba
Removing 4 outliers
[1] 96
Error: segfault from C stack overflow
Execution halted
```


