# recat
![Overview](recat.svg)

## ERROR CLUSTER 1

 * Number of instances: 86
 * Dataset ids: scaling_0001, scaling_0002, scaling_0003, scaling_0004, scaling_0006, scaling_0008, scaling_0010, scaling_0012, scaling_0016, scaling_0019, scaling_0022, scaling_0025, scaling_0041, scaling_0043, scaling_0045, scaling_0047, scaling_0051, scaling_0056, scaling_0061, scaling_0066, scaling_0076, scaling_0082, scaling_0088, scaling_0094, scaling_0106, scaling_0113, scaling_0119, scaling_0120, scaling_0127, scaling_0151, scaling_0157, scaling_0162, scaling_0163, scaling_0167, scaling_0169, scaling_0180, scaling_0181, scaling_0188, scaling_0194, scaling_0195, scaling_0202, scaling_0221, scaling_0226, scaling_0232, scaling_0238, scaling_0243, scaling_0244, scaling_0254, scaling_0276, scaling_0286, scaling_0287, scaling_0297, scaling_0298, scaling_0307, scaling_0309, scaling_0328, scaling_0341, scaling_0350, scaling_0351, scaling_0360, scaling_0361, scaling_0369, scaling_0371, scaling_0391, scaling_0403, scaling_0404, scaling_0417, scaling_0430, scaling_0456, scaling_0466, scaling_0476, scaling_0484, scaling_0486, scaling_0556, scaling_0565, scaling_0573, scaling_0574, scaling_0581, scaling_0583, scaling_0697, scaling_0728, scaling_0783, scaling_0816, scaling_0866, scaling_0979, scaling_1008

Last 10 lines of scaling_0001:
```
Package 'mclust' version 5.4.1
Type 'citation("mclust")' for citing this R package in publications.
Attaching package: 'mclust'
The following object is masked from 'package:purrr':
    map
Loading required package: TSP
[1] 1
Error in .solve_TSP(x, method, control, ...) : NAs not allowed!
Calls: <Anonymous> ... highCorFind -> solve_TSP -> solve_TSP.ATSP -> .solve_TSP
Execution halted
```

## ERROR CLUSTER 2

 * Number of instances: 23
 * Dataset ids: scaling_0093, scaling_0140, scaling_0370, scaling_0428, scaling_0465, scaling_0517, scaling_0543, scaling_0616, scaling_0646, scaling_0647, scaling_0648, scaling_0729, scaling_0730, scaling_0731, scaling_0818, scaling_0819, scaling_0887, scaling_0888, scaling_0934, scaling_0968, scaling_0994, scaling_1021, scaling_1022

Last 10 lines of scaling_0093:
```
Package 'mclust' version 5.4.1
Type 'citation("mclust")' for citing this R package in publications.
Attaching package: 'mclust'
The following object is masked from 'package:purrr':
    map
Loading required package: TSP
Error in if (minDistance < 0 | minDistance > tmp[1]) { : 
  missing value where TRUE/FALSE needed
Calls: <Anonymous> ... forceTSPNext -> forceTSPNext -> forceTSPNext -> forceTSPNext
Execution halted
```

## ERROR CLUSTER 3

 * Number of instances: 4
 * Dataset ids: scaling_0693, scaling_0850, scaling_1017, scaling_1018

Last 10 lines of scaling_0693:
```
Type 'citation("mclust")' for citing this R package in publications.
Attaching package: 'mclust'
The following object is masked from 'package:purrr':
    map
Loading required package: TSP
[1] 1
Error in if (any(c(mu, cholsigma) > signif(.Machine$double.xmax, 6))) { : 
  missing value where TRUE/FALSE needed
Calls: <Anonymous> ... eval -> mclustBIC -> mstep -> eval -> eval -> mstepVVV
Execution halted
```

## ERROR CLUSTER 4

 * Number of instances: 5
 * Dataset ids: scaling_0778, scaling_0814, scaling_0863, scaling_0955, scaling_1019

Last 10 lines of scaling_0778:
```
Package 'mclust' version 5.4.1
Type 'citation("mclust")' for citing this R package in publications.
Attaching package: 'mclust'
The following object is masked from 'package:purrr':
    map
Loading required package: TSP
Error in if (any(c(mu, cholsigma) > signif(.Machine$double.xmax, 6))) { : 
  missing value where TRUE/FALSE needed
Calls: <Anonymous> ... eval -> mclustBIC -> mstep -> eval -> eval -> mstepVVV
Execution halted
```

## ERROR CLUSTER 5

 * Number of instances: 2
 * Dataset ids: scaling_0907, scaling_1003

Last 10 lines of scaling_0907:
```
Package 'mclust' version 5.4.1
Type 'citation("mclust")' for citing this R package in publications.
Attaching package: 'mclust'
The following object is masked from 'package:purrr':
    map
Loading required package: TSP
Error in unchol(cholSigma, upper = TRUE) : 
  NA/NaN/Inf in foreign function call (arg 2)
Calls: <Anonymous> ... mclustBIC -> mstep -> eval -> eval -> mstepEEE -> unchol
Execution halted
```

## ERROR CLUSTER 6

 * Number of instances: 1
 * Dataset ids: scaling_0932

Last 10 lines of scaling_0932:
```
Loading required package: mclust
Package 'mclust' version 5.4.1
Type 'citation("mclust")' for citing this R package in publications.
Attaching package: 'mclust'
The following object is masked from 'package:purrr':
    map
Error in unchol(cholSigma, upper = TRUE) : 
  NA/NaN/Inf in foreign function call (arg 2)
Calls: <Anonymous> ... mclustBIC -> mstep -> eval -> eval -> mstepEEE -> unchol
Execution halted
```


