# projected_slingshot
![Overview](projected_slingshot.svg)

## ERROR CLUSTER 1

 * Number of instances: 80
 * Dataset ids: scaling_0001, scaling_0002, scaling_0003, scaling_0004, scaling_0006, scaling_0008, scaling_0010, scaling_0012, scaling_0016, scaling_0019, scaling_0022, scaling_0025, scaling_0041, scaling_0043, scaling_0045, scaling_0047, scaling_0051, scaling_0056, scaling_0061, scaling_0066, scaling_0076, scaling_0082, scaling_0088, scaling_0094, scaling_0106, scaling_0113, scaling_0120, scaling_0127, scaling_0151, scaling_0157, scaling_0163, scaling_0169, scaling_0181, scaling_0188, scaling_0195, scaling_0202, scaling_0226, scaling_0232, scaling_0238, scaling_0244, scaling_0276, scaling_0287, scaling_0298, scaling_0309, scaling_0341, scaling_0351, scaling_0361, scaling_0371, scaling_0391, scaling_0404, scaling_0417, scaling_0430, scaling_0456, scaling_0466, scaling_0476, scaling_0486, scaling_0556, scaling_0565, scaling_0574, scaling_0583, scaling_0601, scaling_0617, scaling_0633, scaling_0649, scaling_0681, scaling_0698, scaling_0715, scaling_0732, scaling_0766, scaling_0784, scaling_0802, scaling_0820, scaling_0856, scaling_0867, scaling_0878, scaling_0889, scaling_0981, scaling_0995, scaling_1009, scaling_1023

Last 10 lines of scaling_0001:
```
Attaching package: 'purrr'
The following object is masked from 'package:jsonlite':
    flatten
Warning messages:
1: In rgl.init(initValue, onlyNULL) : RGL: unable to open X11 display
2: 'rgl_init' failed, running with rgl.useNULL = TRUE 
Error in pam(dimred, K) : 
  Number of clusters 'k' must be in {1,2, .., n-1}; hence n >= 2
Calls: lapply -> lapply -> FUN -> pam
Execution halted
```

## ERROR CLUSTER 2

 * Number of instances: 290
 * Dataset ids: scaling_0007, scaling_0017, scaling_0026, scaling_0031, scaling_0033, scaling_0034, scaling_0035, scaling_0036, scaling_0037, scaling_0038, scaling_0052, scaling_0053, scaling_0054, scaling_0057, scaling_0058, scaling_0059, scaling_0062, scaling_0067, scaling_0068, scaling_0077, scaling_0080, scaling_0083, scaling_0084, scaling_0085, scaling_0090, scaling_0092, scaling_0095, scaling_0096, scaling_0101, scaling_0107, scaling_0108, scaling_0109, scaling_0110, scaling_0112, scaling_0114, scaling_0115, scaling_0116, scaling_0117, scaling_0122, scaling_0124, scaling_0128, scaling_0129, scaling_0130, scaling_0135, scaling_0136, scaling_0141, scaling_0142, scaling_0143, scaling_0144, scaling_0145, scaling_0147, scaling_0149, scaling_0152, scaling_0153, scaling_0155, scaling_0159, scaling_0164, scaling_0166, scaling_0170, scaling_0171, scaling_0176, scaling_0177, scaling_0182, scaling_0183, scaling_0186, scaling_0189, scaling_0190, scaling_0191, scaling_0196, scaling_0199, scaling_0203, scaling_0204, scaling_0205, scaling_0210, scaling_0211, scaling_0216, scaling_0218, scaling_0220, scaling_0222, scaling_0224, scaling_0227, scaling_0228, scaling_0233, scaling_0234, scaling_0239, scaling_0240, scaling_0241, scaling_0245, scaling_0246, scaling_0251, scaling_0252, scaling_0253, scaling_0256, scaling_0257, scaling_0260, scaling_0261, scaling_0264, scaling_0265, scaling_0266, scaling_0267, scaling_0268, scaling_0269, scaling_0272, scaling_0277, scaling_0278, scaling_0279, scaling_0280, scaling_0288, scaling_0289, scaling_0290, scaling_0291, scaling_0299, scaling_0300, scaling_0301, scaling_0302, scaling_0303, scaling_0304, scaling_0305, scaling_0310, scaling_0311, scaling_0322, scaling_0324, scaling_0325, scaling_0326, scaling_0331, scaling_0333, scaling_0335, scaling_0337, scaling_0339, scaling_0342, scaling_0343, scaling_0344, scaling_0352, scaling_0353, scaling_0354, scaling_0355, scaling_0362, scaling_0363, scaling_0364, scaling_0365, scaling_0366, scaling_0372, scaling_0374, scaling_0382, scaling_0383, scaling_0384, scaling_0385, scaling_0387, scaling_0392, scaling_0393, scaling_0394, scaling_0395, scaling_0405, scaling_0406, scaling_0407, scaling_0408, scaling_0409, scaling_0418, scaling_0419, scaling_0420, scaling_0421, scaling_0422, scaling_0426, scaling_0431, scaling_0432, scaling_0433, scaling_0444, scaling_0445, scaling_0446, scaling_0447, scaling_0451, scaling_0457, scaling_0459, scaling_0467, scaling_0468, scaling_0469, scaling_0477, scaling_0478, scaling_0479, scaling_0487, scaling_0497, scaling_0500, scaling_0506, scaling_0510, scaling_0514, scaling_0518, scaling_0522, scaling_0523, scaling_0526, scaling_0532, scaling_0533, scaling_0534, scaling_0538, scaling_0539, scaling_0540, scaling_0544, scaling_0545, scaling_0550, scaling_0551, scaling_0557, scaling_0558, scaling_0566, scaling_0567, scaling_0568, scaling_0575, scaling_0584, scaling_0593, scaling_0602, scaling_0603, scaling_0604, scaling_0618, scaling_0619, scaling_0620, scaling_0621, scaling_0622, scaling_0623, scaling_0634, scaling_0635, scaling_0637, scaling_0650, scaling_0651, scaling_0666, scaling_0668, scaling_0682, scaling_0683, scaling_0684, scaling_0685, scaling_0700, scaling_0701, scaling_0702, scaling_0703, scaling_0704, scaling_0716, scaling_0717, scaling_0733, scaling_0734, scaling_0736, scaling_0750, scaling_0751, scaling_0752, scaling_0767, scaling_0768, scaling_0769, scaling_0770, scaling_0786, scaling_0787, scaling_0788, scaling_0789, scaling_0790, scaling_0803, scaling_0804, scaling_0805, scaling_0821, scaling_0822, scaling_0839, scaling_0840, scaling_0857, scaling_0858, scaling_0868, scaling_0869, scaling_0870, scaling_0879, scaling_0880, scaling_0890, scaling_0901, scaling_0911, scaling_0912, scaling_0919, scaling_0920, scaling_0921, scaling_0927, scaling_0935, scaling_0936, scaling_0943, scaling_0951, scaling_0952, scaling_0957, scaling_0958, scaling_0963, scaling_0969, scaling_0975, scaling_0982, scaling_0983, scaling_0996, scaling_0997, scaling_0998, scaling_0999, scaling_1010, scaling_1024, scaling_1038

Last 10 lines of scaling_0007:
```
The following object is masked from 'package:jsonlite':
    flatten
Warning messages:
1: In rgl.init(initValue, onlyNULL) : RGL: unable to open X11 display
2: 'rgl_init' failed, running with rgl.useNULL = TRUE 
Using diagonal covariance matrix
Error in colMeans(dimred[names(which(cli == grouping)), , drop = T]) : 
  'x' must be an array of at least two dimensions
Calls: t ... sapply -> sapply -> lapply -> FUN -> colMeans -> colMeans
Execution halted
```

## ERROR CLUSTER 3

 * Number of instances: 109
 * Dataset ids: scaling_0009, scaling_0011, scaling_0015, scaling_0018, scaling_0020, scaling_0021, scaling_0024, scaling_0027, scaling_0029, scaling_0030, scaling_0032, scaling_0039, scaling_0040, scaling_0044, scaling_0046, scaling_0050, scaling_0055, scaling_0060, scaling_0064, scaling_0065, scaling_0072, scaling_0074, scaling_0075, scaling_0081, scaling_0091, scaling_0093, scaling_0102, scaling_0103, scaling_0104, scaling_0105, scaling_0111, scaling_0123, scaling_0125, scaling_0126, scaling_0137, scaling_0138, scaling_0139, scaling_0140, scaling_0146, scaling_0150, scaling_0156, scaling_0167, scaling_0168, scaling_0178, scaling_0179, scaling_0180, scaling_0187, scaling_0198, scaling_0201, scaling_0212, scaling_0213, scaling_0214, scaling_0215, scaling_0221, scaling_0225, scaling_0231, scaling_0237, scaling_0242, scaling_0243, scaling_0254, scaling_0255, scaling_0274, scaling_0275, scaling_0286, scaling_0307, scaling_0308, scaling_0327, scaling_0328, scaling_0329, scaling_0330, scaling_0340, scaling_0369, scaling_0370, scaling_0388, scaling_0389, scaling_0390, scaling_0403, scaling_0428, scaling_0429, scaling_0452, scaling_0453, scaling_0454, scaling_0455, scaling_0485, scaling_0503, scaling_0504, scaling_0505, scaling_0517, scaling_0525, scaling_0543, scaling_0555, scaling_0582, scaling_0598, scaling_0599, scaling_0600, scaling_0647, scaling_0648, scaling_0677, scaling_0679, scaling_0680, scaling_0731, scaling_0762, scaling_0764, scaling_0765, scaling_0819, scaling_0851, scaling_0854, scaling_0855, scaling_0950

Last 10 lines of scaling_0009:
```
The following object is masked from 'package:jsonlite':
    flatten
Warning messages:
1: In rgl.init(initValue, onlyNULL) : RGL: unable to open X11 display
2: 'rgl_init' failed, running with rgl.useNULL = TRUE 
Using diagonal covariance matrix
Error in solve.default(s1 + s2) : 
  Lapack routine dgesv: system is exactly singular: U[1,1] = 0
Calls: slingshot ... dist.fun -> .dist_clusters_diag -> solve -> solve.default
Execution halted
```

## ERROR CLUSTER 4

 * Number of instances: 3
 * Dataset ids: scaling_0023, scaling_0089, scaling_0425

Last 10 lines of scaling_0023:
```
1: In smooth.spline(lambda, xj, w = w, ..., df = df, tol = tol, keep.data = FALSE) :
  not using invalid df; must have 1 < df <= n := #{unique x} = 4
2: In smooth.spline(lambda, xj, w = w, ..., df = df, tol = tol, keep.data = FALSE) :
  not using invalid df; must have 1 < df <= n := #{unique x} = 4
3: In smooth.spline(lambda, xj, w = w, ..., df = df, tol = tol, keep.data = FALSE) :
  not using invalid df; must have 1 < df <= n := #{unique x} = 4
Error in colMeans(dimred[names(which(cli == grouping)), , drop = T]) : 
  'x' must be an array of at least two dimensions
Calls: t ... sapply -> sapply -> lapply -> FUN -> colMeans -> colMeans
Execution halted
```

## ERROR CLUSTER 5

 * Number of instances: 16
 * Dataset ids: scaling_0042, scaling_0078, scaling_0158, scaling_0165, scaling_0200, scaling_0273, scaling_0306, scaling_0321, scaling_0480, scaling_0501, scaling_0576, scaling_0636, scaling_0667, scaling_0718, scaling_0785, scaling_1011

Last 10 lines of scaling_0042:
```
Warning messages:
1: In rgl.init(initValue, onlyNULL) : RGL: unable to open X11 display
2: 'rgl_init' failed, running with rgl.useNULL = TRUE 
Using diagonal covariance matrix
Curves for Lineage5 and average1 appear to be going in opposite directions. No longer forcing them to share an initial point. To manually override this, set allow.breaks = FALSE.
Curves for Lineage1 and Lineage2 appear to be going in opposite directions. No longer forcing them to share an initial point. To manually override this, set allow.breaks = FALSE.
Error in colMeans(dimred[names(which(cli == grouping)), , drop = T]) : 
  'x' must be an array of at least two dimensions
Calls: t ... sapply -> sapply -> lapply -> FUN -> colMeans -> colMeans
Execution halted
```

## ERROR CLUSTER 6

 * Number of instances: 1
 * Dataset ids: scaling_0063

Last 10 lines of scaling_0063:
```
    flatten
Warning messages:
1: In rgl.init(initValue, onlyNULL) : RGL: unable to open X11 display
2: 'rgl_init' failed, running with rgl.useNULL = TRUE 
Using diagonal covariance matrix
There were 12 warnings (use warnings() to see them)
Error in colMeans(dimred[names(which(cli == grouping)), , drop = T]) : 
  'x' must be an array of at least two dimensions
Calls: t ... sapply -> sapply -> lapply -> FUN -> colMeans -> colMeans
Execution halted
```

## ERROR CLUSTER 7

 * Number of instances: 1
 * Dataset ids: scaling_0073

Last 10 lines of scaling_0073:
```
Warning messages:
1: In rgl.init(initValue, onlyNULL) : RGL: unable to open X11 display
2: 'rgl_init' failed, running with rgl.useNULL = TRUE 
Using diagonal covariance matrix
Curves for Lineage1 and Lineage2 appear to be going in opposite directions. No longer forcing them to share an initial point. To manually override this, set allow.breaks = FALSE.
There were 50 or more warnings (use warnings() to see the first 50)
Error in colMeans(dimred[names(which(cli == grouping)), , drop = T]) : 
  'x' must be an array of at least two dimensions
Calls: t ... sapply -> sapply -> lapply -> FUN -> colMeans -> colMeans
Execution halted
```

## ERROR CLUSTER 8

 * Number of instances: 28
 * Dataset ids: scaling_0087, scaling_0118, scaling_0162, scaling_0194, scaling_0297, scaling_0350, scaling_0360, scaling_0416, scaling_0465, scaling_0564, scaling_0573, scaling_0616, scaling_0632, scaling_0646, scaling_0678, scaling_0697, scaling_0730, scaling_0763, scaling_0783, scaling_0817, scaling_0818, scaling_0852, scaling_0853, scaling_0908, scaling_0909, scaling_0934, scaling_1021, scaling_1048

Last 10 lines of scaling_0087:
```
The following object is masked from 'package:jsonlite':
    flatten
Warning messages:
1: In rgl.init(initValue, onlyNULL) : RGL: unable to open X11 display
2: 'rgl_init' failed, running with rgl.useNULL = TRUE 
Using full covariance matrix
Error in solve.default(s1 + s2) : 
  system is computationally singular: reciprocal condition number = 3.84553e-18
Calls: slingshot ... dist.fun -> .dist_clusters_full -> solve -> solve.default
Execution halted
```

## ERROR CLUSTER 9

 * Number of instances: 8
 * Dataset ids: scaling_0119, scaling_0475, scaling_0581, scaling_0676, scaling_0714, scaling_0949, scaling_0979, scaling_1049

Last 10 lines of scaling_0119:
```
The following object is masked from 'package:jsonlite':
    flatten
Warning messages:
1: In rgl.init(initValue, onlyNULL) : RGL: unable to open X11 display
2: 'rgl_init' failed, running with rgl.useNULL = TRUE 
Using full covariance matrix
Error in solve.default(s1 + s2) : 
  Lapack routine dgesv: system is exactly singular: U[1,1] = 0
Calls: slingshot ... dist.fun -> .dist_clusters_full -> solve -> solve.default
Execution halted
```

## ERROR CLUSTER 10

 * Number of instances: 3
 * Dataset ids: scaling_0121, scaling_0197, scaling_0323

Last 10 lines of scaling_0121:
```
  longer object length is not a multiple of shorter object length
8: In lm(pca$sdev[1:20] ~ x + x2)$residuals^2 * rep(1:2, each = 10) :
  longer object length is not a multiple of shorter object length
9: In lm(pca$sdev[1:20] ~ x + x2)$residuals^2 * rep(1:2, each = 10) :
  longer object length is not a multiple of shorter object length
Using diagonal covariance matrix
Error in smooth.spline(lambda, xj, w = w, ..., df = df, tol = tol, keep.data = FALSE,  : 
  need at least four unique 'x' values
Calls: slingshot ... tryCatchList -> tryCatchOne -> <Anonymous> -> smooth.spline
Execution halted
```

## ERROR CLUSTER 11

 * Number of instances: 1
 * Dataset ids: scaling_0699

Last 10 lines of scaling_0699:
```
  longer object length is not a multiple of shorter object length
9: In lm(pca$sdev[1:20] ~ x + x2)$residuals^2 * rep(1:2, each = 10) :
  longer object length is not a multiple of shorter object length
Using diagonal covariance matrix
Curves for Lineage3 and average1 appear to be going in opposite directions. No longer forcing them to share an initial point. To manually override this, set allow.breaks = FALSE.
Curves for Lineage4 and average2 appear to be going in opposite directions. No longer forcing them to share an initial point. To manually override this, set allow.breaks = FALSE.
Error in if (box.vals[1] == box.vals[5]) { : 
  missing value where TRUE/FALSE needed
Calls: slingshot ... getCurves -> .local -> lapply -> FUN -> .percent_shrinkage
Execution halted
```


