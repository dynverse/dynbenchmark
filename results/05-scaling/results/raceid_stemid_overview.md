# raceid_stemid
![Overview](raceid_stemid.svg)

## ERROR CLUSTER 1

 * Number of instances: 242
 * Dataset ids: scaling_0001, scaling_0002, scaling_0003, scaling_0004, scaling_0006, scaling_0008, scaling_0009, scaling_0010, scaling_0012, scaling_0013, scaling_0016, scaling_0017, scaling_0019, scaling_0020, scaling_0021, scaling_0022, scaling_0025, scaling_0026, scaling_0027, scaling_0031, scaling_0033, scaling_0034, scaling_0036, scaling_0037, scaling_0038, scaling_0041, scaling_0043, scaling_0045, scaling_0047, scaling_0051, scaling_0052, scaling_0053, scaling_0056, scaling_0057, scaling_0058, scaling_0061, scaling_0066, scaling_0067, scaling_0068, scaling_0076, scaling_0077, scaling_0078, scaling_0082, scaling_0083, scaling_0084, scaling_0088, scaling_0089, scaling_0094, scaling_0095, scaling_0096, scaling_0106, scaling_0107, scaling_0108, scaling_0113, scaling_0114, scaling_0115, scaling_0120, scaling_0121, scaling_0122, scaling_0127, scaling_0128, scaling_0129, scaling_0141, scaling_0143, scaling_0145, scaling_0147, scaling_0151, scaling_0152, scaling_0157, scaling_0158, scaling_0163, scaling_0164, scaling_0169, scaling_0170, scaling_0181, scaling_0182, scaling_0188, scaling_0189, scaling_0195, scaling_0196, scaling_0202, scaling_0203, scaling_0210, scaling_0216, scaling_0218, scaling_0220, scaling_0222, scaling_0224, scaling_0226, scaling_0227, scaling_0232, scaling_0233, scaling_0238, scaling_0239, scaling_0244, scaling_0245, scaling_0251, scaling_0256, scaling_0260, scaling_0264, scaling_0268, scaling_0272, scaling_0276, scaling_0277, scaling_0278, scaling_0287, scaling_0288, scaling_0289, scaling_0298, scaling_0299, scaling_0300, scaling_0309, scaling_0310, scaling_0311, scaling_0321, scaling_0322, scaling_0331, scaling_0333, scaling_0335, scaling_0337, scaling_0339, scaling_0341, scaling_0342, scaling_0351, scaling_0352, scaling_0361, scaling_0362, scaling_0371, scaling_0372, scaling_0382, scaling_0391, scaling_0392, scaling_0393, scaling_0404, scaling_0405, scaling_0406, scaling_0417, scaling_0418, scaling_0419, scaling_0430, scaling_0431, scaling_0432, scaling_0444, scaling_0445, scaling_0456, scaling_0457, scaling_0466, scaling_0467, scaling_0476, scaling_0477, scaling_0486, scaling_0487, scaling_0497, scaling_0506, scaling_0510, scaling_0514, scaling_0518, scaling_0522, scaling_0526, scaling_0532, scaling_0538, scaling_0544, scaling_0550, scaling_0556, scaling_0557, scaling_0565, scaling_0566, scaling_0574, scaling_0575, scaling_0583, scaling_0584, scaling_0593, scaling_0601, scaling_0602, scaling_0603, scaling_0617, scaling_0618, scaling_0619, scaling_0633, scaling_0634, scaling_0635, scaling_0649, scaling_0650, scaling_0651, scaling_0666, scaling_0667, scaling_0681, scaling_0682, scaling_0683, scaling_0698, scaling_0699, scaling_0700, scaling_0715, scaling_0716, scaling_0717, scaling_0732, scaling_0733, scaling_0734, scaling_0750, scaling_0751, scaling_0766, scaling_0767, scaling_0768, scaling_0784, scaling_0785, scaling_0786, scaling_0802, scaling_0803, scaling_0804, scaling_0820, scaling_0821, scaling_0822, scaling_0839, scaling_0840, scaling_0856, scaling_0857, scaling_0867, scaling_0868, scaling_0878, scaling_0879, scaling_0889, scaling_0890, scaling_0901, scaling_0911, scaling_0919, scaling_0927, scaling_0935, scaling_0943, scaling_0951, scaling_0957, scaling_0963, scaling_0969, scaling_0975, scaling_0981, scaling_0982, scaling_0995, scaling_0996, scaling_1009, scaling_1010, scaling_1023, scaling_1024, scaling_1038

Last 10 lines of scaling_0001:
```
Attaching package: 'purrr'
The following object is masked from 'package:jsonlite':
    flatten
Warning messages:
1: In rgl.init(initValue, onlyNULL) : RGL: unable to open X11 display
2: 'rgl_init' failed, running with rgl.useNULL = TRUE 
Error in cluster::pam(as.dist(sdata), k, diss = TRUE) : 
  Number of clusters 'k' must be in {1,2, .., n-1}; hence n >= 2
Calls: %>% ... <Anonymous> -> clustermethod -> pamk -> <Anonymous>
Execution halted
```

## ERROR CLUSTER 2

 * Number of instances: 322
 * Dataset ids: scaling_0007, scaling_0011, scaling_0015, scaling_0018, scaling_0023, scaling_0024, scaling_0029, scaling_0030, scaling_0032, scaling_0035, scaling_0039, scaling_0040, scaling_0042, scaling_0046, scaling_0050, scaling_0054, scaling_0055, scaling_0062, scaling_0063, scaling_0065, scaling_0072, scaling_0073, scaling_0074, scaling_0075, scaling_0080, scaling_0081, scaling_0090, scaling_0092, scaling_0093, scaling_0101, scaling_0102, scaling_0103, scaling_0104, scaling_0105, scaling_0111, scaling_0112, scaling_0123, scaling_0124, scaling_0126, scaling_0135, scaling_0136, scaling_0137, scaling_0138, scaling_0139, scaling_0140, scaling_0149, scaling_0150, scaling_0155, scaling_0156, scaling_0162, scaling_0165, scaling_0166, scaling_0167, scaling_0168, scaling_0176, scaling_0177, scaling_0179, scaling_0180, scaling_0186, scaling_0187, scaling_0198, scaling_0199, scaling_0200, scaling_0201, scaling_0213, scaling_0214, scaling_0215, scaling_0217, scaling_0221, scaling_0225, scaling_0230, scaling_0231, scaling_0240, scaling_0241, scaling_0242, scaling_0243, scaling_0253, scaling_0254, scaling_0255, scaling_0259, scaling_0266, scaling_0267, scaling_0273, scaling_0274, scaling_0275, scaling_0284, scaling_0285, scaling_0286, scaling_0302, scaling_0303, scaling_0304, scaling_0305, scaling_0306, scaling_0307, scaling_0308, scaling_0323, scaling_0325, scaling_0326, scaling_0327, scaling_0328, scaling_0329, scaling_0330, scaling_0332, scaling_0336, scaling_0340, scaling_0349, scaling_0350, scaling_0360, scaling_0364, scaling_0365, scaling_0366, scaling_0367, scaling_0368, scaling_0369, scaling_0370, scaling_0383, scaling_0385, scaling_0386, scaling_0387, scaling_0388, scaling_0389, scaling_0390, scaling_0400, scaling_0401, scaling_0402, scaling_0403, scaling_0416, scaling_0421, scaling_0422, scaling_0423, scaling_0424, scaling_0425, scaling_0426, scaling_0427, scaling_0428, scaling_0429, scaling_0448, scaling_0449, scaling_0450, scaling_0451, scaling_0452, scaling_0453, scaling_0454, scaling_0455, scaling_0463, scaling_0464, scaling_0465, scaling_0475, scaling_0481, scaling_0482, scaling_0483, scaling_0484, scaling_0485, scaling_0499, scaling_0500, scaling_0501, scaling_0502, scaling_0503, scaling_0504, scaling_0505, scaling_0509, scaling_0515, scaling_0516, scaling_0517, scaling_0523, scaling_0524, scaling_0525, scaling_0530, scaling_0531, scaling_0541, scaling_0542, scaling_0543, scaling_0552, scaling_0553, scaling_0554, scaling_0555, scaling_0562, scaling_0563, scaling_0564, scaling_0573, scaling_0577, scaling_0578, scaling_0579, scaling_0580, scaling_0581, scaling_0582, scaling_0591, scaling_0595, scaling_0596, scaling_0597, scaling_0598, scaling_0599, scaling_0600, scaling_0612, scaling_0613, scaling_0614, scaling_0615, scaling_0616, scaling_0632, scaling_0640, scaling_0641, scaling_0642, scaling_0643, scaling_0644, scaling_0645, scaling_0646, scaling_0647, scaling_0648, scaling_0671, scaling_0672, scaling_0673, scaling_0674, scaling_0675, scaling_0676, scaling_0677, scaling_0678, scaling_0679, scaling_0680, scaling_0693, scaling_0694, scaling_0695, scaling_0696, scaling_0697, scaling_0714, scaling_0723, scaling_0724, scaling_0725, scaling_0726, scaling_0727, scaling_0728, scaling_0729, scaling_0730, scaling_0731, scaling_0756, scaling_0757, scaling_0758, scaling_0759, scaling_0760, scaling_0761, scaling_0762, scaling_0763, scaling_0764, scaling_0765, scaling_0778, scaling_0779, scaling_0780, scaling_0781, scaling_0782, scaling_0783, scaling_0801, scaling_0812, scaling_0813, scaling_0814, scaling_0815, scaling_0816, scaling_0817, scaling_0818, scaling_0819, scaling_0846, scaling_0847, scaling_0848, scaling_0849, scaling_0850, scaling_0851, scaling_0852, scaling_0853, scaling_0854, scaling_0855, scaling_0863, scaling_0864, scaling_0865, scaling_0866, scaling_0877, scaling_0884, scaling_0885, scaling_0886, scaling_0887, scaling_0888, scaling_0899, scaling_0905, scaling_0906, scaling_0907, scaling_0908, scaling_0909, scaling_0910, scaling_0917, scaling_0918, scaling_0931, scaling_0932, scaling_0933, scaling_0934, scaling_0947, scaling_0948, scaling_0949, scaling_0950, scaling_0955, scaling_0956, scaling_0967, scaling_0968, scaling_0978, scaling_0979, scaling_0980, scaling_0990, scaling_0991, scaling_0992, scaling_0993, scaling_0994, scaling_1008, scaling_1016, scaling_1017, scaling_1018, scaling_1019, scaling_1020, scaling_1021, scaling_1022, scaling_1044, scaling_1045, scaling_1046, scaling_1047, scaling_1048, scaling_1049, scaling_1050

Last 10 lines of scaling_0007:
```
Attaching package: 'purrr'
The following object is masked from 'package:jsonlite':
    flatten
Warning messages:
1: In rgl.init(initValue, onlyNULL) : RGL: unable to open X11 display
2: 'rgl_init' failed, running with rgl.useNULL = TRUE 
Error in if (coef(fit)[3] >= 0 | mm >= mthr) { : 
  missing value where TRUE/FALSE needed
Calls: %>% ... withVisible -> <Anonymous> -> filterdata -> fitbackground
Execution halted
```

## ERROR CLUSTER 3

 * Number of instances: 3
 * Dataset ids: scaling_0044, scaling_0110, scaling_0257

Last 10 lines of scaling_0044:
```
subset 45 
subset 46 
subset 47 
subset 48 
subset 49 
subset 50 
find outliers in cluster 1 find outliers in cluster 2 find outliers in cluster 3 find outliers in cluster 4 find outliers in cluster 5 find outliers in cluster 6 Error in apply(fdata, 1, function(x) { : 
  dim(X) must have a positive length
Calls: %>% ... withVisible -> <Anonymous> -> findoutliers -> t -> apply
Execution halted
```

## ERROR CLUSTER 4

 * Number of instances: 90
 * Dataset ids: scaling_0048, scaling_0059, scaling_0060, scaling_0069, scaling_0070, scaling_0079, scaling_0085, scaling_0086, scaling_0097, scaling_0098, scaling_0109, scaling_0116, scaling_0117, scaling_0130, scaling_0131, scaling_0153, scaling_0159, scaling_0160, scaling_0171, scaling_0172, scaling_0184, scaling_0190, scaling_0191, scaling_0204, scaling_0205, scaling_0228, scaling_0234, scaling_0246, scaling_0261, scaling_0269, scaling_0290, scaling_0291, scaling_0312, scaling_0313, scaling_0343, scaling_0353, scaling_0354, scaling_0373, scaling_0374, scaling_0394, scaling_0395, scaling_0407, scaling_0408, scaling_0420, scaling_0433, scaling_0434, scaling_0458, scaling_0459, scaling_0478, scaling_0479, scaling_0488, scaling_0527, scaling_0539, scaling_0545, scaling_0551, scaling_0558, scaling_0576, scaling_0585, scaling_0604, scaling_0605, scaling_0636, scaling_0637, scaling_0653, scaling_0668, scaling_0669, scaling_0684, scaling_0685, scaling_0718, scaling_0719, scaling_0752, scaling_0753, scaling_0769, scaling_0770, scaling_0805, scaling_0806, scaling_0841, scaling_0842, scaling_0858, scaling_0880, scaling_0902, scaling_0912, scaling_0928, scaling_0944, scaling_0952, scaling_0964, scaling_0976, scaling_0983, scaling_1011, scaling_1025, scaling_1039

Last 10 lines of scaling_0048:
```
subset 47 
subset 48 
subset 49 
subset 50 
find outliers in cluster 1 find outliers in cluster 2 find outliers in cluster 3 find outliers in cluster 4 find outliers in cluster 5 find outliers in cluster 6 find outliers in cluster 7 find outliers in cluster 8 find outliers in cluster 9 find outliers in cluster 10 find outliers in cluster 11 find outliers in cluster 12 find outliers in cluster 13 find outliers in cluster 14 find outliers in cluster 15 find outliers in cluster 16 find outliers in cluster 17 find outliers in cluster 18 find outliers in cluster 19 find outliers in cluster 20 find outliers in cluster 21 find outliers in cluster 22 find outliers in cluster 23 find outliers in cluster 24 find outliers in cluster 25 find outliers in cluster 26 find outliers in cluster 27 find outliers in cluster 28 find outliers in cluster 29 find outliers in cluster 30 
determine final clustering partition 1 determine final clustering partition 2 determine final clustering partition 3 determine final clustering partition 4 determine final clustering partition 5 determine final clustering partition 6 determine final clustering partition 7 determine final clustering partition 8 determine final clustering partition 9 determine final clustering partition 10 determine final clustering partition 11 determine final clustering partition 12 determine final clustering partition 13 determine final clustering partition 14 determine final clustering partition 15 determine final clustering partition 16 determine final clustering partition 17 determine final clustering partition 18 determine final clustering partition 19 determine final clustering partition 20 determine final clustering partition 21 determine final clustering partition 22 determine final clustering partition 23 determine final clustering partition 24 determine final clustering partition 25 determine final clustering partition 26 determine final clustering partition 27 determine final clustering partition 28 determine final clustering partition 29 determine final clustering partition 30 
Error in Rtsne.default(X, ..., is_distance = is_distance) : 
  Perplexity is too large.
Calls: %>% ... comptsne -> Rtsne -> Rtsne.dist -> Rtsne -> Rtsne.default
Execution halted
```

## ERROR CLUSTER 5

 * Number of instances: 8
 * Dataset ids: scaling_0064, scaling_0091, scaling_0125, scaling_0146, scaling_0197, scaling_0265, scaling_0301, scaling_0363

Last 10 lines of scaling_0064:
```
subset 45 
subset 46 
subset 47 
subset 48 
subset 49 
subset 50 
find outliers in cluster 1 find outliers in cluster 2 find outliers in cluster 3 Error in apply(fdata, 1, function(x) { : 
  dim(X) must have a positive length
Calls: %>% ... withVisible -> <Anonymous> -> findoutliers -> t -> apply
Execution halted
```

## ERROR CLUSTER 6

 * Number of instances: 1
 * Dataset ids: scaling_0154

Last 10 lines of scaling_0154:
```
subset 45 
subset 46 
subset 47 
subset 48 
subset 49 
subset 50 
find outliers in cluster 1 find outliers in cluster 2 find outliers in cluster 3 find outliers in cluster 4 find outliers in cluster 5 Error in apply(fdata, 1, function(x) { : 
  dim(X) must have a positive length
Calls: %>% ... withVisible -> <Anonymous> -> findoutliers -> t -> apply
Execution halted
```

## ERROR CLUSTER 7

 * Number of instances: 9
 * Dataset ids: scaling_0178, scaling_0211, scaling_0212, scaling_0252, scaling_0324, scaling_0384, scaling_0446, scaling_0447, scaling_0498

Last 10 lines of scaling_0178:
```
subset 45 
subset 46 
subset 47 
subset 48 
subset 49 
subset 50 
find outliers in cluster 1 find outliers in cluster 2 Error in apply(fdata, 1, function(x) { : 
  dim(X) must have a positive length
Calls: %>% ... withVisible -> <Anonymous> -> findoutliers -> t -> apply
Execution halted
```

## ERROR CLUSTER 8

 * Number of instances: 1
 * Dataset ids: scaling_0183

Last 10 lines of scaling_0183:
```
subset 45 
subset 46 
subset 47 
subset 48 
subset 49 
subset 50 
find outliers in cluster 1 find outliers in cluster 2 find outliers in cluster 3 find outliers in cluster 4 find outliers in cluster 5 find outliers in cluster 6 find outliers in cluster 7 Error in apply(fdata, 1, function(x) { : 
  dim(X) must have a positive length
Calls: %>% ... withVisible -> <Anonymous> -> findoutliers -> t -> apply
Execution halted
```

## ERROR CLUSTER 9

 * Number of instances: 1
 * Dataset ids: scaling_0279

Last 10 lines of scaling_0279:
```
subset 45 
subset 46 
subset 47 
subset 48 
subset 49 
subset 50 
find outliers in cluster 1 find outliers in cluster 2 find outliers in cluster 3 find outliers in cluster 4 find outliers in cluster 5 find outliers in cluster 6 find outliers in cluster 7 find outliers in cluster 8 Error in apply(fdata, 1, function(x) { : 
  dim(X) must have a positive length
Calls: %>% ... withVisible -> <Anonymous> -> findoutliers -> t -> apply
Execution halted
```

## ERROR CLUSTER 10

 * Number of instances: 2
 * Dataset ids: scaling_0280, scaling_0891

Last 10 lines of scaling_0280:
```
subset 48 
subset 49 
subset 50 
find outliers in cluster 1 find outliers in cluster 2 find outliers in cluster 3 find outliers in cluster 4 find outliers in cluster 5 find outliers in cluster 6 find outliers in cluster 7 find outliers in cluster 8 find outliers in cluster 9 find outliers in cluster 10 find outliers in cluster 11 find outliers in cluster 12 find outliers in cluster 13 find outliers in cluster 14 find outliers in cluster 15 find outliers in cluster 16 find outliers in cluster 17 find outliers in cluster 18 find outliers in cluster 19 find outliers in cluster 20 find outliers in cluster 21 find outliers in cluster 22 find outliers in cluster 23 find outliers in cluster 24 find outliers in cluster 25 find outliers in cluster 26 find outliers in cluster 27 find outliers in cluster 28 find outliers in cluster 29 find outliers in cluster 30 
merging outliers 1 merging outliers 2 
determine final clustering partition 1 determine final clustering partition 2 determine final clustering partition 3 determine final clustering partition 4 determine final clustering partition 5 determine final clustering partition 6 determine final clustering partition 7 determine final clustering partition 8 determine final clustering partition 9 determine final clustering partition 10 determine final clustering partition 11 determine final clustering partition 12 determine final clustering partition 13 determine final clustering partition 14 determine final clustering partition 15 determine final clustering partition 16 determine final clustering partition 17 determine final clustering partition 18 determine final clustering partition 19 determine final clustering partition 20 determine final clustering partition 21 determine final clustering partition 22 determine final clustering partition 23 determine final clustering partition 24 determine final clustering partition 25 determine final clustering partition 26 determine final clustering partition 27 determine final clustering partition 28 determine final clustering partition 29 determine final clustering partition 30 determine final clustering partition 31 determine final clustering partition 32 
Error in Rtsne.default(X, ..., is_distance = is_distance) : 
  Perplexity is too large.
Calls: %>% ... comptsne -> Rtsne -> Rtsne.dist -> Rtsne -> Rtsne.default
Execution halted
```

## ERROR CLUSTER 11

 * Number of instances: 3
 * Dataset ids: scaling_0344, scaling_0489, scaling_0652

Last 10 lines of scaling_0344:
```
subset 47 
subset 48 
subset 49 
subset 50 
find outliers in cluster 1 find outliers in cluster 2 find outliers in cluster 3 find outliers in cluster 4 find outliers in cluster 5 find outliers in cluster 6 find outliers in cluster 7 find outliers in cluster 8 find outliers in cluster 9 find outliers in cluster 10 find outliers in cluster 11 find outliers in cluster 12 find outliers in cluster 13 find outliers in cluster 14 find outliers in cluster 15 find outliers in cluster 16 find outliers in cluster 17 find outliers in cluster 18 find outliers in cluster 19 find outliers in cluster 20 find outliers in cluster 21 find outliers in cluster 22 find outliers in cluster 23 find outliers in cluster 24 find outliers in cluster 25 find outliers in cluster 26 find outliers in cluster 27 find outliers in cluster 28 find outliers in cluster 29 find outliers in cluster 30 
determine final clustering partition 1 determine final clustering partition 2 determine final clustering partition 3 determine final clustering partition 4 determine final clustering partition 5 determine final clustering partition 6 determine final clustering partition 7 determine final clustering partition 8 determine final clustering partition 9 determine final clustering partition 10 determine final clustering partition 11 determine final clustering partition 12 determine final clustering partition 13 determine final clustering partition 14 determine final clustering partition 15 determine final clustering partition 16 determine final clustering partition 17 determine final clustering partition 18 determine final clustering partition 19 determine final clustering partition 20 determine final clustering partition 21 determine final clustering partition 22 determine final clustering partition 23 determine final clustering partition 24 determine final clustering partition 25 determine final clustering partition 26 determine final clustering partition 27 determine final clustering partition 28 determine final clustering partition 29 determine final clustering partition 30 determine final clustering partition 31 
Error in Rtsne.default(X, ..., is_distance = is_distance) : 
  Perplexity is too large.
Calls: %>% ... comptsne -> Rtsne -> Rtsne.dist -> Rtsne -> Rtsne.default
Execution halted
```

## ERROR CLUSTER 12

 * Number of instances: 3
 * Dataset ids: scaling_0468, scaling_0620, scaling_0736

Last 10 lines of scaling_0468:
```
subset 48 
subset 49 
subset 50 
find outliers in cluster 1 find outliers in cluster 2 find outliers in cluster 3 find outliers in cluster 4 find outliers in cluster 5 find outliers in cluster 6 find outliers in cluster 7 find outliers in cluster 8 find outliers in cluster 9 find outliers in cluster 10 find outliers in cluster 11 find outliers in cluster 12 find outliers in cluster 13 find outliers in cluster 14 find outliers in cluster 15 find outliers in cluster 16 find outliers in cluster 17 find outliers in cluster 18 find outliers in cluster 19 find outliers in cluster 20 find outliers in cluster 21 find outliers in cluster 22 find outliers in cluster 23 find outliers in cluster 24 find outliers in cluster 25 find outliers in cluster 26 find outliers in cluster 27 find outliers in cluster 28 find outliers in cluster 29 find outliers in cluster 30 
merging outliers 1 merging outliers 2 merging outliers 3 
determine final clustering partition 1 determine final clustering partition 2 determine final clustering partition 3 determine final clustering partition 4 determine final clustering partition 5 determine final clustering partition 6 determine final clustering partition 7 determine final clustering partition 8 determine final clustering partition 9 determine final clustering partition 10 determine final clustering partition 11 determine final clustering partition 12 determine final clustering partition 13 determine final clustering partition 14 determine final clustering partition 15 determine final clustering partition 16 determine final clustering partition 17 determine final clustering partition 18 determine final clustering partition 19 determine final clustering partition 20 determine final clustering partition 21 determine final clustering partition 22 determine final clustering partition 23 determine final clustering partition 24 determine final clustering partition 25 determine final clustering partition 26 determine final clustering partition 27 determine final clustering partition 28 determine final clustering partition 29 determine final clustering partition 30 determine final clustering partition 31 determine final clustering partition 32 determine final clustering partition 33 
Error in Rtsne.default(X, ..., is_distance = is_distance) : 
  Perplexity is too large.
Calls: %>% ... comptsne -> Rtsne -> Rtsne.dist -> Rtsne -> Rtsne.default
Execution halted
```

## ERROR CLUSTER 13

 * Number of instances: 3
 * Dataset ids: scaling_0469, scaling_0533, scaling_0936

Last 10 lines of scaling_0469:
```
subset 48 
subset 49 
subset 50 
find outliers in cluster 1 find outliers in cluster 2 find outliers in cluster 3 find outliers in cluster 4 find outliers in cluster 5 find outliers in cluster 6 find outliers in cluster 7 find outliers in cluster 8 find outliers in cluster 9 find outliers in cluster 10 find outliers in cluster 11 find outliers in cluster 12 find outliers in cluster 13 find outliers in cluster 14 find outliers in cluster 15 find outliers in cluster 16 find outliers in cluster 17 find outliers in cluster 18 find outliers in cluster 19 find outliers in cluster 20 find outliers in cluster 21 find outliers in cluster 22 find outliers in cluster 23 find outliers in cluster 24 find outliers in cluster 25 find outliers in cluster 26 find outliers in cluster 27 find outliers in cluster 28 find outliers in cluster 29 find outliers in cluster 30 
merging outliers 1 merging outliers 2 merging outliers 3 merging outliers 4 merging outliers 5 
determine final clustering partition 1 determine final clustering partition 2 determine final clustering partition 3 determine final clustering partition 4 determine final clustering partition 5 determine final clustering partition 6 determine final clustering partition 7 determine final clustering partition 8 determine final clustering partition 9 determine final clustering partition 10 determine final clustering partition 11 determine final clustering partition 12 determine final clustering partition 13 determine final clustering partition 14 determine final clustering partition 15 determine final clustering partition 16 determine final clustering partition 17 determine final clustering partition 18 determine final clustering partition 19 determine final clustering partition 20 determine final clustering partition 21 determine final clustering partition 22 determine final clustering partition 23 determine final clustering partition 24 determine final clustering partition 25 determine final clustering partition 26 determine final clustering partition 27 determine final clustering partition 28 determine final clustering partition 29 determine final clustering partition 30 determine final clustering partition 31 determine final clustering partition 32 determine final clustering partition 33 determine final clustering partition 34 determine final clustering partition 35 
Error in Rtsne.default(X, ..., is_distance = is_distance) : 
  Perplexity is too large.
Calls: %>% ... comptsne -> Rtsne -> Rtsne.dist -> Rtsne -> Rtsne.default
Execution halted
```

## ERROR CLUSTER 14

 * Number of instances: 2
 * Dataset ids: scaling_0567, scaling_0702

Last 10 lines of scaling_0567:
```
subset 48 
subset 49 
subset 50 
find outliers in cluster 1 find outliers in cluster 2 find outliers in cluster 3 find outliers in cluster 4 find outliers in cluster 5 find outliers in cluster 6 find outliers in cluster 7 find outliers in cluster 8 find outliers in cluster 9 find outliers in cluster 10 find outliers in cluster 11 find outliers in cluster 12 find outliers in cluster 13 find outliers in cluster 14 find outliers in cluster 15 find outliers in cluster 16 find outliers in cluster 17 find outliers in cluster 18 find outliers in cluster 19 find outliers in cluster 20 find outliers in cluster 21 find outliers in cluster 22 find outliers in cluster 23 find outliers in cluster 24 find outliers in cluster 25 find outliers in cluster 26 find outliers in cluster 27 find outliers in cluster 28 find outliers in cluster 29 find outliers in cluster 30 
merging outliers 1 merging outliers 2 merging outliers 3 merging outliers 4 merging outliers 5 merging outliers 6 merging outliers 7 merging outliers 8 
determine final clustering partition 1 determine final clustering partition 2 determine final clustering partition 3 determine final clustering partition 4 determine final clustering partition 5 determine final clustering partition 6 determine final clustering partition 7 determine final clustering partition 8 determine final clustering partition 9 determine final clustering partition 10 determine final clustering partition 11 determine final clustering partition 12 determine final clustering partition 13 determine final clustering partition 14 determine final clustering partition 15 determine final clustering partition 16 determine final clustering partition 17 determine final clustering partition 18 determine final clustering partition 19 determine final clustering partition 20 determine final clustering partition 21 determine final clustering partition 22 determine final clustering partition 23 determine final clustering partition 24 determine final clustering partition 25 determine final clustering partition 26 determine final clustering partition 27 determine final clustering partition 28 determine final clustering partition 29 determine final clustering partition 30 determine final clustering partition 31 determine final clustering partition 32 determine final clustering partition 33 determine final clustering partition 34 determine final clustering partition 35 determine final clustering partition 36 determine final clustering partition 37 determine final clustering partition 38 
Error in Rtsne.default(X, ..., is_distance = is_distance) : 
  Perplexity is too large.
Calls: %>% ... comptsne -> Rtsne -> Rtsne.dist -> Rtsne -> Rtsne.default
Execution halted
```

## ERROR CLUSTER 15

 * Number of instances: 1
 * Dataset ids: scaling_0594

Last 10 lines of scaling_0594:
```
subset 45 
subset 46 
subset 47 
subset 48 
subset 49 
subset 50 
find outliers in cluster 1 find outliers in cluster 2 find outliers in cluster 3 find outliers in cluster 4 find outliers in cluster 5 find outliers in cluster 6 find outliers in cluster 7 find outliers in cluster 8 find outliers in cluster 9 find outliers in cluster 10 find outliers in cluster 11 find outliers in cluster 12 find outliers in cluster 13 Error in apply(fdata, 1, function(x) { : 
  dim(X) must have a positive length
Calls: %>% ... withVisible -> <Anonymous> -> findoutliers -> t -> apply
Execution halted
```

## ERROR CLUSTER 16

 * Number of instances: 1
 * Dataset ids: scaling_0621

Last 10 lines of scaling_0621:
```
subset 48 
subset 49 
subset 50 
find outliers in cluster 1 find outliers in cluster 2 find outliers in cluster 3 find outliers in cluster 4 find outliers in cluster 5 find outliers in cluster 6 find outliers in cluster 7 find outliers in cluster 8 find outliers in cluster 9 find outliers in cluster 10 find outliers in cluster 11 find outliers in cluster 12 find outliers in cluster 13 find outliers in cluster 14 find outliers in cluster 15 find outliers in cluster 16 find outliers in cluster 17 find outliers in cluster 18 find outliers in cluster 19 find outliers in cluster 20 find outliers in cluster 21 find outliers in cluster 22 find outliers in cluster 23 find outliers in cluster 24 find outliers in cluster 25 find outliers in cluster 26 find outliers in cluster 27 find outliers in cluster 28 find outliers in cluster 29 find outliers in cluster 30 
merging outliers 1 merging outliers 2 merging outliers 3 merging outliers 4 merging outliers 5 merging outliers 6 merging outliers 7 merging outliers 8 merging outliers 9 
determine final clustering partition 1 determine final clustering partition 2 determine final clustering partition 3 determine final clustering partition 4 determine final clustering partition 5 determine final clustering partition 6 determine final clustering partition 7 determine final clustering partition 8 determine final clustering partition 9 determine final clustering partition 10 determine final clustering partition 11 determine final clustering partition 12 determine final clustering partition 13 determine final clustering partition 14 determine final clustering partition 15 determine final clustering partition 16 determine final clustering partition 17 determine final clustering partition 18 determine final clustering partition 19 determine final clustering partition 20 determine final clustering partition 21 determine final clustering partition 22 determine final clustering partition 23 determine final clustering partition 24 determine final clustering partition 25 determine final clustering partition 26 determine final clustering partition 27 determine final clustering partition 28 determine final clustering partition 29 determine final clustering partition 30 determine final clustering partition 31 determine final clustering partition 32 determine final clustering partition 33 determine final clustering partition 34 determine final clustering partition 35 determine final clustering partition 36 determine final clustering partition 37 determine final clustering partition 38 determine final clustering partition 39 
Error in Rtsne.default(X, ..., is_distance = is_distance) : 
  Perplexity is too large.
Calls: %>% ... comptsne -> Rtsne -> Rtsne.dist -> Rtsne -> Rtsne.default
Execution halted
```

## ERROR CLUSTER 17

 * Number of instances: 2
 * Dataset ids: scaling_0701, scaling_0997

Last 10 lines of scaling_0701:
```
subset 48 
subset 49 
subset 50 
find outliers in cluster 1 find outliers in cluster 2 find outliers in cluster 3 find outliers in cluster 4 find outliers in cluster 5 find outliers in cluster 6 find outliers in cluster 7 find outliers in cluster 8 find outliers in cluster 9 find outliers in cluster 10 find outliers in cluster 11 find outliers in cluster 12 find outliers in cluster 13 find outliers in cluster 14 find outliers in cluster 15 find outliers in cluster 16 find outliers in cluster 17 find outliers in cluster 18 find outliers in cluster 19 find outliers in cluster 20 find outliers in cluster 21 find outliers in cluster 22 find outliers in cluster 23 find outliers in cluster 24 find outliers in cluster 25 find outliers in cluster 26 find outliers in cluster 27 find outliers in cluster 28 find outliers in cluster 29 find outliers in cluster 30 
merging outliers 1 merging outliers 2 merging outliers 3 merging outliers 4 merging outliers 5 merging outliers 6 
determine final clustering partition 1 determine final clustering partition 2 determine final clustering partition 3 determine final clustering partition 4 determine final clustering partition 5 determine final clustering partition 6 determine final clustering partition 7 determine final clustering partition 8 determine final clustering partition 9 determine final clustering partition 10 determine final clustering partition 11 determine final clustering partition 12 determine final clustering partition 13 determine final clustering partition 14 determine final clustering partition 15 determine final clustering partition 16 determine final clustering partition 17 determine final clustering partition 18 determine final clustering partition 19 determine final clustering partition 20 determine final clustering partition 21 determine final clustering partition 22 determine final clustering partition 23 determine final clustering partition 24 determine final clustering partition 25 determine final clustering partition 26 determine final clustering partition 27 determine final clustering partition 28 determine final clustering partition 29 determine final clustering partition 30 determine final clustering partition 31 determine final clustering partition 32 determine final clustering partition 33 determine final clustering partition 34 determine final clustering partition 35 determine final clustering partition 36 
Error in Rtsne.default(X, ..., is_distance = is_distance) : 
  Perplexity is too large.
Calls: %>% ... comptsne -> Rtsne -> Rtsne.dist -> Rtsne -> Rtsne.default
Execution halted
```

## ERROR CLUSTER 18

 * Number of instances: 4
 * Dataset ids: scaling_0735, scaling_0823, scaling_0824, scaling_0970

Last 10 lines of scaling_0735:
```
subset 48 
subset 49 
subset 50 
find outliers in cluster 1 find outliers in cluster 2 find outliers in cluster 3 find outliers in cluster 4 find outliers in cluster 5 find outliers in cluster 6 find outliers in cluster 7 find outliers in cluster 8 find outliers in cluster 9 find outliers in cluster 10 find outliers in cluster 11 find outliers in cluster 12 find outliers in cluster 13 find outliers in cluster 14 find outliers in cluster 15 find outliers in cluster 16 find outliers in cluster 17 find outliers in cluster 18 find outliers in cluster 19 find outliers in cluster 20 find outliers in cluster 21 find outliers in cluster 22 find outliers in cluster 23 find outliers in cluster 24 find outliers in cluster 25 find outliers in cluster 26 find outliers in cluster 27 find outliers in cluster 28 find outliers in cluster 29 find outliers in cluster 30 
merging outliers 1 merging outliers 2 merging outliers 3 merging outliers 4 
determine final clustering partition 1 determine final clustering partition 2 determine final clustering partition 3 determine final clustering partition 4 determine final clustering partition 5 determine final clustering partition 6 determine final clustering partition 7 determine final clustering partition 8 determine final clustering partition 9 determine final clustering partition 10 determine final clustering partition 11 determine final clustering partition 12 determine final clustering partition 13 determine final clustering partition 14 determine final clustering partition 15 determine final clustering partition 16 determine final clustering partition 17 determine final clustering partition 18 determine final clustering partition 19 determine final clustering partition 20 determine final clustering partition 21 determine final clustering partition 22 determine final clustering partition 23 determine final clustering partition 24 determine final clustering partition 25 determine final clustering partition 26 determine final clustering partition 27 determine final clustering partition 28 determine final clustering partition 29 determine final clustering partition 30 determine final clustering partition 31 determine final clustering partition 32 determine final clustering partition 33 determine final clustering partition 34 
Error in Rtsne.default(X, ..., is_distance = is_distance) : 
  Perplexity is too large.
Calls: %>% ... comptsne -> Rtsne -> Rtsne.dist -> Rtsne -> Rtsne.default
Execution halted
```

## ERROR CLUSTER 19

 * Number of instances: 3
 * Dataset ids: scaling_0787, scaling_0958, scaling_0998

Last 10 lines of scaling_0787:
```
subset 48 
subset 49 
subset 50 
find outliers in cluster 1 find outliers in cluster 2 find outliers in cluster 3 find outliers in cluster 4 find outliers in cluster 5 find outliers in cluster 6 find outliers in cluster 7 find outliers in cluster 8 find outliers in cluster 9 find outliers in cluster 10 find outliers in cluster 11 find outliers in cluster 12 find outliers in cluster 13 find outliers in cluster 14 find outliers in cluster 15 find outliers in cluster 16 find outliers in cluster 17 find outliers in cluster 18 find outliers in cluster 19 find outliers in cluster 20 find outliers in cluster 21 find outliers in cluster 22 find outliers in cluster 23 find outliers in cluster 24 find outliers in cluster 25 find outliers in cluster 26 find outliers in cluster 27 find outliers in cluster 28 find outliers in cluster 29 find outliers in cluster 30 
merging outliers 1 Error in if (sum(f) > 1) ind <- (1:sum(f))[apply(m[f, f] <= quantile(clp2p.cl,  : 
  missing value where TRUE/FALSE needed
Calls: %>% ... freduce -> withVisible -> <Anonymous> -> findoutliers
In addition: Warning message:
In min(clp2p.cl) : no non-missing arguments to min; returning Inf
Execution halted
```

## ERROR CLUSTER 20

 * Number of instances: 1
 * Dataset ids: scaling_0788

Last 10 lines of scaling_0788:
```
subset 48 
subset 49 
subset 50 
find outliers in cluster 1 find outliers in cluster 2 find outliers in cluster 3 find outliers in cluster 4 find outliers in cluster 5 find outliers in cluster 6 find outliers in cluster 7 find outliers in cluster 8 find outliers in cluster 9 find outliers in cluster 10 find outliers in cluster 11 find outliers in cluster 12 find outliers in cluster 13 find outliers in cluster 14 find outliers in cluster 15 find outliers in cluster 16 find outliers in cluster 17 find outliers in cluster 18 find outliers in cluster 19 find outliers in cluster 20 find outliers in cluster 21 find outliers in cluster 22 find outliers in cluster 23 find outliers in cluster 24 find outliers in cluster 25 find outliers in cluster 26 find outliers in cluster 27 find outliers in cluster 28 find outliers in cluster 29 find outliers in cluster 30 
merging outliers 1 merging outliers 2 merging outliers 3 merging outliers 4 merging outliers 5 merging outliers 6 merging outliers 7 merging outliers 8 merging outliers 9 merging outliers 10 merging outliers 11 merging outliers 12 merging outliers 13 merging outliers 14 merging outliers 15 merging outliers 16 merging outliers 17 merging outliers 18 merging outliers 19 merging outliers 20 merging outliers 21 merging outliers 22 merging outliers 23 merging outliers 24 merging outliers 25 merging outliers 26 merging outliers 27 merging outliers 28 merging outliers 29 merging outliers 30 merging outliers 31 merging outliers 32 merging outliers 33 merging outliers 34 merging outliers 35 merging outliers 36 merging outliers 37 merging outliers 38 merging outliers 39 merging outliers 40 
determine final clustering partition 1 determine final clustering partition 2 determine final clustering partition 3 determine final clustering partition 4 determine final clustering partition 5 determine final clustering partition 6 determine final clustering partition 7 determine final clustering partition 8 determine final clustering partition 9 determine final clustering partition 10 determine final clustering partition 11 determine final clustering partition 12 determine final clustering partition 13 determine final clustering partition 14 determine final clustering partition 15 determine final clustering partition 16 determine final clustering partition 17 determine final clustering partition 18 determine final clustering partition 19 determine final clustering partition 20 determine final clustering partition 21 determine final clustering partition 22 determine final clustering partition 23 determine final clustering partition 24 determine final clustering partition 25 determine final clustering partition 26 determine final clustering partition 27 determine final clustering partition 28 determine final clustering partition 29 determine final clustering partition 30 determine final clustering partition 31 determine final clustering partition 32 determine final clustering partition 33 determine final clustering partition 34 determine final clustering partition 35 determine final clustering partition 36 determine final clustering partition 37 determine final clustering partition 38 determine final clustering partition 39 determine final clustering partition 40 determine final clustering partition 41 determine final clustering partition 42 determine final clustering partition 43 determine final clustering partition 44 determine final clustering partition 45 determine final clustering partition 46 determine final clustering partition 47 determine final clustering partition 48 determine final clustering partition 49 determine final clustering partition 50 determine final clustering partition 51 determine final clustering partition 52 determine final clustering partition 53 determine final clustering partition 54 determine final clustering partition 55 determine final clustering partition 56 determine final clustering partition 57 determine final clustering partition 58 determine final clustering partition 59 determine final clustering partition 60 determine final clustering partition 61 determine final clustering partition 62 determine final clustering partition 63 determine final clustering partition 64 determine final clustering partition 65 determine final clustering partition 66 determine final clustering partition 67 determine final clustering partition 68 determine final clustering partition 69 determine final clustering partition 70 
Error in Rtsne.default(X, ..., is_distance = is_distance) : 
  Perplexity is too large.
Calls: %>% ... comptsne -> Rtsne -> Rtsne.dist -> Rtsne -> Rtsne.default
Execution halted
```

## ERROR CLUSTER 21

 * Number of instances: 1
 * Dataset ids: scaling_0811

Last 10 lines of scaling_0811:
```
subset 45 
subset 46 
subset 47 
subset 48 
subset 49 
subset 50 
find outliers in cluster 1 find outliers in cluster 2 find outliers in cluster 3 find outliers in cluster 4 find outliers in cluster 5 find outliers in cluster 6 find outliers in cluster 7 find outliers in cluster 8 find outliers in cluster 9 find outliers in cluster 10 find outliers in cluster 11 find outliers in cluster 12 find outliers in cluster 13 find outliers in cluster 14 find outliers in cluster 15 find outliers in cluster 16 find outliers in cluster 17 Error in apply(fdata, 1, function(x) { : 
  dim(X) must have a positive length
Calls: %>% ... withVisible -> <Anonymous> -> findoutliers -> t -> apply
Execution halted
```

## ERROR CLUSTER 22

 * Number of instances: 1
 * Dataset ids: scaling_0869

Last 10 lines of scaling_0869:
```
subset 48 
subset 49 
subset 50 
find outliers in cluster 1 find outliers in cluster 2 find outliers in cluster 3 find outliers in cluster 4 find outliers in cluster 5 find outliers in cluster 6 find outliers in cluster 7 find outliers in cluster 8 find outliers in cluster 9 find outliers in cluster 10 find outliers in cluster 11 find outliers in cluster 12 find outliers in cluster 13 find outliers in cluster 14 find outliers in cluster 15 find outliers in cluster 16 find outliers in cluster 17 find outliers in cluster 18 find outliers in cluster 19 find outliers in cluster 20 find outliers in cluster 21 find outliers in cluster 22 find outliers in cluster 23 find outliers in cluster 24 find outliers in cluster 25 find outliers in cluster 26 find outliers in cluster 27 find outliers in cluster 28 find outliers in cluster 29 find outliers in cluster 30 
merging outliers 1 merging outliers 2 merging outliers 3 merging outliers 4 merging outliers 5 merging outliers 6 merging outliers 7 merging outliers 8 merging outliers 9 merging outliers 10 merging outliers 11 
determine final clustering partition 1 determine final clustering partition 2 determine final clustering partition 3 determine final clustering partition 4 determine final clustering partition 5 determine final clustering partition 6 determine final clustering partition 7 determine final clustering partition 8 determine final clustering partition 9 determine final clustering partition 10 determine final clustering partition 11 determine final clustering partition 12 determine final clustering partition 13 determine final clustering partition 14 determine final clustering partition 15 determine final clustering partition 16 determine final clustering partition 17 determine final clustering partition 18 determine final clustering partition 19 determine final clustering partition 20 determine final clustering partition 21 determine final clustering partition 22 determine final clustering partition 23 determine final clustering partition 24 determine final clustering partition 25 determine final clustering partition 26 determine final clustering partition 27 determine final clustering partition 28 determine final clustering partition 29 determine final clustering partition 30 determine final clustering partition 31 determine final clustering partition 32 determine final clustering partition 33 determine final clustering partition 34 determine final clustering partition 35 determine final clustering partition 36 determine final clustering partition 37 determine final clustering partition 38 determine final clustering partition 39 determine final clustering partition 40 determine final clustering partition 41 
Error in Rtsne.default(X, ..., is_distance = is_distance) : 
  Perplexity is too large.
Calls: %>% ... comptsne -> Rtsne -> Rtsne.dist -> Rtsne -> Rtsne.default
Execution halted
```

## ERROR CLUSTER 23

 * Number of instances: 1
 * Dataset ids: scaling_0870

Last 10 lines of scaling_0870:
```
subset 48 
subset 49 
subset 50 
find outliers in cluster 1 find outliers in cluster 2 find outliers in cluster 3 find outliers in cluster 4 find outliers in cluster 5 find outliers in cluster 6 find outliers in cluster 7 find outliers in cluster 8 find outliers in cluster 9 find outliers in cluster 10 find outliers in cluster 11 find outliers in cluster 12 find outliers in cluster 13 find outliers in cluster 14 find outliers in cluster 15 find outliers in cluster 16 find outliers in cluster 17 find outliers in cluster 18 find outliers in cluster 19 find outliers in cluster 20 find outliers in cluster 21 find outliers in cluster 22 find outliers in cluster 23 find outliers in cluster 24 find outliers in cluster 25 find outliers in cluster 26 find outliers in cluster 27 find outliers in cluster 28 find outliers in cluster 29 find outliers in cluster 30 
merging outliers 1 merging outliers 2 merging outliers 3 merging outliers 4 merging outliers 5 merging outliers 6 merging outliers 7 merging outliers 8 merging outliers 9 merging outliers 10 merging outliers 11 merging outliers 12 merging outliers 13 merging outliers 14 merging outliers 15 merging outliers 16 merging outliers 17 merging outliers 18 merging outliers 19 merging outliers 20 merging outliers 21 merging outliers 22 merging outliers 23 merging outliers 24 merging outliers 25 merging outliers 26 merging outliers 27 merging outliers 28 merging outliers 29 merging outliers 30 merging outliers 31 merging outliers 32 merging outliers 33 merging outliers 34 merging outliers 35 merging outliers 36 merging outliers 37 merging outliers 38 merging outliers 39 merging outliers 40 merging outliers 41 merging outliers 42 merging outliers 43 merging outliers 44 merging outliers 45 merging outliers 46 merging outliers 47 merging outliers 48 merging outliers 49 merging outliers 50 merging outliers 51 merging outliers 52 
determine final clustering partition 1 determine final clustering partition 2 determine final clustering partition 3 determine final clustering partition 4 determine final clustering partition 5 determine final clustering partition 6 determine final clustering partition 7 determine final clustering partition 8 determine final clustering partition 9 determine final clustering partition 10 determine final clustering partition 11 determine final clustering partition 12 determine final clustering partition 13 determine final clustering partition 14 determine final clustering partition 15 determine final clustering partition 16 determine final clustering partition 17 determine final clustering partition 18 determine final clustering partition 19 determine final clustering partition 20 determine final clustering partition 21 determine final clustering partition 22 determine final clustering partition 23 determine final clustering partition 24 determine final clustering partition 25 determine final clustering partition 26 determine final clustering partition 27 determine final clustering partition 28 determine final clustering partition 29 determine final clustering partition 30 determine final clustering partition 31 determine final clustering partition 32 determine final clustering partition 33 determine final clustering partition 34 determine final clustering partition 35 determine final clustering partition 36 determine final clustering partition 37 determine final clustering partition 38 determine final clustering partition 39 determine final clustering partition 40 determine final clustering partition 41 determine final clustering partition 42 determine final clustering partition 43 determine final clustering partition 44 determine final clustering partition 45 determine final clustering partition 46 determine final clustering partition 47 determine final clustering partition 48 determine final clustering partition 49 determine final clustering partition 50 determine final clustering partition 51 determine final clustering partition 52 determine final clustering partition 53 determine final clustering partition 54 determine final clustering partition 55 determine final clustering partition 56 determine final clustering partition 57 determine final clustering partition 58 determine final clustering partition 59 determine final clustering partition 60 determine final clustering partition 61 determine final clustering partition 62 determine final clustering partition 63 determine final clustering partition 64 determine final clustering partition 65 determine final clustering partition 66 determine final clustering partition 67 determine final clustering partition 68 determine final clustering partition 69 determine final clustering partition 70 determine final clustering partition 71 determine final clustering partition 72 determine final clustering partition 73 determine final clustering partition 74 determine final clustering partition 75 determine final clustering partition 76 determine final clustering partition 77 determine final clustering partition 78 determine final clustering partition 79 determine final clustering partition 80 determine final clustering partition 81 determine final clustering partition 82 
Error in aggregate.data.frame(pdil, by = list(lp), median) : 
  no rows to aggregate
Calls: %>% ... <Anonymous> -> projcells -> aggregate -> aggregate.data.frame
Execution halted
```

## ERROR CLUSTER 24

 * Number of instances: 1
 * Dataset ids: scaling_0920

Last 10 lines of scaling_0920:
```
subset 48 
subset 49 
subset 50 
find outliers in cluster 1 find outliers in cluster 2 find outliers in cluster 3 find outliers in cluster 4 find outliers in cluster 5 find outliers in cluster 6 find outliers in cluster 7 find outliers in cluster 8 find outliers in cluster 9 find outliers in cluster 10 find outliers in cluster 11 find outliers in cluster 12 find outliers in cluster 13 find outliers in cluster 14 find outliers in cluster 15 find outliers in cluster 16 find outliers in cluster 17 find outliers in cluster 18 find outliers in cluster 19 find outliers in cluster 20 find outliers in cluster 21 find outliers in cluster 22 find outliers in cluster 23 find outliers in cluster 24 find outliers in cluster 25 find outliers in cluster 26 find outliers in cluster 27 find outliers in cluster 28 find outliers in cluster 29 find outliers in cluster 30 
merging outliers 1 merging outliers 2 merging outliers 3 merging outliers 4 merging outliers 5 merging outliers 6 merging outliers 7 merging outliers 8 merging outliers 9 merging outliers 10 merging outliers 11 merging outliers 12 merging outliers 13 merging outliers 14 merging outliers 15 merging outliers 16 merging outliers 17 merging outliers 18 merging outliers 19 merging outliers 20 merging outliers 21 merging outliers 22 merging outliers 23 merging outliers 24 merging outliers 25 merging outliers 26 merging outliers 27 merging outliers 28 merging outliers 29 merging outliers 30 merging outliers 31 merging outliers 32 merging outliers 33 merging outliers 34 merging outliers 35 merging outliers 36 merging outliers 37 merging outliers 38 merging outliers 39 merging outliers 40 merging outliers 41 merging outliers 42 merging outliers 43 merging outliers 44 
determine final clustering partition 1 determine final clustering partition 2 determine final clustering partition 3 determine final clustering partition 4 determine final clustering partition 5 determine final clustering partition 6 determine final clustering partition 7 determine final clustering partition 8 determine final clustering partition 9 determine final clustering partition 10 determine final clustering partition 11 determine final clustering partition 12 determine final clustering partition 13 determine final clustering partition 14 determine final clustering partition 15 determine final clustering partition 16 determine final clustering partition 17 determine final clustering partition 18 determine final clustering partition 19 determine final clustering partition 20 determine final clustering partition 21 determine final clustering partition 22 determine final clustering partition 23 determine final clustering partition 24 determine final clustering partition 25 determine final clustering partition 26 determine final clustering partition 27 determine final clustering partition 28 determine final clustering partition 29 determine final clustering partition 30 determine final clustering partition 31 determine final clustering partition 32 determine final clustering partition 33 determine final clustering partition 34 determine final clustering partition 35 determine final clustering partition 36 determine final clustering partition 37 determine final clustering partition 38 determine final clustering partition 39 determine final clustering partition 40 determine final clustering partition 41 determine final clustering partition 42 determine final clustering partition 43 determine final clustering partition 44 determine final clustering partition 45 determine final clustering partition 46 determine final clustering partition 47 determine final clustering partition 48 determine final clustering partition 49 determine final clustering partition 50 determine final clustering partition 51 determine final clustering partition 52 determine final clustering partition 53 determine final clustering partition 54 determine final clustering partition 55 determine final clustering partition 56 determine final clustering partition 57 determine final clustering partition 58 determine final clustering partition 59 determine final clustering partition 60 determine final clustering partition 61 determine final clustering partition 62 determine final clustering partition 63 determine final clustering partition 64 determine final clustering partition 65 determine final clustering partition 66 determine final clustering partition 67 determine final clustering partition 68 determine final clustering partition 69 determine final clustering partition 70 determine final clustering partition 71 determine final clustering partition 72 determine final clustering partition 73 determine final clustering partition 74 
Error in Rtsne.default(X, ..., is_distance = is_distance) : 
  Perplexity is too large.
Calls: %>% ... comptsne -> Rtsne -> Rtsne.dist -> Rtsne -> Rtsne.default
Execution halted
```


