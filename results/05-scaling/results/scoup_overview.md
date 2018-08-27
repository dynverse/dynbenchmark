# scoup
![Overview](scoup.svg)

## ERROR CLUSTER 1

 * Number of instances: 201
 * Dataset ids: scaling_0021, scaling_0023, scaling_0027, scaling_0038, scaling_0046, scaling_0048, scaling_0050, scaling_0062, scaling_0064, scaling_0069, scaling_0073, scaling_0074, scaling_0089, scaling_0091, scaling_0098, scaling_0102, scaling_0103, scaling_0104, scaling_0115, scaling_0125, scaling_0126, scaling_0129, scaling_0130, scaling_0131, scaling_0132, scaling_0135, scaling_0136, scaling_0138, scaling_0146, scaling_0148, scaling_0150, scaling_0171, scaling_0172, scaling_0173, scaling_0177, scaling_0179, scaling_0196, scaling_0197, scaling_0198, scaling_0200, scaling_0203, scaling_0204, scaling_0211, scaling_0214, scaling_0215, scaling_0218, scaling_0220, scaling_0221, scaling_0225, scaling_0239, scaling_0241, scaling_0242, scaling_0247, scaling_0248, scaling_0251, scaling_0254, scaling_0266, scaling_0267, scaling_0268, scaling_0269, scaling_0270, scaling_0272, scaling_0273, scaling_0274, scaling_0299, scaling_0300, scaling_0304, scaling_0305, scaling_0311, scaling_0312, scaling_0313, scaling_0317, scaling_0322, scaling_0323, scaling_0326, scaling_0327, scaling_0329, scaling_0330, scaling_0340, scaling_0364, scaling_0365, scaling_0366, scaling_0367, scaling_0368, scaling_0369, scaling_0375, scaling_0376, scaling_0377, scaling_0378, scaling_0382, scaling_0385, scaling_0387, scaling_0418, scaling_0419, scaling_0420, scaling_0423, scaling_0426, scaling_0427, scaling_0428, scaling_0432, scaling_0435, scaling_0445, scaling_0446, scaling_0447, scaling_0449, scaling_0453, scaling_0455, scaling_0467, scaling_0477, scaling_0478, scaling_0479, scaling_0481, scaling_0482, scaling_0483, scaling_0487, scaling_0491, scaling_0494, scaling_0497, scaling_0502, scaling_0504, scaling_0505, scaling_0515, scaling_0516, scaling_0522, scaling_0538, scaling_0540, scaling_0541, scaling_0542, scaling_0543, scaling_0547, scaling_0548, scaling_0550, scaling_0551, scaling_0552, scaling_0553, scaling_0554, scaling_0566, scaling_0575, scaling_0577, scaling_0578, scaling_0586, scaling_0587, scaling_0590, scaling_0594, scaling_0596, scaling_0598, scaling_0639, scaling_0641, scaling_0645, scaling_0652, scaling_0654, scaling_0661, scaling_0667, scaling_0668, scaling_0671, scaling_0674, scaling_0675, scaling_0719, scaling_0720, scaling_0721, scaling_0722, scaling_0724, scaling_0725, scaling_0726, scaling_0727, scaling_0736, scaling_0737, scaling_0739, scaling_0742, scaling_0746, scaling_0753, scaling_0754, scaling_0756, scaling_0758, scaling_0759, scaling_0760, scaling_0812, scaling_0813, scaling_0814, scaling_0815, scaling_0823, scaling_0827, scaling_0828, scaling_0829, scaling_0830, scaling_0834, scaling_0842, scaling_0845, scaling_0847, scaling_0848, scaling_0882, scaling_0884, scaling_0885, scaling_0892, scaling_0893, scaling_0938, scaling_0939, scaling_0971, scaling_1027, scaling_1029, scaling_1030

Last 10 lines of scaling_0021:
```
The following object is masked from 'package:jsonlite':
    flatten
/SCOUP/sp data init time_sp dimred 10 25 2
/SCOUP/scoup data init time_sp gpara cpara ll 10 25 -k 2 -m 100 -M 100 -a 0.1 -A 100 -t 0.001 -T 2 -s 0.1 -e 0.01
0-th iteration in first EM
log-likelihood: -nan
0-th iteration in second EM
log-likelihood: nan
Error: SCOUP returned NaNs
Execution halted
```

## ERROR CLUSTER 2

 * Number of instances: 5
 * Dataset ids: scaling_0994, scaling_1008, scaling_1022, scaling_1036, scaling_1050

Last 10 lines of scaling_0994:
```
Attaching package: 'purrr'
The following object is masked from 'package:jsonlite':
    flatten
/SCOUP/sp data init time_sp dimred 10 63096 2
Segmentation fault (core dumped)
/SCOUP/scoup data init time_sp gpara cpara ll 10 63096 -k 1 -m 100 -M 100 -a 0.1 -A 100 -t 0.001 -T 2 -s 0.1 -e 0.01
Segmentation fault (core dumped)
Error in `.rowNamesDF<-`(x, value = value) : invalid 'row.names' length
Calls: rownames<- ... row.names<- -> row.names<-.data.frame -> .rowNamesDF<-
Execution halted
```


