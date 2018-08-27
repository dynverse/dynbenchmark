# pcreode
![Overview](pcreode.svg)

## ERROR CLUSTER 1

 * Number of instances: 268
 * Dataset ids: scaling_0001, scaling_0002, scaling_0003, scaling_0004, scaling_0006, scaling_0007, scaling_0008, scaling_0009, scaling_0010, scaling_0011, scaling_0012, scaling_0013, scaling_0015, scaling_0016, scaling_0017, scaling_0018, scaling_0019, scaling_0020, scaling_0021, scaling_0022, scaling_0023, scaling_0024, scaling_0025, scaling_0026, scaling_0027, scaling_0029, scaling_0030, scaling_0031, scaling_0032, scaling_0033, scaling_0034, scaling_0035, scaling_0036, scaling_0037, scaling_0039, scaling_0040, scaling_0041, scaling_0043, scaling_0045, scaling_0047, scaling_0051, scaling_0052, scaling_0053, scaling_0056, scaling_0057, scaling_0058, scaling_0061, scaling_0062, scaling_0063, scaling_0066, scaling_0067, scaling_0068, scaling_0072, scaling_0073, scaling_0076, scaling_0077, scaling_0078, scaling_0082, scaling_0083, scaling_0084, scaling_0088, scaling_0089, scaling_0090, scaling_0094, scaling_0095, scaling_0096, scaling_0101, scaling_0102, scaling_0106, scaling_0107, scaling_0108, scaling_0113, scaling_0114, scaling_0115, scaling_0120, scaling_0121, scaling_0122, scaling_0127, scaling_0128, scaling_0129, scaling_0135, scaling_0136, scaling_0141, scaling_0143, scaling_0145, scaling_0147, scaling_0149, scaling_0151, scaling_0152, scaling_0157, scaling_0158, scaling_0163, scaling_0164, scaling_0169, scaling_0170, scaling_0176, scaling_0181, scaling_0182, scaling_0188, scaling_0189, scaling_0195, scaling_0196, scaling_0202, scaling_0210, scaling_0216, scaling_0218, scaling_0220, scaling_0222, scaling_0224, scaling_0226, scaling_0227, scaling_0232, scaling_0233, scaling_0238, scaling_0239, scaling_0244, scaling_0245, scaling_0251, scaling_0256, scaling_0260, scaling_0264, scaling_0268, scaling_0272, scaling_0276, scaling_0277, scaling_0278, scaling_0287, scaling_0288, scaling_0289, scaling_0298, scaling_0299, scaling_0300, scaling_0309, scaling_0310, scaling_0311, scaling_0321, scaling_0322, scaling_0331, scaling_0333, scaling_0335, scaling_0337, scaling_0339, scaling_0341, scaling_0342, scaling_0351, scaling_0352, scaling_0361, scaling_0362, scaling_0371, scaling_0372, scaling_0382, scaling_0383, scaling_0391, scaling_0392, scaling_0393, scaling_0404, scaling_0405, scaling_0406, scaling_0418, scaling_0419, scaling_0430, scaling_0431, scaling_0432, scaling_0444, scaling_0445, scaling_0446, scaling_0456, scaling_0457, scaling_0466, scaling_0467, scaling_0476, scaling_0477, scaling_0486, scaling_0487, scaling_0497, scaling_0498, scaling_0506, scaling_0510, scaling_0514, scaling_0518, scaling_0522, scaling_0526, scaling_0532, scaling_0538, scaling_0544, scaling_0550, scaling_0556, scaling_0557, scaling_0565, scaling_0566, scaling_0574, scaling_0575, scaling_0583, scaling_0584, scaling_0593, scaling_0594, scaling_0601, scaling_0602, scaling_0603, scaling_0617, scaling_0618, scaling_0619, scaling_0633, scaling_0634, scaling_0635, scaling_0649, scaling_0650, scaling_0666, scaling_0667, scaling_0668, scaling_0682, scaling_0683, scaling_0698, scaling_0699, scaling_0700, scaling_0715, scaling_0716, scaling_0717, scaling_0732, scaling_0733, scaling_0734, scaling_0750, scaling_0751, scaling_0752, scaling_0766, scaling_0767, scaling_0768, scaling_0784, scaling_0785, scaling_0786, scaling_0802, scaling_0803, scaling_0804, scaling_0820, scaling_0821, scaling_0822, scaling_0839, scaling_0840, scaling_0841, scaling_0856, scaling_0857, scaling_0867, scaling_0868, scaling_0878, scaling_0879, scaling_0889, scaling_0890, scaling_0901, scaling_0902, scaling_0911, scaling_0919, scaling_0927, scaling_0935, scaling_0943, scaling_0951, scaling_0957, scaling_0963, scaling_0969, scaling_0975, scaling_0981, scaling_0982, scaling_0995, scaling_0996, scaling_1010, scaling_1023, scaling_1024, scaling_1038, scaling_1039

Last 10 lines of scaling_0001:
```
    mute = True
  File "/usr/local/lib/python2.7/site-packages/pcreode/functions.py", line 642, in pCreode
    sil_score[ss] = metrics.silhouette_score( low_cls, labels=label, metric='l2')
  File "/usr/local/lib/python2.7/site-packages/sklearn/metrics/cluster/unsupervised.py", line 101, in silhouette_score
    return np.mean(silhouette_samples(X, labels, metric=metric, **kwds))
  File "/usr/local/lib/python2.7/site-packages/sklearn/metrics/cluster/unsupervised.py", line 167, in silhouette_samples
    check_number_of_labels(len(le.classes_), X.shape[0])
  File "/usr/local/lib/python2.7/site-packages/sklearn/metrics/cluster/unsupervised.py", line 19, in check_number_of_labels
    "to n_samples - 1 (inclusive)" % n_labels)
ValueError: Number of labels is 5. Valid values are 2 to n_samples - 1 (inclusive)
```

## ERROR CLUSTER 2

 * Number of instances: 3
 * Dataset ids: scaling_0038, scaling_0203, scaling_0651

Last 10 lines of scaling_0038:
```
  /group/irc/shared/dynverse/dynbenchmark/derived/03-method_characterisation/singularity_images/dynverse/pcreode.simg \
  /code/run.sh
Traceback (most recent call last):
  File "/code/run.sh", line 45, in <module>
    mute = True
  File "/usr/local/lib/python2.7/site-packages/pcreode/functions.py", line 701, in pCreode
    aligned_ind = consensus_alignment( down, hi_pl_ind.copy(), data, density, noise)
  File "/usr/local/lib/python2.7/site-packages/pcreode/functions.py", line 391, in consensus_alignment
    new_nodes[jj] = np.median( no_noise[chunks[ss][node_ind==hi_pl_ind[jj]]], axis=0)
IndexError: index 13 is out of bounds for axis 0 with size 13
```

## ERROR CLUSTER 3

 * Number of instances: 1
 * Dataset ids: scaling_0360

Last 10 lines of scaling_0360:
```
Traceback (most recent call last):
  File "/code/run.sh", line 45, in <module>
    mute = True
  File "/usr/local/lib/python2.7/site-packages/pcreode/functions.py", line 698, in pCreode
    hi_pl, hi_pl_ind = hierarchical_placement( dens_knn, cen_ind)
  File "/usr/local/lib/python2.7/site-packages/pcreode/functions.py", line 284, in hierarchical_placement
    run_dist = get_graph_distance( end_ind, to_ind, graph)
  File "/usr/local/lib/python2.7/site-packages/pcreode/functions.py", line 120, in get_graph_distance
    d[ii,:] = graph.shortest_paths( from_ind[ii], to_ind, weights="weight")[0]
igraph._igraph.InternalError: Error at structural_properties.c:5200: cannot run Bellman-Ford algorithm, Negative loop detected while calculating shortest paths
```

## ERROR CLUSTER 4

 * Number of instances: 3
 * Dataset ids: scaling_0417, scaling_0681, scaling_1009

Last 10 lines of scaling_0417:
```
Traceback (most recent call last):
  File "/code/run.sh", line 45, in <module>
    mute = True
  File "/usr/local/lib/python2.7/site-packages/pcreode/functions.py", line 640, in pCreode
    kmeans_model  = _KMeans( n_clusters=ss+2, random_state=10).fit( low_cls)
  File "/usr/local/lib/python2.7/site-packages/sklearn/cluster/k_means_.py", line 887, in fit
    X = self._check_fit_data(X)
  File "/usr/local/lib/python2.7/site-packages/sklearn/cluster/k_means_.py", line 861, in _check_fit_data
    X.shape[0], self.n_clusters))
ValueError: n_samples=1 should be >= n_clusters=2
```

## ERROR CLUSTER 5

 * Number of instances: 10
 * Dataset ids: scaling_0663, scaling_0679, scaling_0746, scaling_0798, scaling_0834, scaling_0925, scaling_0941, scaling_0949, scaling_1005, scaling_1033

Last 10 lines of scaling_0663:
```
Input saved to /data/tmp//Rtmpg2jmxh/file1082e31bd276d/ti/input: 
	expression.csv
	params.json
Running singularity exec --pwd /ti/workspace -B \
  '/data/tmp//Rtmpg2jmxh/file1082e31bd276d/ti:/ti,/data/tmp//Rtmpg2jmxh/file1082e53adf82a/tmp:/tmp2' \
  /group/irc/shared/dynverse/dynbenchmark/derived/03-method_characterisation/singularity_images/dynverse/pcreode.simg \
  /code/run.sh
```


