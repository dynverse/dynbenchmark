# forks
![Overview](forks.svg)

## ERROR CLUSTER 1

 * Number of instances: 38
 * Dataset ids: scaling_0001, scaling_0002, scaling_0003, scaling_0004, scaling_0006, scaling_0010, scaling_0012, scaling_0016, scaling_0022, scaling_0025, scaling_0041, scaling_0043, scaling_0045, scaling_0047, scaling_0226, scaling_0244, scaling_0276, scaling_0309, scaling_0341, scaling_0361, scaling_0391, scaling_0417, scaling_0444, scaling_0446, scaling_0456, scaling_0476, scaling_0556, scaling_0574, scaling_0601, scaling_0633, scaling_0681, scaling_0715, scaling_0766, scaling_0802, scaling_0856, scaling_0878, scaling_0981, scaling_1009

Last 10 lines of scaling_0001:
```
    self._fit_transform(X)
  File "/usr/local/lib/python3.6/site-packages/sklearn/manifold/isomap.py", line 116, in _fit_transform
    mode='distance', n_jobs=self.n_jobs)
  File "/usr/local/lib/python3.6/site-packages/sklearn/neighbors/graph.py", line 103, in kneighbors_graph
    return X.kneighbors_graph(X=query, n_neighbors=n_neighbors, mode=mode)
  File "/usr/local/lib/python3.6/site-packages/sklearn/neighbors/base.py", line 489, in kneighbors_graph
    X, n_neighbors, return_distance=True)
  File "/usr/local/lib/python3.6/site-packages/sklearn/neighbors/base.py", line 347, in kneighbors
    (train_size, n_neighbors)
ValueError: Expected n_neighbors <= n_samples,  but n_samples = 10, n_neighbors = 11
```

## ERROR CLUSTER 2

 * Number of instances: 203
 * Dataset ids: scaling_0008, scaling_0011, scaling_0015, scaling_0019, scaling_0020, scaling_0030, scaling_0033, scaling_0034, scaling_0035, scaling_0039, scaling_0040, scaling_0046, scaling_0050, scaling_0054, scaling_0055, scaling_0058, scaling_0064, scaling_0074, scaling_0075, scaling_0076, scaling_0079, scaling_0081, scaling_0091, scaling_0094, scaling_0102, scaling_0104, scaling_0105, scaling_0107, scaling_0111, scaling_0112, scaling_0113, scaling_0124, scaling_0126, scaling_0137, scaling_0139, scaling_0142, scaling_0146, scaling_0150, scaling_0152, scaling_0155, scaling_0156, scaling_0158, scaling_0167, scaling_0178, scaling_0186, scaling_0187, scaling_0199, scaling_0200, scaling_0213, scaling_0217, scaling_0225, scaling_0230, scaling_0231, scaling_0251, scaling_0255, scaling_0259, scaling_0264, scaling_0266, scaling_0267, scaling_0275, scaling_0284, scaling_0285, scaling_0286, scaling_0301, scaling_0305, scaling_0314, scaling_0323, scaling_0327, scaling_0329, scaling_0330, scaling_0332, scaling_0339, scaling_0340, scaling_0349, scaling_0350, scaling_0364, scaling_0370, scaling_0384, scaling_0389, scaling_0390, scaling_0400, scaling_0401, scaling_0402, scaling_0403, scaling_0415, scaling_0422, scaling_0423, scaling_0428, scaling_0448, scaling_0452, scaling_0453, scaling_0454, scaling_0455, scaling_0462, scaling_0464, scaling_0465, scaling_0474, scaling_0483, scaling_0484, scaling_0485, scaling_0500, scaling_0502, scaling_0503, scaling_0504, scaling_0505, scaling_0509, scaling_0513, scaling_0516, scaling_0523, scaling_0525, scaling_0529, scaling_0531, scaling_0537, scaling_0541, scaling_0542, scaling_0543, scaling_0553, scaling_0554, scaling_0555, scaling_0563, scaling_0564, scaling_0572, scaling_0579, scaling_0580, scaling_0582, scaling_0595, scaling_0596, scaling_0599, scaling_0600, scaling_0613, scaling_0614, scaling_0615, scaling_0616, scaling_0630, scaling_0631, scaling_0641, scaling_0643, scaling_0644, scaling_0646, scaling_0648, scaling_0672, scaling_0673, scaling_0675, scaling_0677, scaling_0678, scaling_0679, scaling_0680, scaling_0693, scaling_0694, scaling_0695, scaling_0696, scaling_0697, scaling_0712, scaling_0713, scaling_0726, scaling_0728, scaling_0730, scaling_0731, scaling_0762, scaling_0763, scaling_0765, scaling_0778, scaling_0779, scaling_0781, scaling_0782, scaling_0783, scaling_0799, scaling_0800, scaling_0817, scaling_0818, scaling_0819, scaling_0852, scaling_0861, scaling_0863, scaling_0864, scaling_0865, scaling_0866, scaling_0875, scaling_0876, scaling_0887, scaling_0888, scaling_0910, scaling_0916, scaling_0918, scaling_0926, scaling_0949, scaling_0954, scaling_0955, scaling_0956, scaling_0961, scaling_0962, scaling_0980, scaling_0990, scaling_0992, scaling_0993, scaling_0994, scaling_1006, scaling_1007, scaling_1008, scaling_1020, scaling_1021, scaling_1022, scaling_1050

Last 10 lines of scaling_0008:
```
    cluster_labels = clusterer.fit_predict(X)
  File "/usr/local/lib/python3.6/site-packages/sklearn/cluster/k_means_.py", line 917, in fit_predict
    return self.fit(X).labels_
  File "/usr/local/lib/python3.6/site-packages/sklearn/cluster/k_means_.py", line 887, in fit
    X = self._check_fit_data(X)
  File "/usr/local/lib/python3.6/site-packages/sklearn/cluster/k_means_.py", line 858, in _check_fit_data
    X = check_array(X, accept_sparse='csr', dtype=[np.float64, np.float32])
  File "/usr/local/lib/python3.6/site-packages/sklearn/utils/validation.py", line 470, in check_array
    context))
ValueError: Found array with 0 feature(s) (shape=(10, 0)) while a minimum of 1 is required.
```

## ERROR CLUSTER 3

 * Number of instances: 21
 * Dataset ids: scaling_0023, scaling_0065, scaling_0170, scaling_0218, scaling_0343, scaling_0355, scaling_0405, scaling_0497, scaling_0499, scaling_0512, scaling_0538, scaling_0634, scaling_0716, scaling_0750, scaling_0821, scaling_0839, scaling_0935, scaling_0943, scaling_0945, scaling_0963, scaling_0975

Last 10 lines of scaling_0023:
```
    r,cluster_assigned_to_temp=clusterassignment(connected_nodes_to_node,temp_nodes)
  File "/FORKS/deng_2014_python/forks_fcns.py", line 900, in clusterassignment
    nbrs = NearestNeighbors(n_neighbors=1, algorithm='ball_tree').fit(temp1)
  File "/usr/local/lib/python3.6/site-packages/sklearn/neighbors/base.py", line 803, in fit
    return self._fit(X)
  File "/usr/local/lib/python3.6/site-packages/sklearn/neighbors/base.py", line 204, in _fit
    X = check_array(X, accept_sparse='csr')
  File "/usr/local/lib/python3.6/site-packages/sklearn/utils/validation.py", line 462, in check_array
    context))
ValueError: Found array with 0 sample(s) (shape=(0, 2)) while a minimum of 1 is required.
```

## ERROR CLUSTER 4

 * Number of instances: 105
 * Dataset ids: scaling_0051, scaling_0056, scaling_0061, scaling_0066, scaling_0077, scaling_0082, scaling_0083, scaling_0088, scaling_0089, scaling_0095, scaling_0101, scaling_0108, scaling_0114, scaling_0115, scaling_0120, scaling_0121, scaling_0122, scaling_0128, scaling_0129, scaling_0135, scaling_0136, scaling_0143, scaling_0145, scaling_0147, scaling_0149, scaling_0153, scaling_0159, scaling_0163, scaling_0164, scaling_0165, scaling_0168, scaling_0171, scaling_0176, scaling_0177, scaling_0180, scaling_0184, scaling_0190, scaling_0191, scaling_0196, scaling_0197, scaling_0198, scaling_0204, scaling_0205, scaling_0210, scaling_0211, scaling_0212, scaling_0220, scaling_0224, scaling_0229, scaling_0234, scaling_0235, scaling_0240, scaling_0241, scaling_0246, scaling_0247, scaling_0252, scaling_0253, scaling_0265, scaling_0272, scaling_0273, scaling_0282, scaling_0292, scaling_0293, scaling_0302, scaling_0303, scaling_0304, scaling_0315, scaling_0324, scaling_0325, scaling_0326, scaling_0347, scaling_0356, scaling_0357, scaling_0365, scaling_0366, scaling_0367, scaling_0377, scaling_0385, scaling_0386, scaling_0387, scaling_0399, scaling_0411, scaling_0412, scaling_0424, scaling_0425, scaling_0438, scaling_0449, scaling_0450, scaling_0451, scaling_0471, scaling_0481, scaling_0501, scaling_0524, scaling_0597, scaling_0872, scaling_0883, scaling_0894, scaling_0905, scaling_0959, scaling_0960, scaling_0965, scaling_0966, scaling_0972, scaling_0977, scaling_0978

Last 10 lines of scaling_0051:
```
Traceback (most recent call last):
  File "/code/run.sh", line 57, in <module>
    pca.fit(data1)
  File "/usr/local/lib/python3.6/site-packages/sklearn/decomposition/pca.py", line 329, in fit
    self._fit(X)
  File "/usr/local/lib/python3.6/site-packages/sklearn/decomposition/pca.py", line 370, in _fit
    copy=self.copy)
  File "/usr/local/lib/python3.6/site-packages/sklearn/utils/validation.py", line 470, in check_array
    context))
ValueError: Found array with 0 feature(s) (shape=(1, 0)) while a minimum of 1 is required.
```

## ERROR CLUSTER 5

 * Number of instances: 4
 * Dataset ids: scaling_0106, scaling_0141, scaling_0228, scaling_0322

Last 10 lines of scaling_0106:
```
    n_clus,max_sil,sil_scores=find_nclusters(mappedData,range_clusters)
  File "/FORKS/deng_2014_python/forks_fcns.py", line 414, in find_nclusters
    cluster_labels = clusterer.fit_predict(X)
  File "/usr/local/lib/python3.6/site-packages/sklearn/cluster/k_means_.py", line 917, in fit_predict
    return self.fit(X).labels_
  File "/usr/local/lib/python3.6/site-packages/sklearn/cluster/k_means_.py", line 887, in fit
    X = self._check_fit_data(X)
  File "/usr/local/lib/python3.6/site-packages/sklearn/cluster/k_means_.py", line 861, in _check_fit_data
    X.shape[0], self.n_clusters))
ValueError: n_samples=3 should be >= n_clusters=4
```

## ERROR CLUSTER 6

 * Number of instances: 50
 * Dataset ids: scaling_0127, scaling_0151, scaling_0157, scaling_0169, scaling_0181, scaling_0182, scaling_0183, scaling_0188, scaling_0189, scaling_0195, scaling_0202, scaling_0216, scaling_0232, scaling_0238, scaling_0239, scaling_0261, scaling_0281, scaling_0287, scaling_0298, scaling_0299, scaling_0300, scaling_0321, scaling_0346, scaling_0351, scaling_0363, scaling_0371, scaling_0376, scaling_0382, scaling_0383, scaling_0404, scaling_0430, scaling_0437, scaling_0445, scaling_0447, scaling_0466, scaling_0486, scaling_0515, scaling_0565, scaling_0583, scaling_0617, scaling_0649, scaling_0698, scaling_0732, scaling_0784, scaling_0820, scaling_0867, scaling_0889, scaling_0971, scaling_0995, scaling_1023

Last 10 lines of scaling_0127:
```
    n_clus,max_sil,sil_scores=find_nclusters(mappedData,range_clusters)
  File "/FORKS/deng_2014_python/forks_fcns.py", line 415, in find_nclusters
    silhouette_avg = silhouette_score(X, cluster_labels)
  File "/usr/local/lib/python3.6/site-packages/sklearn/metrics/cluster/unsupervised.py", line 101, in silhouette_score
    return np.mean(silhouette_samples(X, labels, metric=metric, **kwds))
  File "/usr/local/lib/python3.6/site-packages/sklearn/metrics/cluster/unsupervised.py", line 167, in silhouette_samples
    check_number_of_labels(len(le.classes_), X.shape[0])
  File "/usr/local/lib/python3.6/site-packages/sklearn/metrics/cluster/unsupervised.py", line 19, in check_number_of_labels
    "to n_samples - 1 (inclusive)" % n_labels)
ValueError: Number of labels is 8. Valid values are 2 to n_samples - 1 (inclusive)
```

## ERROR CLUSTER 7

 * Number of instances: 1
 * Dataset ids: scaling_0927

Last 10 lines of scaling_0927:
```
    kmeans = KMeans(n_clusters=M, random_state=0).fit(X_reduced)
  File "/usr/local/lib/python3.6/site-packages/sklearn/cluster/k_means_.py", line 887, in fit
    X = self._check_fit_data(X)
  File "/usr/local/lib/python3.6/site-packages/sklearn/cluster/k_means_.py", line 858, in _check_fit_data
    X = check_array(X, accept_sparse='csr', dtype=[np.float64, np.float32])
  File "/usr/local/lib/python3.6/site-packages/sklearn/utils/validation.py", line 453, in check_array
    _assert_all_finite(array)
  File "/usr/local/lib/python3.6/site-packages/sklearn/utils/validation.py", line 44, in _assert_all_finite
    " or a value too large for %r." % X.dtype)
ValueError: Input contains NaN, infinity or a value too large for dtype('float64').
```


