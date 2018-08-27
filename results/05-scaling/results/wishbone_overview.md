# wishbone
![Overview](wishbone.svg)

## ERROR CLUSTER 1

 * Number of instances: 164
 * Dataset ids: scaling_0001, scaling_0004, scaling_0006, scaling_0007, scaling_0009, scaling_0011, scaling_0012, scaling_0013, scaling_0015, scaling_0016, scaling_0017, scaling_0019, scaling_0022, scaling_0025, scaling_0026, scaling_0031, scaling_0033, scaling_0037, scaling_0039, scaling_0041, scaling_0043, scaling_0045, scaling_0047, scaling_0051, scaling_0052, scaling_0056, scaling_0057, scaling_0061, scaling_0062, scaling_0066, scaling_0067, scaling_0076, scaling_0077, scaling_0082, scaling_0083, scaling_0088, scaling_0089, scaling_0094, scaling_0095, scaling_0106, scaling_0107, scaling_0113, scaling_0114, scaling_0120, scaling_0121, scaling_0127, scaling_0128, scaling_0151, scaling_0152, scaling_0157, scaling_0158, scaling_0163, scaling_0164, scaling_0169, scaling_0170, scaling_0176, scaling_0181, scaling_0188, scaling_0195, scaling_0202, scaling_0216, scaling_0218, scaling_0220, scaling_0222, scaling_0224, scaling_0226, scaling_0227, scaling_0232, scaling_0233, scaling_0238, scaling_0239, scaling_0244, scaling_0245, scaling_0251, scaling_0276, scaling_0277, scaling_0287, scaling_0288, scaling_0298, scaling_0299, scaling_0309, scaling_0310, scaling_0321, scaling_0341, scaling_0342, scaling_0351, scaling_0352, scaling_0361, scaling_0362, scaling_0371, scaling_0372, scaling_0382, scaling_0391, scaling_0392, scaling_0404, scaling_0405, scaling_0417, scaling_0418, scaling_0430, scaling_0431, scaling_0444, scaling_0456, scaling_0466, scaling_0476, scaling_0486, scaling_0506, scaling_0510, scaling_0514, scaling_0518, scaling_0522, scaling_0526, scaling_0532, scaling_0538, scaling_0544, scaling_0550, scaling_0556, scaling_0565, scaling_0574, scaling_0583, scaling_0601, scaling_0602, scaling_0617, scaling_0618, scaling_0633, scaling_0634, scaling_0649, scaling_0650, scaling_0666, scaling_0681, scaling_0682, scaling_0698, scaling_0699, scaling_0715, scaling_0716, scaling_0732, scaling_0733, scaling_0750, scaling_0766, scaling_0767, scaling_0784, scaling_0785, scaling_0802, scaling_0803, scaling_0820, scaling_0821, scaling_0839, scaling_0856, scaling_0867, scaling_0878, scaling_0889, scaling_0911, scaling_0919, scaling_0927, scaling_0935, scaling_0943, scaling_0951, scaling_0957, scaling_0963, scaling_0969, scaling_0975, scaling_0981, scaling_0995, scaling_1009, scaling_1023

Last 10 lines of scaling_0001:
```
  /code/run.sh
Target dimensionality reduced to 10.
Traceback (most recent call last):
  File "/code/run.sh", line 39, in <module>
    scdata.run_diffusion_map(knn=p["knn"], epsilon=p["epsilon"], n_diffusion_components=p["n_diffusion_components"], n_pca_components=p["n_pca_components"], markers=markers)
  File "/usr/local/lib/python3.6/site-packages/wishbone/wb.py", line 601, in run_diffusion_map
    distances, indices = nbrs.kneighbors(data)
  File "/usr/local/lib/python3.6/site-packages/sklearn/neighbors/base.py", line 347, in kneighbors
    (train_size, n_neighbors)
ValueError: Expected n_neighbors <= n_samples,  but n_samples = 10, n_neighbors = 25
```

## ERROR CLUSTER 2

 * Number of instances: 19
 * Dataset ids: scaling_0002, scaling_0003, scaling_0008, scaling_0010, scaling_0020, scaling_0023, scaling_0029, scaling_0035, scaling_0053, scaling_0058, scaling_0068, scaling_0072, scaling_0084, scaling_0101, scaling_0102, scaling_0122, scaling_0135, scaling_0136, scaling_0149

Last 10 lines of scaling_0002:
```
    nbrs = NearestNeighbors(n_neighbors=knn).fit(data)
  File "/usr/local/lib/python3.6/site-packages/sklearn/neighbors/base.py", line 803, in fit
    return self._fit(X)
  File "/usr/local/lib/python3.6/site-packages/sklearn/neighbors/base.py", line 204, in _fit
    X = check_array(X, accept_sparse='csr')
  File "/usr/local/lib/python3.6/site-packages/sklearn/utils/validation.py", line 453, in check_array
    _assert_all_finite(array)
  File "/usr/local/lib/python3.6/site-packages/sklearn/utils/validation.py", line 44, in _assert_all_finite
    " or a value too large for %r." % X.dtype)
ValueError: Input contains NaN, infinity or a value too large for dtype('float32').
```

## ERROR CLUSTER 3

 * Number of instances: 82
 * Dataset ids: scaling_0018, scaling_0021, scaling_0024, scaling_0027, scaling_0030, scaling_0032, scaling_0034, scaling_0036, scaling_0038, scaling_0040, scaling_0063, scaling_0073, scaling_0078, scaling_0090, scaling_0096, scaling_0108, scaling_0115, scaling_0129, scaling_0141, scaling_0143, scaling_0145, scaling_0147, scaling_0182, scaling_0189, scaling_0196, scaling_0203, scaling_0210, scaling_0256, scaling_0260, scaling_0264, scaling_0268, scaling_0272, scaling_0278, scaling_0289, scaling_0300, scaling_0311, scaling_0322, scaling_0331, scaling_0333, scaling_0335, scaling_0337, scaling_0339, scaling_0393, scaling_0406, scaling_0419, scaling_0432, scaling_0445, scaling_0457, scaling_0467, scaling_0477, scaling_0487, scaling_0497, scaling_0557, scaling_0566, scaling_0575, scaling_0584, scaling_0593, scaling_0603, scaling_0619, scaling_0635, scaling_0651, scaling_0667, scaling_0683, scaling_0700, scaling_0717, scaling_0734, scaling_0751, scaling_0768, scaling_0786, scaling_0804, scaling_0822, scaling_0840, scaling_0857, scaling_0868, scaling_0879, scaling_0890, scaling_0901, scaling_0982, scaling_0996, scaling_1010, scaling_1024, scaling_1038

Last 10 lines of scaling_0018:
```
    wb.run_wishbone(start_cell=start_cell, components_list=list(range(p["n_diffusion_components"])), num_waypoints=int(p["num_waypoints"]), branch=branch, k=p["k"])
  File "/usr/local/lib/python3.6/site-packages/wishbone/wb.py", line 1068, in run_wishbone
    s=s, k=k, l=k, num_waypoints=num_waypoints, branch=branch)
  File "/usr/local/lib/python3.6/site-packages/wishbone/core.py", line 27, in wishbone
    lnn = nbrs.kneighbors_graph(data, mode='distance' ) 
  File "/usr/local/lib/python3.6/site-packages/sklearn/neighbors/base.py", line 489, in kneighbors_graph
    X, n_neighbors, return_distance=True)
  File "/usr/local/lib/python3.6/site-packages/sklearn/neighbors/base.py", line 347, in kneighbors
    (train_size, n_neighbors)
ValueError: Expected n_neighbors <= n_samples,  but n_samples = 25, n_neighbors = 26
```

## ERROR CLUSTER 4

 * Number of instances: 2
 * Dataset ids: scaling_0046, scaling_0065

Last 10 lines of scaling_0046:
```
15 realignment iterations
output saved in /data/tmp//RtmpwSwhS7/file13cbd97e62a7/ti/output: 
	cell_ids.csv
	dimred.csv
	milestone_network.csv
	progressions.csv
	pseudotime.csv
	timings.json
all(pg_check >= 0 & pg_check < (1 + 1e-08)) isn't true.
Sum of progressions per cell_id should be exactly one
```

## ERROR CLUSTER 5

 * Number of instances: 6
 * Dataset ids: scaling_0137, scaling_0257, scaling_0279, scaling_0343, scaling_0684, scaling_0983

Last 10 lines of scaling_0137:
```
Traceback (most recent call last):
  File "/code/run.sh", line 47, in <module>
    wb.run_wishbone(start_cell=start_cell, components_list=list(range(p["n_diffusion_components"])), num_waypoints=int(p["num_waypoints"]), branch=branch, k=p["k"])
  File "/usr/local/lib/python3.6/site-packages/wishbone/wb.py", line 1068, in run_wishbone
    s=s, k=k, l=k, num_waypoints=num_waypoints, branch=branch)
  File "/usr/local/lib/python3.6/site-packages/wishbone/core.py", line 54, in wishbone
    RNK, bp, diffdists, Y = _splittobranches(traj, traj[0], data, iter_l, dist, paths_l2l)
  File "/usr/local/lib/python3.6/site-packages/wishbone/core.py", line 346, in _splittobranches
    Y = np.zeros((len(RNK)))
TypeError: object of type 'numpy.float64' has no len()
```

## ERROR CLUSTER 6

 * Number of instances: 86
 * Dataset ids: scaling_0206, scaling_0270, scaling_0314, scaling_0369, scaling_0375, scaling_0428, scaling_0436, scaling_0437, scaling_0484, scaling_0491, scaling_0503, scaling_0517, scaling_0519, scaling_0520, scaling_0525, scaling_0547, scaling_0581, scaling_0587, scaling_0588, scaling_0598, scaling_0599, scaling_0600, scaling_0648, scaling_0655, scaling_0656, scaling_0657, scaling_0658, scaling_0659, scaling_0680, scaling_0728, scaling_0729, scaling_0731, scaling_0738, scaling_0739, scaling_0740, scaling_0741, scaling_0742, scaling_0761, scaling_0763, scaling_0764, scaling_0765, scaling_0794, scaling_0801, scaling_0816, scaling_0817, scaling_0819, scaling_0826, scaling_0827, scaling_0828, scaling_0829, scaling_0830, scaling_0831, scaling_0832, scaling_0851, scaling_0852, scaling_0854, scaling_0855, scaling_0877, scaling_0886, scaling_0888, scaling_0892, scaling_0893, scaling_0894, scaling_0895, scaling_0908, scaling_0909, scaling_0910, scaling_0937, scaling_0938, scaling_0939, scaling_0940, scaling_0949, scaling_0950, scaling_0971, scaling_0972, scaling_0979, scaling_0980, scaling_1008, scaling_1022, scaling_1027, scaling_1028, scaling_1029, scaling_1030, scaling_1031, scaling_1049, scaling_1050

Last 10 lines of scaling_0206:
```
    wb.run_wishbone(start_cell=start_cell, components_list=list(range(p["n_diffusion_components"])), num_waypoints=int(p["num_waypoints"]), branch=branch, k=p["k"])
  File "/usr/local/lib/python3.6/site-packages/wishbone/wb.py", line 1068, in run_wishbone
    s=s, k=k, l=k, num_waypoints=num_waypoints, branch=branch)
  File "/usr/local/lib/python3.6/site-packages/wishbone/core.py", line 50, in wishbone
    traj, dist, iter_l, paths_l2l = _trajectory_landmarks( klnn, data, [s], num_waypoints, partial_order, verbose, metric, flock_waypoints, band_sample, branch)
  File "/usr/local/lib/python3.6/site-packages/wishbone/core.py", line 263, in _trajectory_landmarks
    paths_l2l.append( [paths[li] for li in l] )
  File "/usr/local/lib/python3.6/site-packages/wishbone/core.py", line 263, in <listcomp>
    paths_l2l.append( [paths[li] for li in l] )
KeyError: 8
```

## ERROR CLUSTER 7

 * Number of instances: 5
 * Dataset ids: scaling_0543, scaling_0555, scaling_0646, scaling_0762, scaling_1032

Last 10 lines of scaling_0543:
```
  File "/code/run.sh", line 47, in <module>
    wb.run_wishbone(start_cell=start_cell, components_list=list(range(p["n_diffusion_components"])), num_waypoints=int(p["num_waypoints"]), branch=branch, k=p["k"])
  File "/usr/local/lib/python3.6/site-packages/wishbone/wb.py", line 1068, in run_wishbone
    s=s, k=k, l=k, num_waypoints=num_waypoints, branch=branch)
  File "/usr/local/lib/python3.6/site-packages/wishbone/core.py", line 50, in wishbone
    traj, dist, iter_l, paths_l2l = _trajectory_landmarks( klnn, data, [s], num_waypoints, partial_order, verbose, metric, flock_waypoints, band_sample, branch)
  File "/usr/local/lib/python3.6/site-packages/wishbone/core.py", line 232, in _trajectory_landmarks
    tailband_sample = np.random.choice( tailband, size=tailk, replace=False)
  File "mtrand.pyx", line 1126, in mtrand.RandomState.choice
ValueError: a must be non-empty
```


