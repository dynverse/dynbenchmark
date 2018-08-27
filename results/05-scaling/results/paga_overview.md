# paga
![Overview](paga.svg)

## ERROR CLUSTER 1

 * Number of instances: 282
 * Dataset ids: scaling_0001, scaling_0002, scaling_0003, scaling_0004, scaling_0006, scaling_0007, scaling_0008, scaling_0009, scaling_0010, scaling_0011, scaling_0012, scaling_0013, scaling_0015, scaling_0016, scaling_0017, scaling_0018, scaling_0019, scaling_0020, scaling_0021, scaling_0022, scaling_0023, scaling_0024, scaling_0025, scaling_0026, scaling_0027, scaling_0029, scaling_0030, scaling_0031, scaling_0032, scaling_0033, scaling_0034, scaling_0035, scaling_0036, scaling_0037, scaling_0038, scaling_0039, scaling_0040, scaling_0042, scaling_0044, scaling_0046, scaling_0048, scaling_0050, scaling_0053, scaling_0054, scaling_0055, scaling_0058, scaling_0059, scaling_0060, scaling_0063, scaling_0064, scaling_0065, scaling_0068, scaling_0069, scaling_0070, scaling_0073, scaling_0074, scaling_0075, scaling_0079, scaling_0080, scaling_0081, scaling_0085, scaling_0086, scaling_0087, scaling_0091, scaling_0092, scaling_0093, scaling_0097, scaling_0098, scaling_0099, scaling_0103, scaling_0104, scaling_0105, scaling_0110, scaling_0111, scaling_0112, scaling_0117, scaling_0118, scaling_0119, scaling_0124, scaling_0125, scaling_0126, scaling_0131, scaling_0132, scaling_0133, scaling_0138, scaling_0139, scaling_0140, scaling_0142, scaling_0144, scaling_0146, scaling_0148, scaling_0150, scaling_0155, scaling_0156, scaling_0161, scaling_0162, scaling_0167, scaling_0168, scaling_0173, scaling_0174, scaling_0179, scaling_0180, scaling_0186, scaling_0187, scaling_0193, scaling_0194, scaling_0200, scaling_0201, scaling_0207, scaling_0208, scaling_0214, scaling_0215, scaling_0217, scaling_0219, scaling_0221, scaling_0223, scaling_0225, scaling_0230, scaling_0231, scaling_0236, scaling_0237, scaling_0242, scaling_0243, scaling_0248, scaling_0249, scaling_0254, scaling_0255, scaling_0259, scaling_0263, scaling_0267, scaling_0271, scaling_0275, scaling_0284, scaling_0285, scaling_0286, scaling_0295, scaling_0296, scaling_0297, scaling_0306, scaling_0307, scaling_0308, scaling_0317, scaling_0318, scaling_0319, scaling_0328, scaling_0329, scaling_0330, scaling_0332, scaling_0334, scaling_0336, scaling_0338, scaling_0340, scaling_0349, scaling_0350, scaling_0359, scaling_0360, scaling_0369, scaling_0370, scaling_0379, scaling_0380, scaling_0389, scaling_0390, scaling_0401, scaling_0402, scaling_0403, scaling_0414, scaling_0415, scaling_0416, scaling_0427, scaling_0428, scaling_0429, scaling_0440, scaling_0441, scaling_0442, scaling_0453, scaling_0454, scaling_0455, scaling_0464, scaling_0465, scaling_0474, scaling_0475, scaling_0484, scaling_0485, scaling_0494, scaling_0495, scaling_0504, scaling_0505, scaling_0509, scaling_0513, scaling_0517, scaling_0521, scaling_0525, scaling_0531, scaling_0537, scaling_0543, scaling_0549, scaling_0555, scaling_0563, scaling_0564, scaling_0572, scaling_0573, scaling_0581, scaling_0582, scaling_0590, scaling_0591, scaling_0599, scaling_0600, scaling_0614, scaling_0615, scaling_0616, scaling_0630, scaling_0631, scaling_0632, scaling_0646, scaling_0647, scaling_0648, scaling_0662, scaling_0663, scaling_0664, scaling_0678, scaling_0679, scaling_0680, scaling_0695, scaling_0696, scaling_0697, scaling_0712, scaling_0713, scaling_0714, scaling_0729, scaling_0730, scaling_0731, scaling_0746, scaling_0747, scaling_0748, scaling_0763, scaling_0764, scaling_0765, scaling_0781, scaling_0782, scaling_0783, scaling_0799, scaling_0800, scaling_0801, scaling_0817, scaling_0818, scaling_0819, scaling_0835, scaling_0836, scaling_0837, scaling_0853, scaling_0854, scaling_0855, scaling_0865, scaling_0866, scaling_0876, scaling_0877, scaling_0887, scaling_0888, scaling_0898, scaling_0899, scaling_0909, scaling_0910, scaling_0918, scaling_0926, scaling_0934, scaling_0942, scaling_0950, scaling_0956, scaling_0962, scaling_0968, scaling_0974, scaling_0980, scaling_0993, scaling_0994, scaling_1007, scaling_1008, scaling_1021, scaling_1022, scaling_1035, scaling_1036, scaling_1049, scaling_1050

Last 10 lines of scaling_0001:
```
  / disp_mad_bin[df['mean_bin']].values
Traceback (most recent call last):
  File "/code/run.sh", line 44, in <module>
    sc.pp.recipe_zheng17(adata, n_top_genes=n_top_genes)
  File "/usr/local/lib/python3.6/site-packages/scanpy/preprocessing/recipes.py", line 107, in recipe_zheng17
    adata.X, flavor='cell_ranger', n_top_genes=n_top_genes, log=False)
  File "/usr/local/lib/python3.6/site-packages/scanpy/preprocessing/simple.py", line 356, in filter_genes_dispersion
    disp_cut_off = dispersion_norm[n_top_genes-1]
IndexError: index 9 is out of bounds for axis 0 with size 0
Loading required namespace: hdf5r
```

## ERROR CLUSTER 2

 * Number of instances: 143
 * Dataset ids: scaling_0045, scaling_0047, scaling_0051, scaling_0052, scaling_0057, scaling_0062, scaling_0066, scaling_0067, scaling_0072, scaling_0078, scaling_0082, scaling_0084, scaling_0088, scaling_0094, scaling_0095, scaling_0101, scaling_0102, scaling_0106, scaling_0109, scaling_0114, scaling_0120, scaling_0121, scaling_0122, scaling_0123, scaling_0127, scaling_0128, scaling_0130, scaling_0135, scaling_0137, scaling_0141, scaling_0143, scaling_0145, scaling_0147, scaling_0151, scaling_0157, scaling_0158, scaling_0163, scaling_0164, scaling_0165, scaling_0171, scaling_0172, scaling_0177, scaling_0178, scaling_0181, scaling_0183, scaling_0188, scaling_0195, scaling_0196, scaling_0197, scaling_0198, scaling_0203, scaling_0205, scaling_0210, scaling_0211, scaling_0216, scaling_0220, scaling_0224, scaling_0226, scaling_0232, scaling_0233, scaling_0234, scaling_0239, scaling_0240, scaling_0246, scaling_0251, scaling_0257, scaling_0260, scaling_0265, scaling_0268, scaling_0273, scaling_0276, scaling_0277, scaling_0278, scaling_0279, scaling_0280, scaling_0288, scaling_0290, scaling_0291, scaling_0299, scaling_0300, scaling_0301, scaling_0302, scaling_0309, scaling_0310, scaling_0311, scaling_0323, scaling_0324, scaling_0331, scaling_0333, scaling_0339, scaling_0342, scaling_0343, scaling_0344, scaling_0362, scaling_0363, scaling_0364, scaling_0371, scaling_0374, scaling_0382, scaling_0391, scaling_0394, scaling_0407, scaling_0408, scaling_0431, scaling_0432, scaling_0445, scaling_0456, scaling_0457, scaling_0466, scaling_0476, scaling_0477, scaling_0497, scaling_0498, scaling_0544, scaling_0550, scaling_0556, scaling_0565, scaling_0566, scaling_0567, scaling_0617, scaling_0620, scaling_0634, scaling_0668, scaling_0682, scaling_0701, scaling_0718, scaling_0733, scaling_0734, scaling_0752, scaling_0768, scaling_0802, scaling_0803, scaling_0820, scaling_0823, scaling_0840, scaling_0841, scaling_0886, scaling_0908, scaling_0992, scaling_1006, scaling_1009, scaling_1011, scaling_1034

Last 10 lines of scaling_0045:
```
  File "/usr/local/lib/python3.6/site-packages/matplotlib/transforms.py", line 2507, in inverted
    return CompositeGenericTransform(self._b.inverted(), self._a.inverted())
  File "/usr/local/lib/python3.6/site-packages/matplotlib/transforms.py", line 2507, in inverted
    return CompositeGenericTransform(self._b.inverted(), self._a.inverted())
  File "/usr/local/lib/python3.6/site-packages/matplotlib/transforms.py", line 1901, in inverted
    self._inverted = Affine2D(inv(mtx), shorthand_name=shorthand_name)
  File "/usr/local/lib/python3.6/site-packages/numpy/linalg/linalg.py", line 532, in inv
    ainv = _umath_linalg.inv(a, signature=signature, extobj=extobj)
KeyboardInterrupt
Loading required namespace: hdf5r
```

## ERROR CLUSTER 3

 * Number of instances: 1
 * Dataset ids: scaling_0852

Last 10 lines of scaling_0852:
```
  File "/usr/local/lib/python3.6/site-packages/sklearn/decomposition/pca.py", line 348, in fit_transform
    U, S, V = self._fit(X)
  File "/usr/local/lib/python3.6/site-packages/sklearn/decomposition/pca.py", line 370, in _fit
    copy=self.copy)
  File "/usr/local/lib/python3.6/site-packages/sklearn/utils/validation.py", line 453, in check_array
    _assert_all_finite(array)
  File "/usr/local/lib/python3.6/site-packages/sklearn/utils/validation.py", line 44, in _assert_all_finite
    " or a value too large for %r." % X.dtype)
ValueError: Input contains NaN, infinity or a value too large for dtype('float32').
Loading required namespace: hdf5r
```


