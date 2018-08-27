# scimitar
![Overview](scimitar.svg)

## ERROR CLUSTER 1

 * Number of instances: 61
 * Dataset ids: scaling_0002, scaling_0010, scaling_0023, scaling_0036, scaling_0040, scaling_0050, scaling_0053, scaling_0064, scaling_0082, scaling_0087, scaling_0090, scaling_0103, scaling_0113, scaling_0115, scaling_0121, scaling_0125, scaling_0137, scaling_0143, scaling_0157, scaling_0177, scaling_0188, scaling_0189, scaling_0190, scaling_0220, scaling_0232, scaling_0244, scaling_0260, scaling_0261, scaling_0273, scaling_0287, scaling_0288, scaling_0289, scaling_0290, scaling_0291, scaling_0309, scaling_0323, scaling_0331, scaling_0333, scaling_0351, scaling_0353, scaling_0354, scaling_0371, scaling_0372, scaling_0392, scaling_0404, scaling_0406, scaling_0407, scaling_0414, scaling_0415, scaling_0430, scaling_0431, scaling_0458, scaling_0464, scaling_0467, scaling_0506, scaling_0518, scaling_0566, scaling_0567, scaling_0596, scaling_0760, scaling_0790

Last 10 lines of scaling_0002:
```
  File "/usr/local/lib/python2.7/site-packages/scimitar/morphing_mixture.py", line 309, in morphing_mixture_from_pseudotime
    covariances.append(np.copy(corpcor.cov_shrink(data_array, weights=weights, **{'lambda':cov_reg})))
  File "/usr/local/lib/python2.7/site-packages/pyroconductor/corpcor.py", line 12, in cov_shrink
    **kwargs)
  File "/usr/local/lib/python2.7/site-packages/rpy2/robjects/functions.py", line 178, in __call__
    return super(SignatureTranslatedFunction, self).__call__(*args, **kwargs)
  File "/usr/local/lib/python2.7/site-packages/rpy2/robjects/functions.py", line 106, in __call__
    res = super(Function, self).__call__(*new_args, **new_kwargs)
rpy2.rinterface.RRuntimeError: Error in if (denominator == 0) lambda.var = 1 else lambda.var = max(0,  : 
  missing value where TRUE/FALSE needed
```

## ERROR CLUSTER 2

 * Number of instances: 128
 * Dataset ids: scaling_0003, scaling_0008, scaling_0009, scaling_0016, scaling_0019, scaling_0022, scaling_0024, scaling_0029, scaling_0033, scaling_0039, scaling_0043, scaling_0045, scaling_0056, scaling_0061, scaling_0072, scaling_0083, scaling_0084, scaling_0088, scaling_0089, scaling_0092, scaling_0101, scaling_0105, scaling_0120, scaling_0135, scaling_0138, scaling_0139, scaling_0140, scaling_0141, scaling_0144, scaling_0158, scaling_0160, scaling_0163, scaling_0176, scaling_0179, scaling_0180, scaling_0181, scaling_0183, scaling_0195, scaling_0202, scaling_0211, scaling_0215, scaling_0216, scaling_0224, scaling_0226, scaling_0227, scaling_0229, scaling_0233, scaling_0235, scaling_0238, scaling_0239, scaling_0251, scaling_0253, scaling_0256, scaling_0257, scaling_0258, scaling_0275, scaling_0276, scaling_0277, scaling_0279, scaling_0280, scaling_0293, scaling_0298, scaling_0303, scaling_0310, scaling_0321, scaling_0322, scaling_0327, scaling_0329, scaling_0341, scaling_0342, scaling_0343, scaling_0344, scaling_0352, scaling_0361, scaling_0370, scaling_0382, scaling_0383, scaling_0386, scaling_0387, scaling_0393, scaling_0394, scaling_0395, scaling_0398, scaling_0399, scaling_0405, scaling_0408, scaling_0409, scaling_0417, scaling_0425, scaling_0432, scaling_0444, scaling_0452, scaling_0457, scaling_0459, scaling_0468, scaling_0469, scaling_0470, scaling_0485, scaling_0487, scaling_0497, scaling_0522, scaling_0527, scaling_0528, scaling_0533, scaling_0534, scaling_0537, scaling_0558, scaling_0560, scaling_0593, scaling_0605, scaling_0608, scaling_0609, scaling_0621, scaling_0622, scaling_0623, scaling_0624, scaling_0630, scaling_0645, scaling_0672, scaling_0688, scaling_0689, scaling_0703, scaling_0704, scaling_0757, scaling_0794, scaling_0795, scaling_0811, scaling_0848

Last 10 lines of scaling_0003:
```
    pseudotimes, cov_estimator=cov_estimator, **kwargs)
  File "/usr/local/lib/python2.7/site-packages/scimitar/morphing_mixture.py", line 325, in morphing_mixture_from_pseudotime
    timepoints=timepoints)
  File "/usr/local/lib/python2.7/site-packages/scimitar/morphing_mixture.py", line 202, in state_interpolation
    sorted_chols = np.array([np.linalg.cholesky(c).tolist() for c in sorted_covariances])
  File "/usr/local/lib/python2.7/site-packages/numpy/linalg/linalg.py", line 733, in cholesky
    r = gufunc(a, signature=signature, extobj=extobj)
  File "/usr/local/lib/python2.7/site-packages/numpy/linalg/linalg.py", line 92, in _raise_linalgerror_nonposdef
    raise LinAlgError("Matrix is not positive definite")
numpy.linalg.linalg.LinAlgError: Matrix is not positive definite
```

## ERROR CLUSTER 3

 * Number of instances: 72
 * Dataset ids: scaling_0006, scaling_0030, scaling_0051, scaling_0073, scaling_0074, scaling_0080, scaling_0104, scaling_0122, scaling_0145, scaling_0149, scaling_0164, scaling_0165, scaling_0166, scaling_0197, scaling_0199, scaling_0201, scaling_0210, scaling_0214, scaling_0234, scaling_0240, scaling_0252, scaling_0254, scaling_0264, scaling_0265, scaling_0272, scaling_0278, scaling_0281, scaling_0299, scaling_0301, scaling_0302, scaling_0304, scaling_0306, scaling_0324, scaling_0325, scaling_0326, scaling_0339, scaling_0355, scaling_0364, scaling_0365, scaling_0366, scaling_0367, scaling_0384, scaling_0420, scaling_0421, scaling_0422, scaling_0423, scaling_0424, scaling_0448, scaling_0451, scaling_0461, scaling_0483, scaling_0501, scaling_0507, scaling_0515, scaling_0524, scaling_0554, scaling_0561, scaling_0577, scaling_0597, scaling_0598, scaling_0641, scaling_0643, scaling_0674, scaling_0676, scaling_0723, scaling_0726, scaling_0758, scaling_0775, scaling_0776, scaling_0810, scaling_0872, scaling_0905

Last 10 lines of scaling_0006:
```
    cov_reg=p["cov_reg"]
  File "/usr/local/lib/python2.7/site-packages/scimitar/morphing_mixture.py", line 125, in refine
    fit_type=self.fit_type, degree=self.degree, **kwargs)
  File "/usr/local/lib/python2.7/site-packages/scimitar/morphing_mixture.py", line 325, in morphing_mixture_from_pseudotime
    timepoints=timepoints)
  File "/usr/local/lib/python2.7/site-packages/scimitar/morphing_mixture.py", line 221, in state_interpolation
    spl = UnivariateSpline(timepoints, sorted_means[:, i], k=degree, w=sorted_covariances[:, i, i])
  File "/usr/local/lib/python2.7/site-packages/scipy/interpolate/fitpack2.py", line 185, in __init__
    xb=bbox[0],xe=bbox[1],s=s)
dfitpack.error: (m>k) failed for hidden m: fpcurf0:m=2
```

## ERROR CLUSTER 4

 * Number of instances: 1
 * Dataset ids: scaling_0011

Last 10 lines of scaling_0011:
```
    covariances.append(np.copy(corpcor.cov_shrink(data_array, weights=weights, **{'lambda':cov_reg})))
  File "/usr/local/lib/python2.7/site-packages/pyroconductor/corpcor.py", line 12, in cov_shrink
    **kwargs)
  File "/usr/local/lib/python2.7/site-packages/rpy2/robjects/functions.py", line 178, in __call__
    return super(SignatureTranslatedFunction, self).__call__(*args, **kwargs)
  File "/usr/local/lib/python2.7/site-packages/rpy2/robjects/functions.py", line 106, in __call__
    res = super(Function, self).__call__(*new_args, **new_kwargs)
rpy2.rinterface.RRuntimeError: Error in if (denominator == 0) lambda.var = 1 else lambda.var = max(0,  : 
  missing value where TRUE/FALSE needed
ity lambda.var (variance vector): 
```

## ERROR CLUSTER 5

 * Number of instances: 4
 * Dataset ids: scaling_0766, scaling_0784, scaling_0802, scaling_0820

Last 10 lines of scaling_0766:
```
    self._fit_transform(X)
  File "/usr/local/lib/python2.7/site-packages/sklearn/manifold/locally_linear.py", line 645, in _fit_transform
    random_state=random_state, reg=self.reg, n_jobs=self.n_jobs)
  File "/usr/local/lib/python2.7/site-packages/sklearn/manifold/locally_linear.py", line 511, in locally_linear_embedding
    tol=tol, max_iter=max_iter, random_state=random_state)
  File "/usr/local/lib/python2.7/site-packages/sklearn/manifold/locally_linear.py", line 179, in null_space
    M, eigvals=(k_skip, k + k_skip - 1), overwrite_a=True)
  File "/usr/local/lib/python2.7/site-packages/scipy/linalg/decomp.py", line 436, in eigh
    il=lo, iu=hi, overwrite_a=overwrite_a)
KeyboardInterrupt
```


