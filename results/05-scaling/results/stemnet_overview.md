# stemnet
![Overview](stemnet.svg)

## ERROR CLUSTER 1

 * Number of instances: 19
 * Dataset ids: scaling_0046, scaling_0065, scaling_0074, scaling_0075, scaling_0092, scaling_0103, scaling_0198, scaling_0252, scaling_0254, scaling_0323, scaling_0388, scaling_0446, scaling_0479, scaling_0501, scaling_0669, scaling_0757, scaling_0842, scaling_0904, scaling_1039

Last 10 lines of scaling_0046:
```
    flatten
Error in lognet(x, is.sparse, ix, jx, y, weights, offset, alpha, nobs,  : 
  one multinomial or binomial class has 1 or 0 observations; not allowed
Calls: <Anonymous> -> <Anonymous> -> glmnet -> lognet
In addition: Warning messages:
1: In lognet(x, is.sparse, ix, jx, y, weights, offset, alpha, nobs,  :
  one multinomial or binomial class has fewer than 8  observations; dangerous ground
2: In lognet(x, is.sparse, ix, jx, y, weights, offset, alpha, nobs,  :
  one multinomial or binomial class has fewer than 8  observations; dangerous ground
Execution halted
```


