# mfa
![Overview](mfa.svg)

## ERROR CLUSTER 1

 * Number of instances: 1
 * Dataset ids: scaling_0357

Last 10 lines of scaling_0357:
```
Input saved to /data/tmp//RtmpPuW7Zz/file87b32917c89/ti/input: 
	data.rds
	params.json
Running singularity exec --pwd /ti/workspace -B \
  '/data/tmp//RtmpPuW7Zz/file87b32917c89/ti:/ti,/data/tmp//RtmpPuW7Zz/file87b350b6d878/tmp:/tmp2' \
  /group/irc/shared/dynverse/dynbenchmark/derived/03-method_characterisation/singularity_images/dynverse/mfa.simg \
  /code/run.sh
[91mERROR  : Home directory is not owned by calling user: /home/robrechtc
[0m[31mABORT  : Retval = 255
[0m
```


