
# Method testing

Here, we test the methods on some small real and toy datasets, to capture errors early on and make sure all methods run fine on these "simple" datasets.

These testing results are in the end pushed to <https://dynverse.github.io/dynmethods_automated_testing/index.html>

| \#  | script                                        | description                                                                   |
|:----|:----------------------------------------------|:------------------------------------------------------------------------------|
| 1   | [📄`submit_jobs.R`](1-submit_jobs.R)           | Start the method testing on the cluster                                       |
| 2   | [📄`retrieve_results.R`](2-retrieve_results.R) | Retrieve the results from method testing                                      |
| 3   | [📄`create_site.R`](3-create_site.R)           | Create the website containing the method testing results for the world to see |
