## Test environments

* local OS X install, R 3.5.3
* local Windows install, R 3.5.3
* ubuntu 14.04 (on travis-ci), R 3.5.3
* win-builder
* appveyor
* rhub


## R CMD check results

There was 2 NOTEs:

* checking CRAN incoming feasibility ... NOTE
* checking installed package size ... NOTE

  This is a new submission, which is the reason for the first note.
  
  It appears that within the Windows architecture, the CHECK procedure returns only one NOTE regarding the fact that the libs subdirectory is beyond the 1MB threshold. However, this NOTE only occurs for Windows systems while it isn't the case for Linux or OSX. My understanding is that this size inflation of the libs subdirectory is due to the use of the Rcpp package. Indeed, some functions of the simts package have been written in C++ using Rcpp without which various functions would lose a considerable amount of computational efficiency leading to major parts of the package becoming impractical to use.


## Downstream dependencies

There are currently no downstream dependencies for this package.