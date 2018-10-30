## Test environments

* local OS X install, R 3.5.1
* local Windows install, R 3.5.1
* ubuntu 14.04 (on travis-ci), R 3.1.2
* win-builder
* appveyor


## R CMD check results

There was 2 NOTEs:

* checking CRAN incoming feasibility ... NOTE
* checking installed package size ... NOTE

  This is a new submission, which is the reason for the first note.
  
  It seems that on Windows architectures, the CHECK returns one NOTE because  the libs subdirectory is then above the 1MB threshold. However, it seems    that this NOTE only appears under Windows, but not under Linux or OSX. My   understanding is that this inflation of the libs subdirectory is due to the use of Rcpp. Indeed, some functions of the simts package have been       written in C++ using Rcpp. Without the speed up gained from those C++ functions, this package would become impractical.


## Downstream dependencies

There are currently no downstream dependencies for this package.