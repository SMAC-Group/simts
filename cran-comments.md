## Test environments

* local OS X install, R 3.6.1
* local Windows install, R 3.6.1
* ubuntu 14.04 (on travis-ci), R 3.6.1
* win-builder
* appveyor
* rhub


## R CMD check results

There were 2 NOTES:

* checking CRAN incoming feasibility ... NOTE
* checking installed package size ... NOTE

  This is a new submission, which is the reason for the first note.
  
  It appears that within the Windows architecture, the CHECK procedure returns only one NOTE regarding the fact that the libs subdirectory is beyond the 1MB threshold. However, this NOTE only occurs for Windows systems while it isn't the case for Linux or OSX. My understanding is that this size inflation of the libs subdirectory is due to the use of the Rcpp package. Indeed, some functions of the simts package have been written in C++ using Rcpp without which various functions would lose a considerable amount of computational efficiency leading to major parts of the package becoming impractical to use.


## Downstream dependencies

There are currently no downstream dependencies for this package.


## Copyright Issues

As a result of being the first R package submitted to CRAN by the maintainer and the authors, there have been some issues regarding the assignment of copyright holders and contributors to the package. Indeed there are functions in the files polyroot.cpp and sampler.cpp whose copyright belongs to authors that are not direct contributors to the package. Having tried to understand how to cite these authors in the package description (and not having found explicit indications) we have added these authors as copyrights holders of the package (and not contributors to the package). We are ready to edit this description in any way that is appropriate should we receive any clear indication as to how to cite these authors.

## Solaris Compatibility

The initial release of the package reported some problems with Solaris due to C++ code compatibilty. With the current version we hope that these issues have been overcome but we remain available to correct the package should other issues arise.
