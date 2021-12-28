## Test environments

* local OS X install, R 4.1.2
* local Ubuntu 20.04 : Focal Fossa install, R 4.1.2 
* rhub
* GitHub actions (macOS, windows, ubuntu)
  - MacOS-latest (release)
  - windows-latest (release)
  - ubuntu-latest (devel)
  - ubuntu-latest (release)
  - ubuntu-latest (oldrel-1)

## R CMD check results

There were no ERRORs, WARNINGs or NOTEs.

There was 1 NOTE:

* checking installed package size ... NOTE
  
It appears that within the Windows and Linux architectures, the CHECK procedure returns only one NOTE regarding the fact that the libs subdirectory is beyond the 1MB threshold. However, this NOTE doesn't occur to the OSX. Our understanding is that this size inflation of the libs subdirectory is due to the use of the Rcpp package. Indeed, some functions of the simts package have been written in C++ using Rcpp without which various functions would lose a considerable amount of computational efficiency leading to major parts of the package becoming impractical to use.

On Ubuntu 20.04, there is an additional warning if the package qpdf (https://sourceforge.net/projects/qpdf/files/latest/download) is not previously installed. This appears to be a common note for package check on Linux systems and once installed, the warning does not appears on the package check.


## Downstream dependencies

There are currently no downstream dependencies for this package.


## Copyright Issues

As a result of being the first R package submitted to CRAN by the maintainer and the authors, there have been some issues regarding the assignment of copyright holders and contributors to the package. Indeed there are functions in the files polyroot.cpp and sampler.cpp whose copyright belongs to authors that are not direct contributors to the package. Having tried to understand how to cite these authors in the package description (and not having found explicit indications) we have added these authors as copyrights holders of the package (and not contributors to the package). We are ready to edit this description in any way that is appropriate should we receive any clear indication as to how to cite these authors.

## Solaris Compatibility

The initial release of the package (version 0.1.0) reported some problems with Solaris due to C++ code compatibilty. Some functions were misused on an integer type, which may cause the potential overloading ambiguity. In the latest version, we hope that these issues have been overcome but we remain available to correct the package should other issues arise.
