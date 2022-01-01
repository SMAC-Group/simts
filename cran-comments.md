## Test environments 

* local OS X install, R 4.1.2 No warnings and no errors.
* local Ubuntu 20.04 : Focal Fossa install, R 4.1.2 No warnings and no errors.
* [`rhub`](https://r-hub.github.io/rhub/)
  No warnings and no errors on containers used with `rhub::check_for_cran()`.
  Find detailed output of `check_for_cran()` below:

```
── simts 0.2.0: NOTE

  Build ID:   simts_0.2.0.tar.gz-2f1becd804d047acadeac0cc43025dba
  Platform:   Windows Server 2022, R-devel, 64 bit
  Submitted:  35m 19s ago
  Build time: 17m 37.3s

> checking for detritus in the temp directory ... NOTE
  Found the following files/directories:
    'lastMiKTeXException'

0 errors ✓ | 0 warnings ✓ | 1 note x

── simts 0.2.0: NOTE

  Build ID:   simts_0.2.0.tar.gz-02f08e7585704be793448ef1baaacaf6
  Platform:   Ubuntu Linux 20.04.1 LTS, R-release, GCC
  Submitted:  35m 19s ago
  Build time: 16m 33.1s

> checking installed package size ... NOTE
    installed size is 42.4Mb
    sub-directories of 1Mb or more:
      doc    1.5Mb
      libs  40.0Mb

0 errors ✓ | 0 warnings ✓ | 1 note x

── simts 0.2.0: NOTE

  Build ID:   simts_0.2.0.tar.gz-8bede401bfab4a5ea851ed1b9bc55d71
  Platform:   Fedora Linux, R-devel, clang, gfortran
  Submitted:  35m 19s ago
  Build time: 18m 1.5s

> checking installed package size ... NOTE
    installed size is 23.8Mb
    sub-directories of 1Mb or more:
      doc    1.5Mb
      libs  21.4Mb

0 errors ✓ | 0 warnings ✓ | 1 note x

── simts 0.2.0: OK

  Build ID:   simts_0.2.0.tar.gz-9a9669db7c50414999345d00562249fa
  Platform:   Debian Linux, R-devel, GCC ASAN/UBSAN
  Submitted:  35m 19s ago
  Build time: 31m 15.5s

0 errors ✓ | 0 warnings ✓ | 0 notes ✓

```

* Standard CI workflow with GitHub actions (macOS, windows, ubuntu) `usethis::use_github_action("check-standard")` described [here](https://github.com/r-lib/actions/tree/master/examples).
  - MacOS-latest (release)
  - windows-latest (release)
  - ubuntu-latest (devel)
  - ubuntu-latest (release)
  - ubuntu-latest (oldrel-1)
  
  All builds pass successfully.

## R CMD check results

There were 2 NOTE:

* checking installed package size ... NOTE
  
It appears that within the Windows and Linux architectures, the CHECK procedure returns only one NOTE regarding the fact that the libs subdirectory is beyond the 1MB threshold. However, this NOTE doesn't occur to the OSX. Our understanding is that this size inflation of the libs subdirectory is due to the use of the Rcpp package. Indeed, some functions of the simts package have been written in C++ using Rcpp without which various functions would lose a considerable amount of computational efficiency leading to major parts of the package becoming impractical to use.

* checking for detritus in the temp directory ... NOTE


### additional requirements

On Ubuntu 20.04, Windows Server 2022 and potentially other sytems, there is an additional warning if the package `qpdf` (available [here](https://sourceforge.net/projects/qpdf/files/latest/download)) is not previously installed. This appears to be a common note for package check on Linux systems and once installed, the warning does not appears.


## Downstream dependencies

There are currently no downstream dependencies for this package.


## Copyright Issues

As a result of being the first R package submitted to CRAN by the maintainer and the authors, there have been some issues regarding the assignment of copyright holders and contributors to the package. Indeed there are functions in the files polyroot.cpp and sampler.cpp whose copyright belongs to authors that are not direct contributors to the package. Having tried to understand how to cite these authors in the package description (and not having found explicit indications) we have added these authors as copyrights holders of the package (and not contributors to the package). We are ready to edit this description in any way that is appropriate should we receive any clear indication as to how to cite these authors.

## Solaris Compatibility

The initial release of the package (version 0.1.0) reported some problems with Solaris due to C++ code compatibilty. Some functions were misused on an integer type, which may cause the potential overloading ambiguity. In the latest version, we hope that these issues have been overcome but we remain available to correct the package should other issues arise.
