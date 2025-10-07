## Test environmentsfor version simts 0.2.3

### Local checks

`devtools::check()` on local Ubuntu 20.04

==> Rcpp::compileAttributes()

* Updated R/RcppExports.R

==> devtools::check()

══ Documenting ════════════════════════════════════
ℹ Updating simts documentation
ℹ Loading simts

══ Building ═══════════════════════════════════════
Setting env vars:
• CFLAGS    : -Wall -pedantic -fdiagnostics-color=always
• CXXFLAGS  : -Wall -pedantic -fdiagnostics-color=always
• CXX11FLAGS: -Wall -pedantic -fdiagnostics-color=always
• CXX14FLAGS: -Wall -pedantic -fdiagnostics-color=always
• CXX17FLAGS: -Wall -pedantic -fdiagnostics-color=always
• CXX20FLAGS: -Wall -pedantic -fdiagnostics-color=always
── R CMD build ────────────────────────────────────
✔  checking for file ‘/home/lionel/github_repo/simts/DESCRIPTION’ ...
─  preparing ‘simts’:
✔  checking DESCRIPTION meta-information ...
─  cleaning src
─  installing the package to build vignettes
✔  creating vignettes (3m 26.9s)
─  cleaning src
─  checking for LF line-endings in source and make files and shell scripts (578ms)
─  checking for empty or unneeded directories
─  building ‘simts_0.2.3.tar.gz’ (655ms)
   
══ Checking ═══════════════════════════════════════
Setting env vars:
• _R_CHECK_CRAN_INCOMING_USE_ASPELL_           : TRUE
• _R_CHECK_CRAN_INCOMING_REMOTE_               : FALSE
• _R_CHECK_CRAN_INCOMING_                      : FALSE
• _R_CHECK_FORCE_SUGGESTS_                     : FALSE
• _R_CHECK_PACKAGES_USED_IGNORE_UNUSED_IMPORTS_: FALSE
• NOT_CRAN                                     : true
── R CMD check ────────────────────────────────────
─  using log directory ‘/home/lionel/github_repo/simts.Rcheck’ (359ms)
─  using R version 4.5.1 (2025-06-13)
─  using platform: x86_64-pc-linux-gnu
─  R was compiled by
       gcc (Ubuntu 11.4.0-1ubuntu1~22.04) 11.4.0
       GNU Fortran (Ubuntu 11.4.0-1ubuntu1~22.04) 11.4.0
─  running under: Ubuntu 22.04.5 LTS
─  using session charset: UTF-8
─  using options ‘--no-manual --as-cran’ (533ms)
✔  checking for file ‘simts/DESCRIPTION’
─  checking extension type ... Package
─  this is package ‘simts’ version ‘0.2.3’
─  package encoding: UTF-8
✔  checking package namespace information ...
✔  checking package dependencies (1.5s)
✔  checking if this is a source package
✔  checking if there is a namespace
✔  checking for executable files (1.1s)
✔  checking for hidden files and directories ...
✔  checking for portable file names ...
✔  checking for sufficient/correct file permissions
─  checking whether package ‘simts’ can be installed ... [190s/190s] OK (3m 9.8s)
─  used C++ compiler: ‘g++ (Ubuntu 11.4.0-1ubuntu1~22.04.2) 11.4.0’
─  checking installed package size ... INFO
     installed size is 33.3Mb
     sub-directories of 1Mb or more:
       doc    1.5Mb
       libs  30.8Mb
✔  checking package directory ...
✔  checking for future file timestamps (1.2s)
✔  checking ‘build’ directory
✔  checking DESCRIPTION meta-information (908ms)
✔  checking top-level files
✔  checking for left-over files
✔  checking index information (402ms)
✔  checking package subdirectories (957ms)
✔  checking code files for non-ASCII characters ...
✔  checking R files for syntax errors ...
✔  checking whether the package can be loaded (880ms)
✔  checking whether the package can be loaded with stated dependencies (704ms)
✔  checking whether the package can be unloaded cleanly (746ms)
✔  checking whether the namespace can be loaded with stated dependencies (755ms)
✔  checking whether the namespace can be unloaded cleanly (848ms)
✔  checking loading without being on the library search path (889ms)
✔  checking dependencies in R code (1.8s)
✔  checking S3 generic/method consistency (883ms)
✔  checking replacement functions (697ms)
✔  checking foreign function calls (942ms)
─  checking R code for possible problems ... [11s/11s] OK (10.9s)
✔  checking Rd files (1s)
✔  checking Rd metadata ...
✔  checking Rd line widths (459ms)
✔  checking Rd cross-references (764ms)
✔  checking for missing documentation entries (888ms)
✔  checking for code/documentation mismatches (2.5s)
✔  checking Rd \usage sections (1.6s)
✔  checking Rd contents (537ms)
✔  checking for unstated dependencies in examples ...
✔  checking contents of ‘data’ directory ...
✔  checking data for non-ASCII characters ...
✔  checking LazyData
✔  checking data for ASCII and uncompressed saves ...
✔  checking line endings in C/C++/Fortran sources/headers ...
✔  checking line endings in Makefiles
✔  checking compilation flags in Makevars ...
✔  checking for GNU extensions in Makefiles ...
✔  checking for portable use of $(BLAS_LIBS) and $(LAPACK_LIBS)
✔  checking use of PKG_*FLAGS in Makefiles ...
✔  checking use of SHLIB_OPENMP_*FLAGS in Makefiles
✔  checking pragmas in C/C++ headers and code ...
✔  checking compilation flags used
✔  checking compiled code ...
✔  checking installed files from ‘inst/doc’ ...
✔  checking files in ‘vignettes’ ...
─  checking examples ... [13s/13s] OK (13.4s)
✔  checking for unstated dependencies in vignettes ...
✔  checking package vignettes ...
─  checking re-building of vignette outputs ... [11s/11s] OK (10.6s)
✔  checking for non-standard things in the check directory
✔  checking for detritus in the temp directory
   
   
── R CMD check results ─────────── simts 0.2.3 ────
Duration: 4m 11.5s

0 errors ✔ | 0 warnings ✔ | 0 notes ✔

R CMD check succeeded


### Github actions CI workflows

Standard CI workflow with GitHub actions (macOS, windows, ubuntu)

Results on the following environments:
  - MacOS-latest (release)
  - ubuntu-latest (devel)
  - ubuntu-latest (oldrel-1)
  - ubuntu-latest (release)
  - windows-latest (release)
  
All builds pass successfully.


see https://github.com/SMAC-Group/simts/actions/workflows/R-CMD-check.yaml

## Downstream dependencies

There are currently no downstream dependencies for this package.

## Copyright Issues

As a result of being the first R package submitted to CRAN by the maintainer and the authors, there have been some issues regarding the assignment of copyright holders and contributors to the package. Indeed there are functions in the files polyroot.cpp and sampler.cpp whose copyright belongs to authors that are not direct contributors to the package. Having tried to understand how to cite these authors in the package description (and not having found explicit indications) we have added these authors as copyrights holders of the package (and not contributors to the package). We are ready to edit this description in any way that is appropriate should we receive any clear indication as to how to cite these authors.

## Solaris Compatibility

The initial release of the package (version 0.1.0) reported some problems with Solaris due to C++ code compatibilty. Some functions were misused on an integer type, which may cause the potential overloading ambiguity. In the latest version, we hope that these issues have been overcome but we remain available to correct the package should other issues arise.
