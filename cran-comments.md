## Test environmentsfor version simts 0.2.1

### Local checks

`devtools::check()` on local Ubuntu 20.04: Focal Fossa install, R 4.2.1: 

── R CMD check results ────── simts 0.2.1 ────
Duration: 5m 34.7s

❯ checking installed package size ... NOTE
    installed size is 42.9Mb
    sub-directories of 1Mb or more:
      doc    1.5Mb
      libs  40.5Mb

0 errors ✔ | 0 warnings ✔ | 1 note ✖

R CMD check succeeded

### Github actions CI workflows

Standard CI workflow with GitHub actions (macOS, windows, ubuntu) implemented using `usethis::use_github_action("check-standard")` described [here](https://github.com/r-lib/actions/tree/master/examples).

Results on the following environments:
  - MacOS-latest (release)
  - ubuntu-latest (devel)
  - ubuntu-latest (oldrel-1)
  - ubuntu-latest (release)
  - windows-latest (release)
  
All builds pass successfully.

### Rhub checks

Checks on [`rhub`](https://r-hub.github.io/rhub/) with `rhub::check_for_cran()`:

── simts 0.2.1: OK

  Build ID:   simts_0.2.1.tar.gz-5bc498627123416ab7ffba89866539bd
  Platform:   Debian Linux, R-devel, GCC ASAN/UBSAN
  Submitted:  1h 2m 38.9s ago
  Build time: 49m 12.1s

0 errors ✔ | 0 warnings ✔ | 0 notes ✔

── simts 0.2.1: NOTE

  Build ID:   simts_0.2.1.tar.gz-7d0185ba29ef4615bb7e9e675af4642c
  Platform:   Ubuntu Linux 20.04.1 LTS, R-release, GCC
  Submitted:  1h 2m 38.9s ago
  Build time: 22m 38.1s

❯ checking installed package size ... NOTE
    installed size is 42.9Mb
    sub-directories of 1Mb or more:
      doc    1.5Mb
      libs  40.4Mb

0 errors ✔ | 0 warnings ✔ | 1 note ✖

── simts 0.2.1: NOTE

  Build ID:   simts_0.2.1.tar.gz-bdc5c82890674c6ba48b1a5c594f6f82
  Platform:   Fedora Linux, R-devel, clang, gfortran
  Submitted:  1h 2m 38.9s ago
  Build time: 22m 46.9s

❯ checking installed package size ... NOTE
    installed size is 24.4Mb
    sub-directories of 1Mb or more:
      doc    1.5Mb
      libs  22.0Mb

❯ checking HTML version of manual ... NOTE
  Skipping checking HTML validation: no command 'tidy' found
  Skipping checking math rendering: package 'V8' unavailable

0 errors ✔ | 0 warnings ✔ | 2 notes ✖

── simts 0.2.1: NOTE

  Build ID:   simts_0.2.1.tar.gz-e63ae251c38e44c6aced1876d198a548
  Platform:   Windows Server 2022, R-devel, 64 bit
  Submitted:  1h 2m 38.9s ago
  Build time: 12m 58.3s

❯ checking for detritus in the temp directory ... NOTE
  Found the following files/directories:
    'lastMiKTeXException'

0 errors ✔ | 0 warnings ✔ | 1 note ✖


### additional requirements

On Ubuntu 20.04, Windows Server 2022 and potentially other sytems, there is an additional warning if the package `qpdf` (https://sourceforge.net/projects/qpdf/files/latest/download) is not previously installed. This appears to be a common note for package check on Linux systems and once installed, the warning does not appears.

## Downstream dependencies

There are currently no downstream dependencies for this package.

## Reverse Dependencies

Reverse dependencies were checked with `revdepcheck` (https://github.com/r-lib/revdepcheck). See results in `revdep` and `revdep/cran.md`.

`R` console output `revdep_check(num_workers = 4)`:

```
> red_rev_dep_check = revdep_check(num_workers = 4)
── INIT ────────────────────────────────────── Computing revdeps ──
── INSTALL ────────────────────────────────────────── 2 versions ──
Installing CRAN version of simts
also installing the dependencies ‘colorspace’, ‘stringi’, ‘fansi’, ‘pkgconfig’, ‘digest’, ‘gtable’, ‘isoband’, ‘withr’, ‘cli’, ‘crayon’, ‘utf8’, ‘farver’, ‘labeling’, ‘lifecycle’, ‘munsell’, ‘R6’, ‘RColorBrewer’, ‘viridisLite’, ‘backports’, ‘ellipsis’, ‘generics’, ‘glue’, ‘rlang’, ‘stringr’, ‘tibble’, ‘ggplot2’, ‘tidyselect’, ‘vctrs’, ‘pillar’, ‘cpp11’, ‘Rcpp’, ‘scales’, ‘broom’, ‘dplyr’, ‘magrittr’, ‘purrr’, ‘tidyr’, ‘robcor’, ‘RcppArmadillo’

Installing DEV version of simts
Installing 39 packages: vctrs, utf8, rlang, lifecycle, fansi, ellipsis, crayon, cli, glue, purrr, withr, tibble, scales, isoband, gtable, digest, cpp11, tidyselect, magrittr, dplyr, pkgconfig, pillar, stringi, R6, generics, colorspace, Rcpp, ggplot2, tidyr, stringr, backports, viridisLite, RColorBrewer, munsell, labeling, farver, RcppArmadillo, robcor, broom
── CHECK ──────────────────────────────────────────────────────────────────────── 2 packages ──
✓ avar 0.1.1                             ── E: 0     | W: 0     | N: 0                         
✓ wv 0.1.1                               ── E: 0     | W: 0     | N: 2                         
OK: 2                                                                                        
BROKEN: 0
Total time: 6 min
── REPORT ─────────────────────────────────────────────────────────────────────────────────────
Writing summary to 'revdep/README.md'
Writing problems to 'revdep/problems.md'
Writing failures to 'revdep/failures.md'
Writing CRAN report to 'revdep/cran.md'
Warning message:
call dbDisconnect() when finished working with a connection 
```

output of `revdep_report_cran()`:

```
## revdepcheck results

We checked 2 reverse dependencies, comparing R CMD check results across CRAN and dev versions of this package.

 * We saw 0 new problems
 * We failed to check 0 packages
```


## Copyright Issues

As a result of being the first R package submitted to CRAN by the maintainer and the authors, there have been some issues regarding the assignment of copyright holders and contributors to the package. Indeed there are functions in the files polyroot.cpp and sampler.cpp whose copyright belongs to authors that are not direct contributors to the package. Having tried to understand how to cite these authors in the package description (and not having found explicit indications) we have added these authors as copyrights holders of the package (and not contributors to the package). We are ready to edit this description in any way that is appropriate should we receive any clear indication as to how to cite these authors.

## Solaris Compatibility

The initial release of the package (version 0.1.0) reported some problems with Solaris due to C++ code compatibilty. Some functions were misused on an integer type, which may cause the potential overloading ambiguity. In the latest version, we hope that these issues have been overcome but we remain available to correct the package should other issues arise.
