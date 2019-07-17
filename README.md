
<!-- README.md is generated from README.Rmd. Please edit that file -->

[![Travis-CI Build
Status](https://travis-ci.org/SMAC-Group/simts.svg?branch=master)](https://travis-ci.org/SMAC-Group/simts)
[![AppVeyor Build
Status](https://ci.appveyor.com/api/projects/status/github/tidyverse/dplyr?branch=master&svg=true)](https://ci.appveyor.com/project/stephaneguerrier/simts)
[![Project Status:
Active](http://www.repostatus.org/badges/latest/active.svg)](http://www.repostatus.org/#active)
[![Licence](https://img.shields.io/badge/licence-AGPL--3.0-blue.svg)](https://opensource.org/licenses/AGPL-3.0)
[![minimal R
version](https://img.shields.io/badge/R%3E%3D-3.4.0-6666ff.svg)](https://cran.r-project.org/)
[![CRAN](http://www.r-pkg.org/badges/version/simts)](https://cran.r-project.org/package=simts)
[![packageversion](https://img.shields.io/badge/Package%20version-0.1.0-orange.svg?style=flat-square)](https://github.com/SMAC-Group/simts/blob/master/DESCRIPTION)
[![Last-changedate](https://img.shields.io/badge/last%20change-2019--07--17-yellowgreen.svg)](https://github.com/SMAC-Group/simts)

# `simts` Overview <a href="https://smac-group.com/"><img src="man/figures/logo.png" align="right" style="width: 20%; height: 20%"/></a>

The Time Series Tools (`simts`) R package provides a series of tools to
simulate, plot, estimate, select and forecast different time series
models. It is originally conceived as a support to the online textbook
[“Applied Time Series Analysis with
R”](https://smac-group.github.io/ts/) and, more specifically, provides
tools with the following features:

  - Simulation of time series from SARIMA models to various state-space
    models that can be expressed as latent time series processes.
  - Visualization of time series data with user specifications.
  - Specific simulation and visualization tools for latent time series
    models.
  - Easy-to-use functions to estimate and infer on the parameters of
    time series models through different methods (standard and robust).
  - Diagnostic and statistical tools to assess goodness of fit and
    select the best model for the data.
  - Estimating and plotting tools to deliver point forecasts and
    confidence intervals.

To understand the usage of the `simts` package, please refer to the
“Vignettes” tab above.

## Install Instructions

To install the `simts` package, there is currently one option:
[GitHub](https://github.com/SMAC-Group/simts/). For users who are
interested in having the latest developments, this option is ideal
although more dependencies are required to run a stable version of the
package. Most importantly, users **must** have a (C++) compiler
installed on their machine that is compatible with R (e.g. Clang).

*The setup to obtain the development version of `simts` is platform
dependent.*

**All Systems**

The following R packages are also required. Once you’ve made sure that
you have a compatible C++ compiler installed on your computer, run the
following code in an R session and you will be ready to use the
devlopment version of `simts`.

``` r
# Install dependencies
install.packages(c("RcppArmadillo","devtools","knitr","rmarkdown"))

# Install the package from GitHub without Vignettes/User Guides
devtools::install_github("SMAC-Group/simts")

# Install the package with Vignettes/User Guides 
devtools::install_github("SMAC-Group/simts", build_vignettes = TRUE)
```

## License

The license this source code is released under is the GNU AFFERO GENERAL
PUBLIC LICENSE (AGPL) v3.0. Please see the LICENSE file for full text.
Otherwise, please consult [TLDR
Legal](https://tldrlegal.com/license/gnu-affero-general-public-license-v3-\(agpl-3.0\))
or [GNU](https://www.gnu.org/licenses/agpl-3.0.en.html) which will
provide a synopsis of the restrictions placed upon the
code.

<!-- ### Requirements and Dependencies -->

<!-- **OS X** -->

<!-- Some users report the need to use X11 to suppress shared library errors. To install X11, visit [xquartz.org](http://www.xquartz.org/). -->

<!-- **Linux** -->

<!-- Both curl and libxml are required. -->

<!-- For **Debian** systems, enter the following in terminal: -->

<!-- ```{r, eval = F, engine='bash'} -->

<!-- sudo apt-get install curl libcurl3 libcurl3-dev libxml2 libxml2-dev -->

<!-- ``` -->

<!-- For **RHEL** systems, enter the following in terminal: -->

<!-- ```{r, eval = F, engine='bash'} -->

<!-- sudo yum install curl curl-devel libxml2 libxml2-dev -->

<!-- ``` -->
