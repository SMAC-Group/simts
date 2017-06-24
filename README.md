
<!-- README.md is generated from README.Rmd. Please edit that file -->
[![Travis-CI Build Status](https://travis-ci.org/SMAC-Group/simts.svg?branch=master)](https://travis-ci.org/SMAC-Group/simts)

`simts` Overview <a href="https://smac-group.com/"><img src="man/figures/logo.png" align="right" style="width: 20%; height: 20%"/></a>
======================================================================================================================================

The Time Series Simulation (`simts`) R package generates various time series objects for use in other packages, in which includes:

-   Simulation of time series simulations from SARIMA models and various state-space models that can be expressed as latent time series processes.
-   Visualize time series data with user specifications.
-   Visualize latent time series processes.

To see what `simts` is capable of, please refer to the vignettes.

Install Instructions
--------------------

To install the `simts` package, there is currently one option: [GitHub](https://github.com/SMAC-Group/simts/).

### Installing the package through GitHub

For users who are interested in having the latest developments, this option is ideal. Though, more dependancies are required to run a stable version of the package. Most importantly, users **must** have a compiler installed on their machine that is compatible with R (e.g. Clang).

*The setup to obtain the development version of `simts` is platform dependent.*

### Requirements and Dependencies

**OS X**

Some users report the need to use X11 to suppress shared library errors. To install X11, visit [xquartz.org](http://www.xquartz.org/).

**Linux**

Both curl and libxml are required.

For **Debian** systems, enter the following in terminal:

``` bash
sudo apt-get install curl libcurl3 libcurl3-dev libxml2 libxml2-dev
```

For **RHEL** systems, enter the following in terminal:

``` bash
sudo yum install curl curl-devel libxml2 libxml2-dev
```

**All Systems**

The following R packages are also required. If you have made it this far, run the following code in an R session and you will be ready to use the devlopment version of `simts`.

``` r
# Install dependencies
install.packages(c("RcppArmadillo","devtools","knitr","rmarkdown"))

# Install the package from GitHub without Vignettes/User Guides
devtools::install_github("SMAC-Group/simts")

# Install the package with Vignettes/User Guides 
devtools::install_github("SMAC-Group/simts", vignette = TRUE)
```
