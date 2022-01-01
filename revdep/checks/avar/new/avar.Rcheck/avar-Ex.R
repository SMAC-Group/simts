pkgname <- "avar"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
library('avar')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
base::assign(".old_wd", base::getwd(), pos = 'CheckExEnv')
cleanEx()
nameEx("MOAV")
### * MOAV

flush(stderr()); flush(stdout())

### Name: MOAV
### Title: Non-stationary Maximal-overlapping Allan Variance
### Aliases: MOAV

### ** Examples




cleanEx()
nameEx("NOAV")
### * NOAV

flush(stderr()); flush(stdout())

### Name: NOAV
### Title: Non-stationary Non-overlapping Allan Variance
### Aliases: NOAV

### ** Examples





cleanEx()
nameEx("av_ar1")
### * av_ar1

flush(stderr()); flush(stdout())

### Name: av_ar1
### Title: Calculate Theoretical Allan Variance for Stationary First-Order
###   Autoregressive (AR1) Process
### Aliases: av_ar1

### ** Examples

av1 = av_ar1(n = 5, phi = 0.9, sigma2 = 1)
av2 = av_ar1(n = 8, phi = 0.5, sigma2 = 2)



cleanEx()
nameEx("av_dr")
### * av_dr

flush(stderr()); flush(stdout())

### Name: av_dr
### Title: Calculate Theoretical Allan Variance for Drift Process
### Aliases: av_dr

### ** Examples

av1 = av_dr(delta = 1, n = 5)
av2 = av_dr(delta = 2, n = 8)



cleanEx()
nameEx("av_qn")
### * av_qn

flush(stderr()); flush(stdout())

### Name: av_qn
### Title: Calculate Theoretical Allan Variance for Stationary Quantization
###   Noise Process
### Aliases: av_qn

### ** Examples

av1 = av_qn(Q2 = 1, n = 5)
av2 = av_qn(Q2 = 2, n = 8)



cleanEx()
nameEx("av_rw")
### * av_rw

flush(stderr()); flush(stdout())

### Name: av_rw
### Title: Calculate Theoretical Allan Variance for Random Walk Process
### Aliases: av_rw

### ** Examples

av1 = av_rw(omega2 = 1, n = 5)
av2 = av_rw(omega2 = 2, n = 8)



cleanEx()
nameEx("av_wn")
### * av_wn

flush(stderr()); flush(stdout())

### Name: av_wn
### Title: Calculate Theoretical Allan Variance for Stationary White Noise
###   Process
### Aliases: av_wn

### ** Examples

av1 = av_wn(sigma2 = 1, n = 5)
av2 = av_wn(sigma2 = 2, n = 8)



cleanEx()
nameEx("avar")
### * avar

flush(stderr()); flush(stdout())

### Name: avar
### Title: Compute the Empirical Allan Variance
### Aliases: avar avar.default avar.imu
### Keywords: internal

### ** Examples

set.seed(999)
Xt = rnorm(10000)
av_mat_mo = avar(Xt, type = "mo", freq = 100)
av_mat_tau = avar(Xt, type = "to")




cleanEx()
nameEx("avar_mo_cpp")
### * avar_mo_cpp

flush(stderr()); flush(stdout())

### Name: avar_mo_cpp
### Title: Compute Maximal-Overlap Allan Variance using Means
### Aliases: avar_mo_cpp
### Keywords: internal

### ** Examples




cleanEx()
nameEx("avar_to_cpp")
### * avar_to_cpp

flush(stderr()); flush(stdout())

### Name: avar_to_cpp
### Title: Compute Tau-Overlap Allan Variance
### Aliases: avar_to_cpp
### Keywords: internal

### ** Examples




cleanEx()
nameEx("avlr")
### * avlr

flush(stderr()); flush(stdout())

### Name: avlr
### Title: Computes the Allan Variance Linear Regression estimator
### Aliases: avlr avlr.default avlr.imu_avar

### ** Examples




cleanEx()
nameEx("covmat_ar1blocks")
### * covmat_ar1blocks

flush(stderr()); flush(stdout())

### Name: covmat_ar1blocks
### Title: Calculate Theoretical Covariance Matrix of AR(1) Blocks Process
### Aliases: covmat_ar1blocks

### ** Examples

covmat1 = covmat_ar1blocks(n_total = 1000, n_block = 10,
phi = 0.9, sigma2 = 1)
covmat2 = covmat_ar1blocks(n_total = 800, n_block = 20,
phi = 0.5, sigma2 = 2)



cleanEx()
nameEx("covmat_bi")
### * covmat_bi

flush(stderr()); flush(stdout())

### Name: covmat_bi
### Title: Calculate Theoretical Covariance Matrix of Bias-Instability
###   Process
### Aliases: covmat_bi

### ** Examples

covmat1 = covmat_bi(sigma2 = 1, n_total = 1000, n_block = 10)
covmat2 = covmat_bi(sigma2 = 2, n_total = 800, n_block = 20)



cleanEx()
nameEx("covmat_nswn")
### * covmat_nswn

flush(stderr()); flush(stdout())

### Name: covmat_nswn
### Title: Calculate Theoretical Covariance Matrix of Non-Stationary White
###   Noise Process
### Aliases: covmat_nswn

### ** Examples

covmat1 = covmat_nswn(sigma2 = 1, n_total = 1000)
covmat2 = covmat_nswn(sigma2 = 2, n_total = 800)



cleanEx()
nameEx("is.whole")
### * is.whole

flush(stderr()); flush(stdout())

### Name: is.whole
### Title: Integer Check
### Aliases: is.whole
### Keywords: internal

### ** Examples

is.whole(2.3)
is.whole(4)
is.whole(c(1,2,3))
is.whole(c(.4,.5,.6))
is.whole(c(7,.8,9))



cleanEx()
nameEx("plot.avar")
### * plot.avar

flush(stderr()); flush(stdout())

### Name: plot.avar
### Title: Plot Allan Deviation
### Aliases: plot.avar

### ** Examples




cleanEx()
nameEx("plot.avlr")
### * plot.avlr

flush(stderr()); flush(stdout())

### Name: plot.avlr
### Title: Plot the AVLR with the Allan Deviation
### Aliases: plot.avlr

### ** Examples




cleanEx()
nameEx("plot.imu_avar")
### * plot.imu_avar

flush(stderr()); flush(stdout())

### Name: plot.imu_avar
### Title: Plot Allan Deviation based on IMU Data
### Aliases: plot.imu_avar

### ** Examples

data("navchip_av")
plot(navchip_av)



cleanEx()
nameEx("plot.imu_avlr")
### * plot.imu_avlr

flush(stderr()); flush(stdout())

### Name: plot.imu_avlr
### Title: Plot the AVLR with the Allan Deviation for IMU
### Aliases: plot.imu_avlr

### ** Examples




cleanEx()
nameEx("print.avar")
### * print.avar

flush(stderr()); flush(stdout())

### Name: print.avar
### Title: Prints Allan Variance
### Aliases: print.avar

### ** Examples

set.seed(999)
Xt = rnorm(10000)
out = avar(Xt)
print(out)




cleanEx()
nameEx("print.avlr")
### * print.avlr

flush(stderr()); flush(stdout())

### Name: print.avlr
### Title: Print avlr object
### Aliases: print.avlr
### Keywords: internal

### ** Examples




cleanEx()
nameEx("print.imu_avlr")
### * print.imu_avlr

flush(stderr()); flush(stdout())

### Name: print.imu_avlr
### Title: Print imu_avlr object
### Aliases: print.imu_avlr
### Keywords: internal

### ** Examples




cleanEx()
nameEx("summary.avar")
### * summary.avar

flush(stderr()); flush(stdout())

### Name: summary.avar
### Title: Summary Allan Variance
### Aliases: summary.avar

### ** Examples

set.seed(999)
Xt = rnorm(10000)
out = avar(Xt)
summary(out)




### * <FOOTER>
###
cleanEx()
options(digits = 7L)
base::cat("Time elapsed: ", proc.time() - base::get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')
