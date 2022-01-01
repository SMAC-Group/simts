pkgname <- "wv"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
library('wv')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
base::assign(".old_wd", base::getwd(), pos = 'CheckExEnv')
cleanEx()
nameEx("ACF")
### * ACF

flush(stderr()); flush(stdout())

### Name: ACF
### Title: Auto-Covariance and Correlation Functions
### Aliases: ACF

### ** Examples

# Get Autocorrelation
m = ACF(datasets::AirPassengers)

# Get Autocovariance and do not remove trend from signal
m = ACF(datasets::AirPassengers, cor = FALSE, demean = FALSE)



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
nameEx("compare_wvar")
### * compare_wvar

flush(stderr()); flush(stdout())

### Name: compare_wvar
### Title: Comparison Between Multiple Wavelet Variances
### Aliases: compare_wvar

### ** Examples

set.seed(999)
n = 10^4
Xt = arima.sim(n = n, list(ar = 0.10))
Yt = arima.sim(n = n, list(ar = 0.35))
Zt = arima.sim(n = n, list(ar = 0.70))
Wt = arima.sim(n = n, list(ar = 0.95))

wv_Xt = wvar(Xt)
wv_Yt = wvar(Yt)
wv_Zt = wvar(Zt)
wv_Wt = wvar(Wt)

compare_wvar(wv_Xt, wv_Yt, wv_Zt, wv_Wt)



cleanEx()
nameEx("dwt")
### * dwt

flush(stderr()); flush(stdout())

### Name: dwt
### Title: Discrete Wavelet Transform
### Aliases: dwt

### ** Examples

set.seed(999)
x = rnorm(2^8)
ret = dwt(x)

summary(ret)

plot(ret)



cleanEx()
nameEx("modwt")
### * modwt

flush(stderr()); flush(stdout())

### Name: modwt
### Title: Maximum Overlap Discrete Wavelet Transform
### Aliases: modwt

### ** Examples

set.seed(999)
x = rnorm(100)
ret = modwt(x)

summary(ret)

plot(ret)



cleanEx()
nameEx("plot.auto_corr")
### * plot.auto_corr

flush(stderr()); flush(stdout())

### Name: plot.auto_corr
### Title: Auto-Covariance and Correlation Functions
### Aliases: plot.auto_corr
### Keywords: internal

### ** Examples

# Calculate the Autocorrelation
m = ACF(datasets::AirPassengers)

# Plot with 95% CI
plot(m) 

# Plot with 90% CI
plot(m, ci = 0.90) 

# Plot without 95% CI
plot(m, show.ci = FALSE)



cleanEx()
nameEx("plot.dwt")
### * plot.dwt

flush(stderr()); flush(stdout())

### Name: plot.dwt
### Title: Plot Discrete Wavelet Transform
### Aliases: plot.dwt
### Keywords: internal

### ** Examples

# Simulate a Gaussian white noise
n = 10^3
Xt = rnorm(n)

# dwt
Yt = dwt(Xt)

# Graph examples
plot(Yt)
plot(Yt, index = c(1,4,5,6,8,2))
plot(Yt, index = c(1,4,5,6), couleur = "blue")
plot(Yt, index = c(1,4,5,6), couleur = rep(c("blue","yellow"),2))



cleanEx()
nameEx("plot.imu_wvar")
### * plot.imu_wvar

flush(stderr()); flush(stdout())

### Name: plot.imu_wvar
### Title: Plot Wavelet Variance based on IMU Data
### Aliases: plot.imu_wvar
### Keywords: internal

### ** Examples

data("kvh1750_wv")
plot(kvh1750_wv)



cleanEx()
nameEx("plot.modwt")
### * plot.modwt

flush(stderr()); flush(stdout())

### Name: plot.modwt
### Title: Plot Maximum Overlap Discrete Wavelet Transform
### Aliases: plot.modwt
### Keywords: internal

### ** Examples

# Simulate a Gaussian white noise
n = 10^3
Xt = rnorm(n)

# MODWT
Yt = modwt(Xt)

# Graph examples
plot(Yt)
plot(Yt, index = c(1,4,5,6,8,2))
plot(Yt, index = c(1,4,5,6), couleur = "blue")
plot(Yt, index = c(1,4,5,6), couleur = rep(c("blue","yellow"),2))



cleanEx()
nameEx("plot.wccv_pair")
### * plot.wccv_pair

flush(stderr()); flush(stdout())

### Name: plot.wccv_pair
### Title: Plot Cross Covariance Pair
### Aliases: plot.wccv_pair
### Keywords: internal

### ** Examples

n = 10^5
Xt = cumsum(rnorm(n, 0, 0.01))
Wt = Xt + rnorm(n)
Yt = Xt + rnorm(n)
wcov = wccv_pair(Wt, Yt)
plot(wcov)



cleanEx()
nameEx("plot.wvar")
### * plot.wvar

flush(stderr()); flush(stdout())

### Name: plot.wvar
### Title: Plot Wavelet Variance
### Aliases: plot.wvar
### Keywords: internal

### ** Examples

set.seed(999)
n = 10^4
Xt = rnorm(n)
wv = wvar(Xt)
plot(wv)
plot(wv, main = "Simulated white noise", xlab = "Scales")
plot(wv, units = "sec", legend_position = "topright")
plot(wv, col_wv = "darkred", col_ci = "pink")



cleanEx()
nameEx("print.dwt")
### * print.dwt

flush(stderr()); flush(stdout())

### Name: print.dwt
### Title: Print Discrete Wavelet Transform
### Aliases: print.dwt
### Keywords: internal

### ** Examples

set.seed(999)
x = rnorm(2^8)
print(dwt(x))



cleanEx()
nameEx("print.modwt")
### * print.modwt

flush(stderr()); flush(stdout())

### Name: print.modwt
### Title: Print Maximum Overlap Discrete Wavelet Transform
### Aliases: print.modwt
### Keywords: internal

### ** Examples

set.seed(999)
x = rnorm(100)
print(modwt(x))



cleanEx()
nameEx("print.wvar")
### * print.wvar

flush(stderr()); flush(stdout())

### Name: print.wvar
### Title: Print Wavelet Variances
### Aliases: print.wvar
### Keywords: internal

### ** Examples

set.seed(999)
x = rnorm(100)
out = wvar(x)
print( out )



cleanEx()
nameEx("robust_eda")
### * robust_eda

flush(stderr()); flush(stdout())

### Name: robust_eda
### Title: Comparison between classical and robust Wavelet Variances
### Aliases: robust_eda

### ** Examples

set.seed(999)
n = 10^4
Xt = rnorm(n)
wv = wvar(Xt)

plot(wv)
plot(wv, main = "Simulated white noise", xlab = "Scales")
plot(wv, units = "sec", legend_position = "topright")
plot(wv, col_wv = "darkred", col_ci = "pink")



cleanEx()
nameEx("summary.dwt")
### * summary.dwt

flush(stderr()); flush(stdout())

### Name: summary.dwt
### Title: Summary Discrete Wavelet Transform
### Aliases: summary.dwt
### Keywords: internal

### ** Examples

set.seed(999)
x = rnorm(2^8)
summary(dwt(x))



cleanEx()
nameEx("summary.modwt")
### * summary.modwt

flush(stderr()); flush(stdout())

### Name: summary.modwt
### Title: Summary Maximum Overlap Discrete Wavelet Transform
### Aliases: summary.modwt
### Keywords: internal

### ** Examples

set.seed(999)
x = rnorm(100)
summary(modwt(x))



cleanEx()
nameEx("summary.wvar")
### * summary.wvar

flush(stderr()); flush(stdout())

### Name: summary.wvar
### Title: Summary of Wavelet Variances
### Aliases: summary.wvar
### Keywords: internal

### ** Examples

set.seed(999)
x = rnorm(100)
ret = wvar(x)
summary(ret)



cleanEx()
nameEx("unitConversion")
### * unitConversion

flush(stderr()); flush(stdout())

### Name: unitConversion
### Title: Convert Unit of Time Series Data
### Aliases: unitConversion
### Keywords: internal

### ** Examples

x = seq(60, 3600, 60)
unitConversion(x, 'sec', 'min')
y = 1:10
unitConversion(y, 'hour', 'sec')



cleanEx()
nameEx("wccv_get_y")
### * wccv_get_y

flush(stderr()); flush(stdout())

### Name: wccv_get_y
### Title: Mapping to log10 scale
### Aliases: wccv_get_y

### ** Examples

x = 2^(-1:-9)
y.min = floor(min(log10(abs(x))))
y.step = 2
wccv_get_y(x, y.min, y.step)



cleanEx()
nameEx("wvar")
### * wvar

flush(stderr()); flush(stdout())

### Name: wvar
### Title: Wavelet Variance
### Aliases: wvar wvar.lts wvar.gts wvar.ts wvar.imu wvar.default

### ** Examples

set.seed(999)
x = rnorm(100)

# Default
wvar(x)

# Robust
wvar(x, robust = TRUE, eff=0.3)

# Classical
wvar(x, robust = FALSE, eff=0.3)

# 90% Confidence Interval 
wvar(x, alpha = 0.10)



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
