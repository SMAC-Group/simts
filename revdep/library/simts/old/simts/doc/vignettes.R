## ---- echo = FALSE-------------------------------------------------------
library(simts)

## ------------------------------------------------------------------------
# Set seed for reproducibility
set.seed(1337)

# Number of observations
n = 10^4

# Generate a White Noise Process
wn = gen_gts(n, WN(sigma2 = 1)) 

# Generate a Quantization Noise
qn = gen_gts(n, QN(q2 = .5)) 

# Generate a Random Walk
rw = gen_gts(n, RW(gamma2 = .75)) 

## ---- fig.align='center', fig.height = 11, fig.width = 7.25, fig.cap = 'Figure 1: Simulated white noise process (top panel), quantiation noise (middle panel) and random walk process (bottom panel)'----
par(mfrow = c(3,1))
plot(wn)
plot(qn)
plot(rw)

## ---- fig.align='center', fig.height = 4, fig.width = 7.25, fig.cap = 'Figure 2: Simulated SARIMA(1,0,1)x(2,1,1)[12] process'----
# Generate an SARIMA(1,0,1)x(2,1,1)[12]
sarima = gen_gts(n, SARIMA(ar = 0.3, i = 0, ma = -0.27,
                        sar = c(-0.12, -0.2), si = 1, sma = -0.9,
                        sigma2 = 1.5, s = 12))
# Plot simulation of SARIMA(1,0,1)x(2,1,1)[12]
plot(sarima)

## ---- fig.align='center', fig.height = 4, fig.width = 7.25, fig.cap = 'Figure 3: Simulated state-space model (RW + WN + DR)'----
set.seed(1)
model = RW(gamma2 = 0.01) + DR(omega = 0.001) + WN(sigma2 = 1)
Yt = gen_gts(model, n = 10^3)
plot(Yt)

## ---- fig.align='center', fig.height = 11, fig.width = 7.25, fig.cap = 'Figure 4: Simulated state-space model (RW + WN + DR) showing latent processes'----
set.seed(1)
model = RW(gamma2 = 0.01) + DR(omega = 0.001) + WN(sigma2 = 1)
Yt = gen_lts(model, n = 10^3)
plot(Yt)

## ---- fig.align='center', fig.height = 4, fig.width = 7.25, fig.cap = 'Figure 5: Simulated ARMA(2,1) + WN() process'----
# Generate a ARMA(2,1) + WN()  
arma_wn_model =
  ARMA(ar = c(0.9, -0.5), ma = 0.3, sigma2 = 1) + 
  WN(sigma = 4)
arma_wn_sim = gen_gts(n = n, model  = arma_wn_model)

# Plot simulation of ARMA(2,1) + WN()
plot(arma_wn_sim)

## ---- fig.align='center', fig.height = 7, fig.width = 7.25, fig.cap = 'Figure 6: Simulated SARMA(1,0) x (0,1) + WN(2) process with a breakdown of the underlying latent processes'----
# Generate a SARMA() + WN() 
sarma_wn_model = 
  SARMA(ar = 0, ma = 0, sar = 0.98, sma = 0, s = 10, sigma2 = 1) + 
  WN(sigma2 = 1)
sarma_wn_sim = gen_lts(n = 10^3, model = sarma_wn_model)

# Plot simulation of SARMA() + WN() 
plot(sarma_wn_sim)

## ---- fig.align='center', fig.height = 7, fig.width = 7.25, fig.cap = 'Figure 7: Simulated SARMA(1,0) x (0,1) + WN(2) process with a breakdown of the underlying latent processes'----
plot(sarma_wn_sim, fixed_range = TRUE)

## ---- echo=FALSE, message=FALSE, warning=FALSE---------------------------
library(datasets)

## ---- fig.align='center', fig.height = 4, fig.width = 7.25, fig.cap = 'Figure 1: Monthly  precipitation  series  from  1907  to  1972  taken  from @hipel1994time'----
# Load hydro dataset
data("hydro")

# Simulate based on data
hydro = gts(as.vector(hydro), start = 1907, freq = 12, unit_ts = "in.", 
            unit_time = "year", name_ts = "Precipitation", 
            data_name = "Precipitation data from Hipel et al., (1994)")

# Plot hydro simulation
plot(hydro)

## ---- fig.align='center', fig.height = 4, fig.width = 7.25, fig.cap = 'Figure 2: Standard and Robust Empirical autocorrelation functions of monthly  precipitation  series  from @hipel1994time'----
# Compare the standard and robust ACF
compare_acf(hydro)

## ------------------------------------------------------------------------
model_hydro = estimate(AR(2), hydro, method = "rgmwm")
model_hydro$mod$estimate

## ---- fig.align='center', fig.height = 4, fig.width = 7.25, fig.cap = 'Figure 3: Monthly  (seasonally  adjusted)  Personal Saving  Rates data  from  January  1959  to  May  2015  provided  by  the Federal  Reserve  Bank  of  St.  Louis.'----
# Load savingrt dataset
data("savingrt")

# Simulate based on data
savingrt = gts(as.vector(savingrt), start = 1959, freq = 12, unit_ts = "%", 
            name_ts = "Saving Rates", data_name = "US Personal Saving Rates",
            unit_time = "year")

# Plot savingrt simulation
plot(savingrt)

## ---- fig.align='center', fig.height = 4, fig.width = 7.25, fig.cap = 'Figure 4: Empirical autocorrelation function of Personal Saving  Rates data'----
# Compute ACF and plot result
savingrt_acf = auto_corr(savingrt)
plot(savingrt_acf)

## ------------------------------------------------------------------------
# Estimate the latent model ARMA(2,1) + RW()
model_savings = estimate(ARMA(2,1) + RW(), savingrt, method = "gmwm")

## ---- fig.align='center', fig.height = 4, fig.width = 7.25, fig.cap = 'Figure 5: Monthly Clothing Retail Sales in US for 1992-2016'----
# Load sales dataset
data("sales")

# Simulate based on data
sales = gts(as.vector(sales), start = 1992, freq = 12, unit_time = "year",
            unit_ts = bquote(paste(10^6,"$")), name_ts = "Retail Sales", 
            data_name = "Monthly Clothing Retail Sales in US for 1992-2016")

# Plot sales simulation
plot(sales)

## ---- fig.align='center', fig.height = 4, fig.width = 7.25, fig.cap = 'Figure 6: Empirical autocorrelation function of Monthly Clothing Retail Sales in US for 1992-2016'----
# Compute ACF and plot result
sales_acf = auto_corr(sales)
plot(sales_acf)

## ---- fig.align='center', fig.height = 4, fig.width = 7.25, fig.cap = 'Figure 7: Empirical partial autocorrelation function of Monthly Clothing Retail Sales in US for 1992-2016'----
#Compute PACF and plot result
sales_pacf = auto_corr(sales, pacf = TRUE)
plot(sales_pacf)

## ---- fig.align='center', fig.height = 4, fig.width = 7.25, fig.cap = 'Figure 8: Empirical ACF and PACF of Monthly Clothing Retail Sales in US for 1992-2016'----
# Compute and plot ACF and PACF
sales_corr = corr_analysis(sales)

# Get ACF and PACF values
sales_acf = sales_corr$ACF
sales_pacf = sales_corr$PACF

## ---- fig.align='center', fig.height = 4, fig.width = 7.25, fig.cap = 'Figure 9: Plot of Annual numbers of lynx trappings for 1821-1934 in Canada'----
# Load lynx dataset 
data(lynx)

# Simulate based on data
lynx = gts(as.vector(lynx), start = 1821, end = 1934, freq = 1, 
           unit_ts = bquote(paste(10^8," ",m^3)), name_ts = "Numbers", 
           unit_time = "year", data_name = "Annual Numbers of Lynx Trappings")

# Plot lynx simulation
plot(lynx)

## ---- fig.align='center', fig.height = 4, fig.width = 7.25, fig.cap = 'Figure 10: Empirical autocorrelation function of Annual numbers of lynx trappings in Canada for 1821-1934'----
# Compute ACF and plot result
lynx_acf = auto_corr(lynx)
plot(lynx_acf)

## ---- fig.align='center', fig.height = 4, fig.width = 7.25, fig.cap = 'Figure 11: Empirical partial autocorrelation function of Annual numbers of lynx trappings in Canada for 1821-1934'----
# Compute PACF and plot result
lynx_pacf = auto_corr(lynx, pacf = TRUE)
plot(lynx_pacf)

## ---- fig.align='center', fig.height = 4, fig.width = 7.25, fig.cap = 'Figure 12: Empirical partial autocorrelation function of Annual numbers of lynx trappings in Canada for 1821-1934'----
test = estimate(SARMA(2,2,1), lynx)
check(test)

## ---- fig.align='center', fig.height = 4, fig.width = 7.25, fig.cap = 'Figure 13: Plot of Yearly numbers of sunspots from 1700 to 1988'----
# Load sunspot dataset 
sunspot = datasets::sunspot.year

# Simulate based on data
sunspot = gts(as.vector(sunspot.year), start = 1700, end = 1988, freq = 1, 
           unit_ts = bquote(paste(10^8," ",m^3)), name_ts = "Numbers", 
           unit_time = "year", data_name = "Yearly Numbers of Sunspots")

# Plot sunspot simulation
plot(sunspot)

## ---- fig.align='center', fig.height = 4, fig.width = 7.25, fig.cap = 'Figure 14: Empirical autocorrelation function of Yearly numbers of sunspots from 1700 to 1988'----
# Compute ACF and plot result
sunspot_acf = auto_corr(sunspot)
plot(sunspot_acf)

## ---- fig.align='center', fig.height = 4, fig.width = 7.25, fig.cap = 'Figure 15: Empirical partial autocorrelation function of Yearly numbers of sunspots from 1700 to 1988'----
#Compute PACF and plot result
sunspot_pacf = auto_corr(sunspot, pacf = TRUE)
plot(sunspot_pacf)

## ---- fig.align='center', fig.height = 4, fig.width = 7.25, fig.cap = 'Figure 16: Results on model selection for AR(p) models for Yearly numbers of sunspots from 1700 to 1988'----
select(AR(15), sunspot)

## ---- fig.align='center', fig.height = 4, fig.width = 7.25, fig.cap = 'Figure 17: 10-steps-ahead forecasts (and confidence intervals) for Yearly numbers of sunspots from 1700 to 1988'----
model_sunspots = estimate(AR(9), sunspot)
predict(model_sunspots, n.ahead = 10, level = c(0.60, 0.90, 0.95))

## ---- fig.align='center', fig.height = 4, fig.width = 7.25, fig.cap = 'Figure 18: Plot of Annual Nile river flow from 1871-1970'----
# Load Nile dataset
Nile = datasets::Nile

# Simulate based on data
nile = gts(as.vector(Nile), start = 1871, end = 1970, freq = 1, 
           unit_ts = bquote(paste(10^8," ",m^3)), name_ts = "Flow", 
           unit_time = "year", data_name = "Annual Flow of the Nile River")

# Plot Nile simulation 
plot(nile)

## ---- fig.align='center', fig.height = 4, fig.width = 7.25, fig.cap = 'Figure 19: Empirical autocorrelation function of the Nile river flow data'----
# Compute ACF and plot result
nile_acf = auto_corr(nile)
plot(nile_acf)

## ---- fig.align='center', fig.height = 4, fig.width = 7.25, fig.cap = 'Figure 20: Empirical ACF and PACF of the Nile river flow data'----
# Compute and plot ACF and PACF
nile_corr = corr_analysis(nile)

# Get ACF and PACF values
nile_acf = nile_corr$ACF
nile_pacf = nile_corr$PACF

