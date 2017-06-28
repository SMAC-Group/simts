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

