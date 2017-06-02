## ---- echo = FALSE-------------------------------------------------------
library(simts)

## ------------------------------------------------------------------------
# Set seed for reproducibility
set.seed(1337)

# Number of observations
n = 1e4

# Generate a White Noise Process
wn = gen_gts(n, WN(sigma2 = 1)) 

# Generate a Quantization Noise
qn = gen_gts(n, QN(q2 = .5)) 

# Generate a Random Walk
rw = gen_gts(n, RW(gamma2 = .75)) 

# Generate a Drift
dr = gen_gts(n, DR(omega = 0.10)) 

# Generate an AR(1)
ar1 = gen_gts(n, AR1(phi = .9, sigma2 = .1)) 

# Generate an MA(1)
ma1 = gen_gts(n, MA1(theta = .3, sigma2 = .5)) 

# Generate an ARMA(2, 1)
arma11 = gen_gts(n, ARMA(ar = c(.9, -.5), ma = .2, sigma2 = 1))

# Generate an SARIMA(1,0,1)x(2,1,1)[12]
sarima = gen_gts(n, SARIMA(ar = 0.3, i = 0, ma = -0.27,
                        sar = c(-0.12, -0.2), si = 1, sma = -0.9,
                        sigma2 = 1.5, s = 12))

## ------------------------------------------------------------------------
# Generate a ARMA(2,1) + WN()  
arma_wn_model =
  ARMA(ar = c(0.9, -0.5), ma = 0.3, sigma2 = 1) + 
  WN(sigma = 4)
arma_wn_sim = gen_gts(n = n, model  = arma_wn_model)

# Generate a SARMA() + WN(2) 
sarma_wn_model = 
  SARMA(ar = 0, ma = 0, sar = 0.9, sma = 0, s = 10, sigma2 = 1) + 
  WN(sigma2 = 2)
sarma_wn_sim = gen_lts(n = n, model  = sarma_wn_model)

# Generate a SARMA() + WN(2) + MA(2) 
sarma_wn_ma_model = 
  SARMA(ar = 0.25, ma = 0, sar = 0.9, sma = 0, s = 10, sigma2 = 1) + 
  WN(sigma2 = 2) +
  MA(theta = c(0.1,-0.8), sigma2 = 2)
sarma_wn_ma_sim = gen_lts(n = n, model  = sarma_wn_ma_model)

## ---- fig.align='center', fig.height = 5, fig.width = 7.25---------------
# Plot simulation of SARIMA(1,0,1)x(2,1,1)[12]
plot(sarima)

# Plot simulation of ARMA(2,1) + WN()
plot(arma_wn_sim)

## ---- fig.align='center', fig.height = 7, fig.width = 7.25---------------
# Plot simulation of SARMA() + WN(2) 
plot(sarma_wn_sim)

# Plot simulation of SARMA() + WN(2) + MA(2) 
plot(sarma_wn_ma_sim)

