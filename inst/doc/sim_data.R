## ---- echo=FALSE, message=FALSE, warning=FALSE---------------------------
library(simts)
library(datasets)

## ---- fig.align='center', fig.height = 4, fig.width = 7.25, fig.cap = 'Figure 1: Monthly  precipitation  series  from  1907  to  1972  taken  from @hipel1994time'----
# Load hydro dataset
data("hydro")

# Simulate based on data
hydro = gts(as.vector(hydro), start = 1907, freq = 12, unit_ts = "in.", 
            name_ts = "Precipitation", data_name = "Precipitation data from Hipel et al., (1994)")

# Plot hydro simulation
plot(hydro)

## ---- fig.align='center', fig.height = 4, fig.width = 7.25, fig.cap = 'Figure 2: Empirical autocorrelation function of monthly  precipitation  series  from @hipel1994time'----
# Compute ACF and plot result
hydro_acf = ACF(hydro)
plot(hydro_acf)

## ---- fig.align='center', fig.height = 4, fig.width = 7.25, fig.cap = 'Figure 3: Monthly  (seasonally  adjusted)  Personal Saving  Rates data  from  January  1959  to  May  2015  provided  by  the Federal  Reserve  Bank  of  St.  Louis.'----
# Load savingrt dataset
data("savingrt")
# Simulate based on data
savingrt = gts(as.vector(savingrt), start = 1959, freq = 12, unit_ts = "%", 
            name_ts = "Saving Rates", data_name = "US Personal Saving Rates")
# Plot savingrt simulation
plot(savingrt)

## ---- fig.align='center', fig.height = 4, fig.width = 7.25, fig.cap = 'Figure 4: Empirical autocorrelation function of Personal Saving  Rates data'----
# Compute ACF and plot result
savingrt_acf = ACF(savingrt)
plot(savingrt_acf)

## ---- fig.align='center', fig.height = 4, fig.width = 7.25, fig.cap = 'Figure 1: Monthly Clothing Retail Sales in US for 1992-2016'----
# Load sales dataset
data("sales")

# Simulate based on data
sales = gts(as.vector(sales), start = 1992, freq = 12, name_time = "Month",
            unit_ts = "millions of dollars", name_ts = "Retail.trade.clothing", 
            data_name = "Monthly Clothing Retail Sales in US for 1992-2016")


# Plot sales simulation
plot(sales)

## ---- fig.align='center', fig.height = 4, fig.width = 7.25, fig.cap = 'Figure 2: Empirical autocorrelation function of Monthly Clothing Retail Sales in US for 1992-2016'----
# Compute ACF and plot result
sales_acf = ACF(sales)
plot(sales_acf)

## ---- fig.align='center', fig.height = 4, fig.width = 7.25, fig.cap = 'Figure 3: Empirical partial autocorrelation function of Monthly Clothing Retail Sales in US for 1992-2016'----
#Compute PACF and plot result
sales_pacf = PACF(sales)
plot(sales_pacf)

## ---- fig.align='center', fig.height = 4, fig.width = 7.25, fig.cap = 'Figure 4: Empirical ACF and PACF of Monthly Clothing Retail Sales in US for 1992-2016'----
# Compute and plot ACF and PACF
sales_corr = corr_analysis(sales)

# Get ACF and PACF values
sales_acf = sales_corr$ACF
sales_pacf = sales_corr$PACF

## ---- fig.align='center', fig.height = 4, fig.width = 7.25, fig.cap = 'Figure 5: Plot of Annual Nile river flow from 1871-1970'----
# Load Nile dataset
Nile = datasets::Nile

# Simulate based on data
nile = gts(as.vector(Nile), start = 1871, end = 1970, freq = 1, 
           unit_ts = bquote(paste(10^8," ",m^3)), name_ts = "Flow", 
           name_time = "Years", data_name = "Annual Flow of the Nile River")

# Plot Nile simulation 
plot(nile)

## ---- fig.align='center', fig.height = 4, fig.width = 7.25, fig.cap = 'Figure 6: Empirical autocorrelation function of the Nile river flow data'----
# Compute ACF and plot result
nile_acf = ACF(nile)
plot(nile_acf)

