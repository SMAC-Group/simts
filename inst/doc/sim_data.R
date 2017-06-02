## ---- echo = FALSE-------------------------------------------------------
library(simts)
library(datasets)

## ---- fig.align='center', fig.height = 4, fig.width = 7.25, fig.cap = 'Figure 1: Plot of the Hydrology data from 1907'----
# Load hydro dataset
data("hydro")
# Simulate based on data
hydro = gts(as.vector(hydro), start = 1907, freq = 12, unit_ts = "in.", 
            name_ts = "Precipitation", data_name = "Hydrology data")
# Plot hydro simulation
plot(hydro)

## ---- fig.align='center', fig.height = 4, fig.width = 7.25, fig.cap = 'Figure 2: Plot of Personal Savings as DPI from 1959'----
# Load savingrt dataset
data("savingrt")
# Simulate based on data
savingrt = gts(as.vector(savingrt), start = 1959, freq = 12, unit_ts = "%", 
            name_ts = "Saving Rates", data_name = "US Personal Saving Rates")
# Plot savingrt simulation
plot(savingrt)

## ---- fig.align='center', fig.height = 4, fig.width = 7.25, fig.cap = 'Figure 3: Plot of Annual Nile river flow from 1871-1970'----
# Load Nile dataset
Nile = datasets::Nile
# Simulate based on data
nile = gts(as.vector(Nile), start = 1871, end = 1970, freq = 1, 
           unit_ts = bquote(paste(10^8," ",m^3)), name_ts = "Flow", 
           name_time = "Years", data_name = "Annual Flow of river Nile")
# Plot Nile simulation 
plot(nile)

