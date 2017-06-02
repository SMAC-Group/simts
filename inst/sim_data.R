## ---- echo = FALSE-------------------------------------------------------
library(simts)
library(datasets)

## ------------------------------------------------------------------------
# Load hydro dataset
data("hydro")
# Simulate based on data
hydro = gts(as.vector(hydro), start = 1907, freq = 12, unit_ts = "in.", 
            name_ts = "Precipitation", data_name = "Hydrology data")

# Load savingrt dataset
data("savingrt")
# Simulate based on data
savingrt = gts(as.vector(savingrt), start = 1959, freq = 12, unit_ts = "%", 
            name_ts = "Saving Rates", data_name = "US Personal Saving Rates")

# Load Nile dataset
data("Nile")
nile = gts(as.vector(Nile), start = 1871, end = 1970, freq = 1, 
           unit_ts = bquote(paste(10^8," ",m^3)), name_ts = "Flow", 
           name_time = "Years", data_name = "Annual Flow of river Nile")

## ---- fig.align='center', fig.height = 5, fig.width = 7.25---------------
# Plot hydro simulation
plot(hydro)

# Plot savingrt simulation
plot(savingrt)

# Plot Nile simulation 
plot(nile)

