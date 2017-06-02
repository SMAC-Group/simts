################### Hydrology Data

hydro = read.csv("data-raw/mean-monthly-precipitation-1907-.csv", header=T, sep=";")

hydro = ts(as.numeric(hydro[,2][!is.na(hydro[,2])]), start = 1907, end = 1972, frequency = 12)

devtools::use_data(hydro)


################### Personal Saving Rate Data
savingrt = read.csv("data-raw/PSAVERT.csv", sep=",")

savingrt = gts(savingrt$PSAVERT, start = 1959, freq = 12,
               name_ts = 'US Personal Saving Rates', unit_time = "month")

devtools::use_data(savingrt)

################### Nile Data
nile = datasets::Nile

devtools::use_data(nile)
