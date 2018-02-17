set.seed(4)
Xt = gen_gts(300, AR(phi = c(0, 0, 0.8), sigma2 = 1))
model = arima(Xt, order = c(3,0,0), include.mean = T)
resid_plot(Xt, model, std = FALSE, type = "hist")

plot(Xt)

x_range = range(scales)
y_range = range(x)
xlab = name_time
ylab = name_ts
main = main
mar = c(5.1, 5.1, 1, 2.1)
add_axis_x = TRUE
add_axis_y = TRUE
col_box = "black"
col_grid = "grey95"
col_band = "grey95"
col_title = "black"
add_band = TRUE
title_band_width = 0.09
grid_lty = 1

x_range = x_range
y_range = y_range
my_hist = hist(resid_sd, plot = FALSE)
# make frame
x_range = range(my_hist$breaks) * 1.05
y_range = c(0, max(my_hist$counts/sum(my_hist$counts*diff(my_hist$breaks)[1])))*1.05
xlab = "Standardized Residuals"
ylab = "Percent"
main = "Residuals Histogram"
