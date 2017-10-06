#######################
# resid_plot function
#######################
#' @title Plot the Distribution of (Standardized) Residuals
#' @description This function can plot a histogram of the distribution of standardized
#' residuals, or plot the (standardized) residuals plot, or both.
#' @author Yuming Zhang
#' @param Xt The data used to construct said model.
#' @param model The \code{arima} model used to fit the data. 
#' @param std A \code{boolean} indicating whether the residuals plot is for standardized
#' residuals or original residuals.
#' @param type     A \code{string} indicating either:
#' \code{"hist"} (standardized residual histogram), \code{"resid"} (residual plot),
#' or \code{"both"}
#' @export
#' @examples 
#' Xt = gen_gts(300, AR(phi = c(0, 0, 0.8), sigma2 = 1))
#' model = arima(Xt, order = c(3,0,0), include.mean = T)
#' 
#' resid_plot(Xt, model, type = "hist")
#' resid_plot(Xt, model, type = "resid")
#' resid_plot(Xt, model, std = TRUE, type = "both")
resid_plot = function(Xt, model, std = FALSE, type = "hist", ...){
  # obtain residuals
  resid = resid(model)
  resid_sd = resid / sd(resid)
  
  # standardize residuals or not
  if(std){
    resid = resid / sd(resid)
  }
  
  # standard normal for comparison
  x_normal = seq(-5, 5, by=0.01)
  
  if (type == "hist"){
    my_hist = hist(resid_sd, plot = FALSE)
    # make frame
    x_range = range(my_hist$breaks) * 1.05
    y_range = c(0, max(my_hist$counts/sum(my_hist$counts*diff(my_hist$breaks)[1])))*1.05
    make_frame(x_range, y_range, 
               xlab = "Standardized Residuals", ylab = "Percent",
               main = "Residuals Histogram")
    
    # plot histogram
    hist(resid_sd, probability = TRUE, col = "#BEBEBE7F", labels = FALSE, add = TRUE)
    lines(density(resid_sd, kernel="gaussian"), col = "blue")
    lines(x_normal, dnorm(x_normal,0,1))
    
    legend("topright", 
           legend = c("Kernel", "Normal"), 
           text.col = c("blue", "black"),
           bty = "n",
           cex = 0.8)
  }
  
  if (type == "resid"){
    # make frame
    x_range = c(1, length(resid))
    y_range = c(min(resid), max(resid))*1.05
    make_frame(x_range, y_range, xlab = "Observation Number", ylab = "Residuals",
               main = "Residual Plot")
    # plotting
    lines(resid)
  }
  
  if (type == "both"){
    par(mfrow=c(1,2))
    
    # ----- plot histogram
    my_hist = hist(resid_sd, plot = FALSE)
    # make frame
    x_range = range(my_hist$breaks) * 1.05
    y_range = c(0, max(my_hist$counts/sum(my_hist$counts*diff(my_hist$breaks)[1])))*1.05
    make_frame(x_range, y_range, 
               xlab = "Standardized Residuals", ylab = "Percent",
               main = "Residuals Histogram")
    
    # plot histogram
    hist(resid_sd, probability = TRUE, col = "#BEBEBE7F", labels = FALSE, add = TRUE)
    lines(density(resid_sd, kernel="gaussian"), col = "blue")
    lines(x_normal, dnorm(x_normal,0,1))
    
    legend("topright", 
           legend = c("Kernel", "Normal"), 
           text.col = c("blue", "black"),
           bty = "n",
           cex = 0.8)
    
    # ----- residual plot
    # make frame
    x_range = c(1, length(resid))
    y_range = c(min(resid), max(resid))*1.05
    make_frame(x_range, y_range, xlab = "Observation Number", ylab = "Residuals",
               main = "Residual Plot")
    # plotting
    lines(resid)
    
  }
}


#######################
# diag_plot function
#######################
#' @title Diagnostic Plot of Residuals
#' @description This function will plot 8 diagnostic plots to assess the model used to 
#' fit the data. These include: (1) residuals plot, (2) residuals vs fitted values, 
#' (3) histogram of distribution of standardized residuals, (4) Normal Q-Q plot of 
#' residuals, (5) ACF plot, (6) PACF plot, (7) Haar Wavelet Variance Representation,
#' (8) Box test results.
#' @author Yuming Zhang
#' @param Xt The data used to construct said model.
#' @param model The \code{arima} model used to fit the data. 
#' @param std A \code{boolean} indicating whether we use standardized residuals for 
#' (1) residuals plot and (8) Box test results.
#' @export
#' @examples 
#' Xt = gen_gts(300, AR(phi = c(0, 0, 0.8), sigma2 = 1))
#' model = arima(Xt, order = c(3,0,0), include.mean = T)
#' diag_plot(Xt, model)
diag_plot = function(Xt, model, std = FALSE){
  par(mfrow = c(2,4))
  
  # extract residuals 
  res = resid(model)
  
  # ----- plot 1
  resid_plot(Xt, model, std = std, type = "resid")
  
  # ----- plot 2
  fitted = as.numeric(Xt - res)
  # make frame
  x_range = c(min(fitted), max(fitted)) * 1.05
  y_range = c(min(res), max(res)) * 1.05
  make_frame(x_range, y_range, 
             xlab = "Fitted Values", ylab = "Residuals",
             main = "Residuals vs Fitted Values")
  # add points
  points(fitted, res, pch=16, col = "black")
  
  # ----- plot 3
  resid_plot(Xt, model, std = TRUE, type = "hist")
  
  # ----- plot 4
  my_qqnorm = qqnorm(res, plot.it = FALSE)
  # make frame
  x_range = c(min(my_qqnorm$x), max(my_qqnorm$x))*1.05
  y_range = c(min(my_qqnorm$y), max(my_qqnorm$y))*1.05
  make_frame(x_range, y_range, 
             xlab = "Theoretical Quantiles", 
             ylab = "Sample Quantiles",
             main = "Normal Q-Q Plot")
  
  # add qq plots
  points(my_qqnorm$x, my_qqnorm$y)
  qqline(res, col = "blue",lwd = 2)
  
  # ----- plot 5
  plot(ACF(Xt), col_ci = "#00BBDC33")
  
  # ----- plot 6
  plot(PACF(Xt), col_ci = "#00BBDC33")
  
  # ----- plot 7
  plot(wvar(res), main = "Haar WVar Representation", legend_position = NA)
  
  sigma2 = rep(var(res), length(wvar(res)$scales))
  points(wvar(res)$scales, sigma2/as.numeric(wvar(res)$scales), col = "orange", pch=0, cex=2)
  lines(wvar(res)$scales, sigma2/as.numeric(wvar(res)$scales), col = "orange", lty = 1)
  
  # add legend
  if (wvar(res)$robust == TRUE){
    wv_title_part1 = "Empirical Robust WV "
  }else{
    wv_title_part1 = "Empirical WV "
  }
  
  CI_conf = 1 - wvar(res)$alpha
  
  legend("bottomleft",
         legend = c(as.expression(bquote(paste(.(wv_title_part1), hat(nu)^2))), 
                    as.expression(bquote(paste("CI(",hat(nu)^2,", ",.(CI_conf),")"))),
                    "WV implied by WN"),
         pch = c(16, 15, 0), lty = c(1, NA, 1), col = c("darkblue", hcl(h = 210, l = 65, c = 100, alpha = 0.2), "orange"), 
         cex = 1, pt.cex = c(1.25, 3, 1.25), bty = "n")
  
  
  # ----- plot 8
  object = diag_ljungbox(model, stop_lag = 20, stdres = std)
  
  # make frame
  x_range = c(min(object$lag), max(object$lag))*1.05
  y_range = c(0, max(object$pvalue))*1.05
  make_frame(x_range, y_range, 
             xlab = "Lag", ylab = "P-value", main = "Ljung-Box Results")
  
  # add plotting
  points(object$lag, object$pvalue, pch = 16)
  lines(object$lag, object$pvalue, lty = 3)
  abline(h = 0.05, col = "blue", lty = 2)

}


