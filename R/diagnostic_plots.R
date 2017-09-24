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
    hist(resid_sd, probability = TRUE, col = "#BEBEBE7F",
         xlab = "Standardized Residuals", ylab = "Percent", 
         main = "Histogram")
    lines(density(resid_sd, kernel="gaussian"), col = "blue")
    lines(x_normal, dnorm(x_normal,0,1))
    
    legend("topright", 
           legend = c("Kernel", "Normal"), 
           text.col = c("blue", "black"),
           bty = "n",
           cex = 0.8)
  }
  
  if (type == "resid"){
    plot(resid, type="l",
         xlab = "Observation Number", ylab = "Residuals",
         main = "Residual Plot")
  }
  
  if (type == "both"){
    par(mfrow=c(1,2))
    
    # plot histogram
    hist(resid_sd, probability = TRUE, col = "#BEBEBE7F",
         xlab = "Standardized Residuals", ylab = "Percent", 
         main = "Histogram")
    lines(density(resid_sd, kernel="gaussian"), col = "blue")
    lines(x_normal, dnorm(x_normal,0,1))
    
    legend("topright", 
           legend = c("Kernel", "Normal"), 
           text.col = c("blue", "black"),
           bty = "n",
           cex = 0.8)
    
    # plot residual
    plot(resid, type="l",
         xlab = "Observation Number", ylab = "Residuals",
         main = "Residual Plot")
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
  
  # plot 1
  resid_plot(Xt, model, std = std, type = "resid")
  
  # plot 2
  fitted = as.numeric(Xt - res)
  plot(fitted, res, pch=16, col = "black",
       xlab = "Fitted Values",
       ylab = "Residuals")
  
  # plot 3
  resid_plot(Xt, model, std = TRUE, type = "hist")
  
  # plot 4
  qqnorm(res)
  qqline(res, col = "blue",lwd = 2)
  
  # plot 5
  plot(ACF(Xt))
  
  # plot 6
  plot(PACF(Xt))
  
  # plot 7
  plot(wvar(res), main = "Haar WVar Representation")
  sigma2 = rep(var(res), length(wvar(res)$scales))
  points(wvar(res)$scales, sigma2/as.numeric(wvar(res)$scales), col = "orange", pch=0, cex=2)
  lines(wvar(res)$scales, sigma2/as.numeric(wvar(res)$scales), col = "orange", lty = 1)
  
  # plot 8
  plot(diag_ljungbox(model, stop_lag = 20, stdres = std))
}
