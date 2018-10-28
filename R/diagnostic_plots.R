#######################
# resid_plot function
#######################
#' @title Plot the Distribution of (Standardized) Residuals
#' @description This function plots a histogram (with kernel density function and normal distribution) of the standardized
#' residuals or a basic plot the (standardized) residuals, or both.
#' @author Yuming Zhang
#' @param res A \code{vector} of residuals.
#' @param std A \code{boolean} indicating whether the residuals plot is for standardized
#' residuals or original residuals.
#' @param type  A \code{string} indicating either:
#' \code{"hist"} (standardized residual histogram with superimposed kernel density estimator and normal distribution), \code{"resid"} (standard residual plot),
#' or \code{"both"}
#' @param ...  Additional parameters
#' @importFrom stats sd
#' @importFrom graphics hist
#' @importFrom stats density
#' @importFrom stats sd
#' @importFrom stats dnorm
resid_plot = function(res, std = FALSE, type = "hist", ...){
  
  res_sd = res / sd(res)
  # standardize residuals or not
  if(std){
    res = res / sd(res)
  }
  
  # standard normal for comparison
  x_normal = seq(-5, 5, by=0.01)
  
  if (type == "hist"){
    my_hist = hist(res_sd, plot = FALSE)
    # make frame
    x_range = range(my_hist$breaks) * 1.05
    y_range = c(0, max(my_hist$counts/sum(my_hist$counts*diff(my_hist$breaks)[1])))*1.05
    make_frame(x_range, y_range, 
               xlab = "Standardized Residuals", ylab = "Frequency",
               main = "Residuals Histogram")
    
    # plot histogram
    hist(res_sd, probability = TRUE, col = "#BEBEBE7F", labels = FALSE, add = TRUE)
    lines(density(res_sd, kernel="gaussian"), col = "blue")
    lines(x_normal, dnorm(x_normal,0,1))
    
    
    custom_legend("topright", legend = c("Kernel", "Normal"), 
                  text.col = c("blue", "black"),
                  bty = "n")
    
  }
  
  if (type == "resid"){
    # make frame
    x_range = c(1, length(res))
    y_range = c(min(res), max(res))*1.05
    make_frame(x_range, y_range, xlab = "Observation Number", ylab = "Residuals",
               main = "Residuals Plot")
    # plotting
    lines(res, col = "blue4")
  }
  
  if (type == "both"){
    par(mfrow=c(1,2))
    
    # ----- plot histogram
    my_hist = hist(res_sd, plot = FALSE)
    # make frame
    x_range = range(my_hist$breaks) * 1.05
    y_range = c(0, max(my_hist$counts/sum(my_hist$counts*diff(my_hist$breaks)[1])))*1.05
    make_frame(x_range, y_range, 
               xlab = "Standardized Residuals", ylab = "Percent",
               main = "Residuals Histogram")
    
    # plot histogram
    hist(res_sd, probability = TRUE, col = "#BEBEBE7F", labels = FALSE, add = TRUE)
    lines(density(res_sd, kernel="gaussian"), col = "blue")
    lines(x_normal, dnorm(x_normal,0,1))
    
    
    custom_legend("topright", legend = c("Kernel", "Normal"), 
                   text.col = c("blue", "black"),
                   bty = "n")
    
    # ----- residual plot
    # make frame
    x_range = c(1, length(res))
    y_range = c(min(res), max(res))*1.05
    make_frame(x_range, y_range, xlab = "Observation Number", ylab = "Residuals",
               main = "Residual Plot")
    # plotting
    lines(res, col = "blue4")
    
  }
}



#' @title Basic Diagnostic Plot of Residuals
#' @description This function will plot four diagnostic plots to assess how well the model fits 
#' the data. These plots are: (1) residuals plot, (2) histogram of 
#' (standardized) residuals, (3) normal Q-Q plot of residuals and (4) residuals vs fitted values plot.
#' @author Yuming Zhang
#' @param Xt The original time series data.
#' @param model The \code{arima} model fit to the data.
#' @param std A \code{boolean} indicating whether we use standardized residuals for the 
#' (1) residuals plot and the (2) histogram of (standardized) residuals.
#' @importFrom graphics points
#' @importFrom stats qqnorm
#' @importFrom stats qqline
#' @importFrom stats var
#' @importFrom stats resid
#' @importFrom stats na.omit
simple_diag_plot = function(Xt, model, std = FALSE){
  par(mfrow = c(2,2))
  
  # extract residuals 
  if(!is.null(model)){
    if (class(model) == "fitsimts"){
      if (model$method == "gmwm" | model$method == "rgmwm"){
        res = predict(model$mod, model$Xt)$resid
      }else{
        res = model$mod$resid
      }
      xx = na.omit(Xt - res)
      res = na.omit(res)
    }else{
      res = resid(model)
      xx = na.omit(Xt - res)
    }
  }
  
  
  # ----- plot 1
  resid_plot(res, std = std, type = "resid")
  
  # ----- plot 2
  my_hist = hist(res, plot = FALSE)
  # make frame
  x_range = range(my_hist$breaks) * 1.05
  y_range = c(0, max(my_hist$counts/sum(my_hist$counts*diff(my_hist$breaks)[1])))*1.05
  
  if (std==TRUE){
    xlab = "Standardized Residuals"
  }else{
    xlab = "Residuals"
  }
  
  make_frame(x_range, y_range, 
             xlab = xlab, ylab = "Frequency",
             main = "Residuals Histogram")
  
  # plot histogram
  hist(res, probability = TRUE, col = "#BEBEBE7F", labels = FALSE, add = TRUE)
  
  
  # ----- plot 3
  my_qqnorm = qqnorm(res, plot.it = FALSE)
  # make frame
  x_range = c(min(my_qqnorm$x), max(my_qqnorm$x))*1.05
  y_range = c(min(my_qqnorm$y), max(my_qqnorm$y)*1.02)*1.2
  make_frame(x_range, y_range, 
             xlab = "Theoretical Quantiles", 
             ylab = "Sample Quantiles",
             main = "Normal Q-Q Plot")
  
  # add qq plots
  points(my_qqnorm$x, my_qqnorm$y, pch = 16, col = "grey50")
  qqline(res, col = "blue2",lwd = 2)
  
  # Plot 4: Residuals vs Fitted
  x_range = range(xx)*1.05
  y_range = range(res)*1.05
  make_frame(x_range, y_range, 
             xlab = "Fitted values", 
             ylab = "Residuals",
             main = "Residuals vs Fitted")
  points(xx, res, col = "blue4")
  
  par(mfrow = c(1,1))
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
#' @param model A \code{fitsimts}, \code{lm} or \code{gam} object. 
#' @param resids A \code{vector} of residuals for diagnostics. 
#' @param std A \code{boolean} indicating whether we use standardized residuals for 
#' (1) residuals plot and (8) Box test results.
#' @importFrom stats na.omit
diag_plot = function(Xt = NULL, model = NULL, resids = NULL, std = FALSE){
  par(mfrow = c(2,3))
  
  # extract residuals 
  if(!is.null(model)){
    if (class(model) == "fitsimts"){
      if (model$method == "gmwm" | model$method == "rgmwm"){
        res = predict(model$mod, model$Xt)$resid
      }else{
        res = model$mod$resid
      }
      xx = na.omit(Xt - res)
      res = na.omit(res)
    }else{
      res = resid(model)
      xx = na.omit(Xt - res)
    }
  }
  
  if(!is.null(resids)){
    res = resids
  }
  
  
  # plot 1
  resid_plot(res, std = std, type = "resid")
  
  # plot 2
  resid_plot(res, std = TRUE, type = "hist")
  
  # plot 3
  my_qqnorm = qqnorm(res, plot.it = FALSE)
  # make frame
  x_range = c(min(my_qqnorm$x), max(my_qqnorm$x))*1.05
  y_range = c(min(my_qqnorm$y), max(my_qqnorm$y)*1.02)*1.2
  make_frame(x_range, y_range, 
             xlab = "Theoretical Quantiles", 
             ylab = "Sample Quantiles",
             main = "Normal Q-Q Plot")
  
  # add qq plots
  points(my_qqnorm$x, my_qqnorm$y, pch = 16, col = "grey50")
  qqline(res, col = "blue2",lwd = 2)
  
  # plot 4
  plot(auto_corr(res))
  
  # plot 5
  plot(auto_corr(res, pacf = TRUE))
  
  # plot 6
  stop_lag = 20
  if (stop_lag >= 0.6*length(res)){ stop_lag = round(0.6*length(res)) }
  
  object = diag_ljungbox(as.numeric(res), order = 0, stop_lag = stop_lag, stdres = std, plot = FALSE)
  maxval = max(object$pvalue)
  
  x_range = c(min(object$lag), max(object$lag))*1.05
  y_range = c(0, max(object$pvalue))*1.05
  make_frame(x_range, y_range, 
             xlab = "Lag", ylab = "P-value", main = "Ljung-Box Test Result")
  
  points(object$lag, object$pvalue, pch = 16, cex = 1.25, col = "blue4")
  lines(object$lag, object$pvalue, lty = 3, col = "blue4")
  abline(h = 0.05, col = "blue2", lty = 2)
  par(mfrow = c(1,1))
}