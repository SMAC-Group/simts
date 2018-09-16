#######################
# resid_plot function
#######################
#' @title Plot the Distribution of (Standardized) Residuals
#' @description This function plots a histogram (with kernel density function and normal distribution) of the standardized
#' residuals or a basic plot the (standardized) residuals, or both.
#' @author Yuming Zhang
#' @param Xt The original time series data.
#' @param model The \code{arima} model fit to the data. 
#' @param std A \code{boolean} indicating whether the residuals plot is for standardized
#' residuals or original residuals.
#' @param type     A \code{string} indicating either:
#' \code{"hist"} (standardized residual histogram with superimposed kernel density estimator and normal distribution), \code{"resid"} (standard residual plot),
#' or \code{"both"}
#' @export
#' @importFrom stats sd
#' @importFrom graphics hist
#' @importFrom stats density
#' @importFrom stats sd
#' @importFrom stats dnorm
#' @examples 
#' Xt = gen_gts(300, AR(phi = c(0, 0, 0.8), sigma2 = 1))
#' model = arima(Xt, order = c(3,0,0), include.mean = TRUE)
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
               xlab = "Standardized Residuals", ylab = "Frequency",
               main = "Residuals Histogram")
    
    # plot histogram
    hist(resid_sd, probability = TRUE, col = "#BEBEBE7F", labels = FALSE, add = TRUE)
    lines(density(resid_sd, kernel="gaussian"), col = "blue")
    lines(x_normal, dnorm(x_normal,0,1))
    
    
    custom_legend("topright", legend = c("Kernel", "Normal"), 
                  text.col = c("blue", "black"),
                  bty = "n")
    
  }
  
  if (type == "resid"){
    # make frame
    x_range = c(1, length(resid))
    y_range = c(min(resid), max(resid))*1.05
    make_frame(x_range, y_range, xlab = "Observation Number", ylab = "Residuals",
               main = "Residuals Plot")
    # plotting
    lines(resid, col = "blue4")
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
    
    
    custom_legend("topright", legend = c("Kernel", "Normal"), 
                   text.col = c("blue", "black"),
                   bty = "n")
    
    # ----- residual plot
    # make frame
    x_range = c(1, length(resid))
    y_range = c(min(resid), max(resid))*1.05
    make_frame(x_range, y_range, xlab = "Observation Number", ylab = "Residuals",
               main = "Residual Plot")
    # plotting
    lines(resid, col = "blue4")
    
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
#' @export
#' @importFrom graphics points
#' @importFrom stats qqnorm
#' @importFrom stats qqline
#' @importFrom stats var
#' @importFrom stats resid
#' @examples 
#' Xt = gen_gts(300, AR(phi = c(0, 0, 0.8), sigma2 = 1))
#' model = arima(Xt, order = c(3,0,0), include.mean = TRUE)
#' simple_diag_plot(Xt, model)
simple_diag_plot = function(Xt, model, std = FALSE){
  par(mfrow = c(2,2))
  
  # extract residuals 
  res = resid(model)
  
  # ----- plot 1
  resid_plot(Xt, model, std = std, type = "resid")
  
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
  y_range = c(min(my_qqnorm$y), max(my_qqnorm$y)*1.02)*1.05
  make_frame(x_range, y_range, 
             xlab = "Theoretical Quantiles", 
             ylab = "Sample Quantiles",
             main = "Normal Q-Q Plot")
  
  # add qq plots
  points(my_qqnorm$x, my_qqnorm$y)
  qqline(res, col = "blue",lwd = 2)
  
  # Plot 4: Residuals vs Fitted
  xx = Xt - res
  
  x_range = range(xx)*1.05
  y_range = range(res)*1.05
  make_frame(x_range, y_range, 
             xlab = "Fitted values", 
             ylab = "Residuals",
             main = "Residuals vs Fitted")
  points(xx, res, col = "blue4")
  
  par(mfrow = c(1,1))
  
}






