################################
### Theoretical ACF / PACF
################################

# hidden casting function
cast_acf = function(object, n, name_ = "Empirical", type = "Autocorrelation",
                    class = "theo_arma"){
  
  # Force to array
  if(is.null(nrow(object))){ dim(object) = c(length(object), 1, 1)}
  
  # Make pretty names
  ids = seq_len(nrow(object))
  if(type == "Autocorrelation" || type == "Autocovariance"){
    ids = ids - 1
  }
  
  if("PACF" %in% class){ ids = seq_len(nrow(object)) }
  
  dimnames(object)  = list(ids, name_, name_)
  
  structure(object, type = type, n = n, class = class)
}

# Work horse of the two functions for theoretical ACF & PACF
theo_arma_ = function(model, lagmax = 20, pacf = FALSE){
  
  if(!is.ts.model(model)){ stop("`model` must be a `ts.model` object.")}
  if(model$starting){ stop("`model` must have specific parameter values.")}
  if(length(model$desc) != 1 && model$desc != "SARIMA"){ stop("`model` must contain only 1 process.")}
  
  
  objdesc = model$objdesc[[1]]
  
  ar = model$theta[model$process.desc == "AR"]
  ma = model$theta[model$process.desc == "MA"]
  
  if(is.null(lagmax)){ lagmax = max(length(ar), length(ma) + 1) + 2}
  
  if(pacf == TRUE){
    myclass = c("PACF", "array")
  }else{
    myclass = c("simtsACF", "array")
  }
  
  cast_acf(ARMAacf(ar = ar, ma = ma, lag.max = lagmax, pacf = pacf),
           n = lagmax,
           name_ = "Theoretical",
           type = "Autocorrelation",
           class = myclass)
}

#' @title Theoretical Autocorrelation (ACF) of an ARMA process
#' @description This function computes the theoretical Autocorrelation (ACF) of an ARMA process.
#' @param ar A \code{vector} containing the AR coefficients.
#' @param ma A \code{vector} containing the MA coefficients.
#' @param lagmax An \code{integer} indicating the maximum lag up to which to compute the theoretical ACF.
#' @author Yuming Zhang
#' @importFrom stats ARMAacf
#' @examples
#' # Compute the theoretical ACF for an ARMA(1,0) (i.e. a first-order autoregressive model: AR(1))
#' theo_acf(ar = -0.25, ma = NULL)
#' # Computes the theoretical ACF for an ARMA(2, 1)
#' theo_acf(ar = c(.50, -0.25), ma = 0.20, lagmax = 10)
#' @export
theo_acf = function(ar, ma = NULL, lagmax = 20){
  model = ARMA(ar=ar, ma=ma)
  theo_arma_(model = model, lagmax = lagmax, pacf = FALSE)
}


#' @title Theoretical Partial Autocorrelation (PACF) of an ARMA process
#' @description This function computes the theoretical Partial Autocorrelation (PACF) of an ARMA process.
#' @param ar A \code{vector} containing the AR coefficients.
#' @param ma A \code{vector} containing the MA coefficients.
#' @param lagmax An \code{integer} indicating the maximum lag up to which to compute the theoretical PACF.
#' @author Yuming Zhang
#' @export
#' @examples
#' # Computes the theoretical ACF for an ARMA(1,0) (i.e. a first-order autoregressive model: AR(1))
#' theo_pacf(ar = -0.25, ma = NULL, lagmax = 7)
#' # Computes the theoretical ACF for an ARMA(2, 1)
#' theo_pacf(ar = c(.50, -0.25), ma = .20, lagmax = 10)
theo_pacf = function(ar, ma = NULL, lagmax = 20){
  model = ARMA(ar=ar, ma=ma)
  theo_arma_(model = model, lagmax = lagmax, pacf = TRUE)
}



################################
### Empirical ACF / PACF
################################

# ----- wrapper function

#' @title Empirical ACF and PACF
#' @description This function can estimate either the autocovariance / autocorrelation for univariate time series,
#' or the partial autocovariance / autocorrelation for univariate time series.
#' @author Yuming Zhang
#' @param x       A \code{vector} or \code{ts} object (of length \eqn{N > 1}).
#' @param lag.max An \code{integer} indicating the maximum lag up to which to compute the empirical ACF / PACF.
#' @param pacf    A \code{boolean} indicating whether to output the PACF. 
#' If it's \code{TRUE}, then the function will only estimate the empirical PACF. If it's \code{FALSE} (the default),
#' then the function will only estimate the empirical ACF. 
#' @param type   A \code{character} string giving the type of acf to be computed. Allowed values are "correlation" (the default) and "covariance".
#' @param demean A \code{boolean} indicating whether the data should be detrended (\code{TRUE}) or not (\code{FALSE}). Defaults to \code{TRUE}.
#' @param robust A \code{boolean} indicating whether a robust estimator should be used (\code{TRUE}) or not (\code{FALSE}). Defaults to \code{FALSE}.
#' This only works when the function is estimating ACF.
#' @return An \code{array} of dimensions \eqn{N \times 1 \times 1}{N x 1 x 1}.
#' @details 
#' \code{lagmax} default is \eqn{10*log10(N/m)} where \eqn{N} is the number of
#' observations and \eqn{m} is the number of time series being compared. If 
#' \code{lagmax} supplied is greater than the number of observations N, then one
#' less than the total will be taken (i.e. N - 1).
#' @importFrom stats acf pacf mad
#' @importFrom robcor robacf
#' @export
#' 
#' @examples 
#' m = auto_corr(datasets::AirPassengers)
#' m = auto_corr(datasets::AirPassengers, pacf = TRUE)
auto_corr = function(x, lag.max = NULL, pacf = FALSE, type = "correlation", demean = TRUE, robust = FALSE){
  if(pacf == FALSE){
    ACF(x, lag.max = lag.max, type = type, demean = demean, robust = robust)
  }else{
    PACF(x, lag.max = lag.max, type = type, demean = demean)
  }
}


# ------ empirical ACF
ACF = function(x, lag.max = NULL, type = "correlation", demean = TRUE, robust = FALSE){
  
  # Change the data to matrix form
  if(is.ts(x) || is.atomic(x)){
    x2 = data.matrix(x)        
  }
  
  if (type != "correlation" && type != "covariance"){
    stop("Type not authorized. Allowed values correlation and covariance.")
  }
  
  if (demean){
    x3 = as.matrix(x)
    x3 = sweep(x3, 2, colMeans(x3, na.rm = TRUE), check.margin = FALSE)
  }
  
  # Get the acf value of the data
  if (robust == FALSE){
    acfe = acf(x3, lag.max = lag.max, type = type, plot = FALSE)
  }else{
    if (type == "correlation"){
      acfe = robacf(x3, lag.max = lag.max, plot = FALSE)
    }else{
      sig2 = mean(abs(x3 - mean(x3)))*sqrt(pi/2)
      acfe = sig2*robacf(x3, lag.max = lag.max, plot = FALSE)
    }
  }
  
  acfe = acfe$acf
  
  # Get the data name 
  varName = deparse(substitute(x))
  
  # Adjust the name for data 
  dimnames(acfe)  = list(seq_len(nrow(acfe))-1, "ACF", varName)
  
  if (is.null(attr(x, "data_name"))){
    acfe = structure(acfe, n = nrow(x2), class = c("simtsACF", "array"))
  }else{
    acfe = structure(acfe, n = nrow(x2), data_name = attr(x, "data_name"), class = c("simtsACF", "array"))
  }
  
  unit_time = attr(x, "unit_time")
  if (!is.null(unit_time)){
    attr(acfe, "unit_time") = unit_time
  }
  
  if(!is.null(names(x))){
    attr(acfe, "data_name") = names(x)
  }
  acfe
}

#' @title Plot Auto-Covariance and Correlation Functions
#' @description The function plots the output of the \code{theo_acf} and \code{auto_corr} functions (autocovariance or autocorrelation functions).
#' @author Yunxiang Zhang, Stéphane Guerrier and Yuming Zhang
#' @param x         An \code{"ACF"} object output from \code{theo_acf} and \code{auto_corr}.
#' @param xlab         A \code{string} indicating the label of the x axis: the default name is 'Lags'.
#' @param ylab     A \code{string} indicating the label of the y axis: the default name is 'ACF'.
#' @param show.ci   A \code{bool} indicating whether to show the confidence region. Defaults to \code{TRUE}.
#' @param alpha     A \code{double} indicating the level of significance for the confidence interval. By default \code{alpha = 0.05} which gives a 1 - \code{alpha} = 0.95 confidence interval. 
#' @param col_ci    A \code{string} that specifies the color of the region covered by the confidence intervals (confidence region).
#' @param transparency A \code{double} between 0 and 1 indicating the transparency level of the color defined in \code{col_ci}.
#' Defaults to 0.25. 
#' @param main      A \code{string} indicating the title of the plot. Default name is "Variable name ACF plot'.
#' @param parValue  A \code{vector} defining the margins for the plot.
#' @param ...       Additional parameters
#' @rdname plot.simtsACF
#' @export
#' @importFrom grDevices col2rgb
#' @examples 
#' # Calculate the Autocorrelation
#' m = auto_corr(datasets::AirPassengers)
#' 
#' # Plot with 95% CI
#' plot(m) 
#' 
#' # Plot with 90% CI
#' plot(m, alpha = 0.1) 
#' 
#' # Plot without 95% CI
#' plot(m, show.ci = FALSE)
#' 
#' # More customized CI
#' plot(m, xlab = "my xlab", ylab = "my ylab", show.ci = TRUE,
#' alpha = NULL, col_ci = "grey", transparency = 0.5, main = "my main")
plot.simtsACF = function(x, xlab = NULL, ylab = NULL, show.ci = TRUE, alpha = NULL, col_ci = NULL, transparency = NULL, main = NULL, parValue = NULL, ...){
  # TO ADD AS INPUTS
  lag_unit = attr(x, "unit_time")
  
  # add xlab
  if (!is.null(xlab)){
    xlab = xlab
  }else{
    if (!is.null(lag_unit)){
      xlab = paste("Lags (", lag_unit,")", sep = "")
    }else{
      xlab = "Lags"
    }
  }
  
  # add ylab
  if (!is.null(ylab)){
    ylab = ylab
  }else{
    ylab = "ACF"
  }
  
  
  # add alpha
  if (!is.null(alpha)){
    alpha = alpha
  }else{
    alpha = 0.05
  }
  
  # add transparency
  if (!is.null(transparency)){
    transparency = transparency
  }else{
    transparency = 0.25
  }
  
  # add color of CI
  if (!is.null(col_ci)){
    col_ci = col2rgb(col_ci)
    col_ci = rgb(col_ci[1], col_ci[2], col_ci[3], transparency*255, maxColorValue = 255)
  }else{
    col_ci = rgb(red = 0, green = 0.6, blue = 1, transparency)
  }
  
  
  # Quiet the warnings...
  Lag = xmin = xmax = ymin = ymax = NULL 
  
  # Wide to long array transform
  x2 = as.data.frame.table(x, responseName = "ACF")
  
  colnames(x2) = c("Lag", "Signal X", "Signal Y", "ACF")
  
  # Remove character cast
  x2$Lag = as.numeric(x2$Lag)
  
  # Range
  x_range = range(x2$Lag)-1
  
  # Remove confidence intervals for theoretical ACF
  if (attr(x, "dimnames")[[2]] == "Theoretical"){
    show.ci = FALSE
  }
  
  if (show.ci == TRUE){
      n = attr(x,"n")
      mult = qnorm(1-alpha/2)
      y_range = range(c(x2$ACF, 1/sqrt(n)*mult*c(-1,1)))
    }else{
      y_range = range(x2$ACF)
    }

  x_ticks = seq(x_range[1], x_range[2], by = 1)
  y_ticks = seq(y_range[1], y_range[2], by = 0.05)
  
  if (!is.null(parValue)){
    par(mar = parValue)
  }else{
    par(mar = c(5.1, 5.1, 1, 2.1)) 
  }
  
  
  
  # Title
  if (is.null(main)){
    if (is.null(attr(x,"data_name"))){
      main = paste0(as.character((x2$`Signal Y`)[1]), " ACF plot")
    }else{
      main = paste0(attr(x,"data_name")[1], " ACF plot")
    }
  }else {
    main = main
  }
  
  
  # Main plot
  plot(NA, xlim = c(0, max(x2$Lag)), ylim = y_range, 
       xlab = xlab, ylab = ylab, xaxt = 'n', 
       yaxt = 'n', bty = "n", ann = FALSE)
  win_dim = par("usr")
  
  par(new = TRUE)
  plot(NA, xlim = c(0, max(x2$Lag)), ylim = c(win_dim[3], win_dim[4] + 0.09*(win_dim[4] - win_dim[3])),
       xlab = xlab, ylab = ylab, xaxt = 'n', yaxt = 'n', bty = "n")
  win_dim = par("usr")
  
  # Add grid
  grid(NULL, NULL, lty = 1, col = "grey95")
  
  
  # Add title
  
  x_vec = c(win_dim[1], win_dim[2], win_dim[2], win_dim[1])
  y_vec = c(win_dim[4], win_dim[4],
            win_dim[4] - 0.09*(win_dim[4] - win_dim[3]),
            win_dim[4] - 0.09*(win_dim[4] - win_dim[3]))
  polygon(x_vec, y_vec, col = "grey95", border = NA)
  text(x = mean(c(win_dim[1], win_dim[2])),
       y = (win_dim[4] - 0.09/2*(win_dim[4] - win_dim[3])), 
       main)
  
  # Add axes and box
  lines(x_vec[1:2], rep((win_dim[4] - 0.09*(win_dim[4] - win_dim[3])),2), col = 1)
  box()
  axis(1, padj = 0.3)
  y_axis = axis(2, labels = FALSE, tick = FALSE)
  y_axis = y_axis[y_axis < (win_dim[4] - 0.09*(win_dim[4] - win_dim[3]))]
  axis(2, padj = -0.2, at = y_axis)
  
  
  abline(h = 0, lty = 1, lwd = 2)
  # Plot CI 
  if(show.ci){
    
    clim0 = 1/sqrt(n)*mult
    rect(xleft = -2, ybottom = -clim0, xright = 2*x_range[2], ytop = clim0, 
         col = col_ci, lwd = 0)
    
  }
  # Plot ACF
  segments(x0 = x_ticks, y0 = rep(0, x_range[2]), x1 = x_ticks, y1 = x2$ACF, lty = 1, lwd = 1)
}



# ----- empirical PACF
PACF = function(x, lag.max = NULL, type = "correlation", demean = TRUE){
  
  if (type != "correlation" && type != "covariance"){
    stop("Type not authorized. Allowed values correlation and covariance.")
  }
  
  # Change the data to matrix form
  if(is.ts(x) || is.atomic(x)){
    x2 = data.matrix(x)        
  }
  
  if (demean){
    x3 = as.matrix(x)
    x3 = sweep(x3, 2, colMeans(x3, na.rm = TRUE), check.margin = FALSE)
  }

  #Get the pacf value of the data
  pacfe = pacf(x3, lag.max = lag.max, type = type, plot = FALSE)
  pacfe = pacfe$acf
  
  # Get the data name 
  varName = deparse(substitute(x))
  
  
  # Adjust the name for data 
  dimnames(pacfe)  = list(seq_len(nrow(pacfe))-1, "PACF", varName)
  
  if (is.null(attr(x, "data_name"))){
    pacfe = structure(pacfe, n = nrow(x2), class = c("PACF", "array"))
  }else{
    pacfe = structure(pacfe, n = nrow(x2), data_name = attr(x, "data_name"), class = c("PACF", "array"))
  }
  
  unit_time = attr(x, "unit_time")
  if (!is.null(unit_time)){
    attr(pacfe, "unit_time") = unit_time
  }
  
  if(!is.null(names(x))){
    attr(pacfe, "data_name") = names(x)
  }
  
  pacfe
  
}


#' @title Plot Partial Auto-Covariance and Correlation Functions
#' @description The function plots the output of the \code{\link{theo_pacf}} and \code{\link{auto_corr}} functions (partial autocovariance or autocorrelation functions).
#' @author Yunxiang Zhang and Yuming Zhang
#' @param x         A \code{"PACF"} object output from \code{\link{theo_pacf}} or \code{\link{auto_corr}}.
#' @param xlab      A \code{string} indicating the label of the x axis: the default name is 'Lags'.
#' @param ylab      A \code{string} indicating the label of the y axis: the default name is 'PACF'.
#' @param show.ci   A \code{bool} indicating whether to show the confidence region. Defaults to \code{TRUE}.
#' @param alpha     A \code{double} indicating the level of significance for the confidence interval. By default \code{alpha = 0.05} which gives a 1 - \code{alpha} = 0.95 confidence interval. 
#' @param col_ci    A \code{string} that specifies the color of the region covered by the confidence intervals (confidence region).
#' @param transparency A \code{double} between 0 and 1 indicating the transparency level of the color defined in \code{col_ci}.
#' Defaults to 0.25. 
#' @param main      A \code{string} indicating the title of the plot. Default name is "Variable name PACF plot'.
#' @param parValue  A \code{vector} defining the margins for the plot.
#' @param ...       Additional parameters
#' @rdname plot.PACF
#' @importFrom grDevices col2rgb
#' @export
#' @examples 
#' # Plot the Partial Autocorrelation
#' m = auto_corr(datasets::AirPassengers, pacf = TRUE)
#' plot(m)
#' 
#' # More customized CI
#' plot(m, xlab = "my xlab", ylab = "my ylab", show.ci = TRUE, 
#' alpha = NULL, col_ci = "grey", transparency = 0.5, main = "my main")
plot.PACF = function(x, xlab = NULL, ylab = NULL, show.ci = TRUE, alpha = NULL, col_ci = NULL, transparency = NULL, main = NULL, parValue = NULL, ...){
  # TO ADD AS INPUTS
  lag_unit = attr(x, "unit_time")
  
  # add xlab
  if (!is.null(xlab)){
    xlab = xlab
  }else{
    if (!is.null(lag_unit)){
      xlab = paste("Lags (", lag_unit,")", sep = "")
    }else{
      xlab = "Lags"
    }
  }
  
  # add ylab
  if (!is.null(ylab)){
    ylab = ylab
  }else{
    ylab = "PACF"
  }
  
  # add alpha
  if (!is.null(alpha)){
    alpha = alpha
  }else{
    alpha = 0.05
  }
  
  # add transparency
  if (!is.null(transparency)){
    transparency = transparency
  }else{
    transparency = 0.25
  }
  
  # add color of CI
  #col_ci = rgb(0, 0.6, 1, 0.2)
  if (!is.null(col_ci)){
    col_ci = col2rgb(col_ci)
    col_ci = rgb(col_ci[1], col_ci[2], col_ci[3], transparency*255, maxColorValue = 255)
  }else{
    col_ci = rgb(red = 0, green = 0.6, blue = 1, transparency)
  }
  
  
  # Quiet the warnings...
  Lag = xmin = xmax = ymin = ymax = NULL 
  
  # Wide to long array transform
  x2 = as.data.frame.table(x, responseName = "PACF")
  
  colnames(x2) = c("Lag", "Signal X", "Signal Y", "PACF")
  
  # Remove character cast
  x2$Lag = as.numeric(x2$Lag)
  
  # Range
  x_range = range(x2$Lag)
  
  # Remove confidence intervals for theoretical ACF
  if (attr(x, "dimnames")[[2]] == "Theoretical"){
    show.ci = FALSE
  }
  
  if (show.ci == TRUE){
    n = attr(x,"n")
    mult = qnorm(1-alpha/2)
    y_range = range(c(x2$PACF, 1/sqrt(n)*mult*c(-1,1)))
  }else{
    y_range = range(x2$PACF)
  }
  
  x_ticks = seq(x_range[1], x_range[2], by = 1)
  y_ticks = seq(y_range[1], y_range[2], by = 0.05)

  
  if (!is.null(parValue)){
    par(mar = parValue)
  }else{
    par(mar = c(5.1, 5.1, 1, 2.1)) 
  }
  
  
  # Title
  if (is.null(main)){
    if (is.null(attr(x,"data_name"))){
      main = paste0(as.character((x2$`Signal Y`)[1]), " PACF plot")
    }else{
      main = paste0(attr(x,"data_name")[1], " PACF plot")
    }
  }else {
    main = main
  }
  
  
  # Main plot
  plot(NA, xlim = x_range, ylim = y_range, 
       xlab = xlab, ylab = ylab, xaxt = 'n', 
       yaxt = 'n', bty = "n", ann = FALSE)
  win_dim = par("usr")
  
  par(new = TRUE)
  plot(NA, xlim = x_range, ylim = c(win_dim[3], win_dim[4] + 0.09*(win_dim[4] - win_dim[3])),
       xlab = xlab, ylab = ylab, xaxt = 'n', yaxt = 'n', bty = "n")
  win_dim = par("usr")
  
  # Add grid
  grid(NULL, NULL, lty = 1, col = "grey95")
  
  # Add title
  x_vec = c(win_dim[1], win_dim[2], win_dim[2], win_dim[1])
  y_vec = c(win_dim[4], win_dim[4],
            win_dim[4] - 0.09*(win_dim[4] - win_dim[3]),
            win_dim[4] - 0.09*(win_dim[4] - win_dim[3]))
  polygon(x_vec, y_vec, col = "grey95", border = NA)
  text(x = mean(c(win_dim[1], win_dim[2])),
       y = (win_dim[4] - 0.09/2*(win_dim[4] - win_dim[3])), 
       main)
  
  # Add axes and box
  lines(x_vec[1:2], rep((win_dim[4] - 0.09*(win_dim[4] - win_dim[3])),2), col = 1)
  box()
  axis(1, padj = 0.3)
  y_axis = axis(2, labels = FALSE, tick = FALSE)
  y_axis = y_axis[y_axis < (win_dim[4] - 0.09*(win_dim[4] - win_dim[3]))]
  axis(2, padj = -0.2, at = y_axis)
  
  abline(h = 0, lty = 1, lwd = 2)
  # Plot CI 
  if(show.ci){
    
    clim0 = 1/sqrt(n)*mult
    rect(xleft = -2, ybottom = -clim0, xright = 2*x_range[2], 
         ytop = clim0, col = col_ci, lwd = 0)
    
  }
  # Plot PACF
  segments(x0 = x_ticks, y0 = rep(0, x_range[2]), x1 = x_ticks, y1 = x2$PACF, lty = 1, lwd = 1)
}


#' @title Correlation Analysis Functions
#' @description Correlation Analysis function computes and plots both empirical ACF and PACF
#' of univariate time series.
#' @author Yunxiang Zhang
#' @param x         A \code{vector} or \code{"ts"} object (of length \eqn{N > 1}).
#' @param lag.max    A \code{integer} indicating the maximum lag up to which to compute the ACF and PACF functions.
#' @param type   A \code{character} string giving the type of acf to be computed. Allowed values are "correlation" (the default) and "covariance".
#' @param demean    A \code{bool} indicating whether the data should be detrended (\code{TRUE}) or not (\code{FALSE}). Defaults to \code{TRUE}.
#' @param show.ci   A \code{bool} indicating whether to compute and show the confidence region. Defaults to \code{TRUE}.
#' @param alpha     A \code{double} indicating the level of significance for the confidence interval. By default \code{alpha = 0.05} which gives a 1 - \code{alpha} = 0.95 confidence interval. 
#' @param plot      A \code{bool} indicating whether a plot of the computed quantities should be produced. Defaults to \code{TRUE}.
#' @param ...       Additional parameters.
#' @return Two \code{array} objects (ACF and PACF) of dimension \eqn{N \times S \times S}{N x S x S}.
#' @rdname corr_analysis
#' @export
#' @examples
#' # Estimate both the ACF and PACF functions
#' corr_analysis(datasets::AirPassengers)
corr_analysis = function(x, lag.max = NULL, type = "correlation", demean = TRUE, show.ci = TRUE, alpha = 0.05, plot = TRUE,  ...){
  
  if (type != "correlation" && type != "covariance"){
    stop("Type not authorized. Allowed values correlation and covariance.")
  }
  
  # Compute ACF and PACF
  acfe = auto_corr(x, lag.max = lag.max, type = type, demean = demean)
  pacfe = PACF(x, lag.max = lag.max, type = type, demean = demean)
  
  # Plots
  if (plot){
    par(mfrow = c(1,2))
    plot(acfe, show.ci = TRUE, alpha = 0.05, main = "Empirical ACF", parValue = c(5.1, 4.5, 1,2))
    plot(pacfe, show.ci = TRUE, alpha = 0.05, main = "Empirical PACF", parValue = c(5.1,3.85,1,2.1))
  }
  
  par(mfrow = c(1,1))
  invisible(list("ACF" = acfe, "PACF" = pacfe))
}



#' @title Comparison of Classical and Robust Correlation Analysis Functions
#' @description Compare classical and robust ACF
#' of univariate time series.
#' @author Yunxiang Zhang
#' @param x         A \code{vector} or \code{"ts"} object (of length \eqn{N > 1}).
#' @param lag.max    A \code{integer} indicating the maximum lag up to which to compute the ACF and PACF functions.
#' @param demean    A \code{bool} indicating whether the data should be detrended (\code{TRUE}) or not (\code{FALSE}). Defaults to \code{TRUE}.
#' @param show.ci   A \code{bool} indicating whether to compute and show the confidence region. Defaults to \code{TRUE}.
#' @param alpha     A \code{double} indicating the level of significance for the confidence interval. By default \code{alpha = 0.05} which gives a 1 - \code{alpha} = 0.95 confidence interval. 
#' @param plot      A \code{bool} indicating whether a plot of the computed quantities should be produced. Defaults to \code{TRUE}.
#' @param ...       Additional parameters.
#' @rdname compare_acf
#' @export
#' @examples
#' # Estimate both the ACF and PACF functions
#' compare_acf(datasets::AirPassengers)
compare_acf = function(x, lag.max = NULL, demean = TRUE, show.ci = TRUE, alpha = 0.05, plot = TRUE,  ...){
  
  # Compute ACF and PACF
  acfe = auto_corr(x, lag.max = lag.max, demean = demean)
  pacfe = auto_corr(x, lag.max = lag.max, demean = demean, robust = TRUE)
  
  # Plots
  if (plot){
    par(mfrow = c(1,2))
    plot(acfe, show.ci = TRUE, alpha = 0.05, main = "Empirical Classical ACF", parValue = c(5.1, 4.5, 1,2))
    plot(pacfe, show.ci = TRUE, alpha = 0.05, main = "Empirical Robust ACF", parValue = c(5.1, 4.5, 1,2))
  }
  par(mfrow = c(1,1))
}









