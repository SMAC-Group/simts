# Copyright (C) 2017 James Balamuta, Justin Lee, Stephane Guerrier, Roberto Molinari
#
# This file is part of simts R Methods Package
#
# The `wv` R package is free software: you can redistribute it and/or modify
# it under the terms of the GNU Affero General Public License as
# published by the Free Software Foundation, either version 3 of the
# License, or (at your option) any later version.
#
# The `simts` R package is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
#
# You should have received a copy of the GNU Affero General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.


#' @title Auto-Covariance and Correlation Functions
#' @description The ACF function computes the estimated
#' autocovariance or autocorrelation for both univariate and multivariate cases.
#' @author Yunxiang Zhang
#' @param x      A \code{matrix} with dimensions \eqn{N \times S}{N x S} or N observations and S processes
#' @param lagmax A \code{integer} indicating the max lag.
#' @param cor    A \code{bool} indicating whether the correlation 
#' (\code{TRUE}) or covariance (\code{FALSE}) should be computed.
#' @param demean A \code{bool} indicating whether the data should be detrended
#'  (\code{TRUE}) or not (\code{FALSE})
#' @return An \code{array} of dimensions \eqn{N \times S \times S}{N x S x S}.
#' @details 
#' \code{lagmax} default is \eqn{10*log10(N/m)} where \eqn{N} is the number of
#' observations and \eqn{m} is the number of series being compared. If 
#' \code{lagmax} supplied is greater than the number of observations, then one
#' less than the total will be taken.
#' @importFrom stats acf pacf 
#' @export
#' @examples 
#' # Get Autocorrelation
#' m = ACF(datasets::AirPassengers)
#' 
#' # Get Autocovariance and do not remove trend from signal
#' m = ACF(datasets::AirPassengers, cor = FALSE, demean = FALSE)
ACF = function(x, lagmax = 0, cor = TRUE, demean = TRUE){
  
  # Change the data to matrix form
  if(is.ts(x) || is.atomic(x)){
    x2 = data.matrix(x)        
  }
  
  #Get the acf value of the data
  acfe = acf(x, lagmax = lagmax , cor = cor, demean = demean, plot = FALSE)
  acfe = acfe$acf
  
  
  # Get the data name 
  varName = deparse(substitute(x))
  
  
  # Adjust the name for data 
  dimnames(acfe)  = list(seq_len(nrow(acfe))-1, "ACF", varName)
  
  if (is.null(attr(x, "data_name"))){
    acfe = structure(acfe, n = nrow(x2), class = c("ACF", "array"))
  }else{
    acfe = structure(acfe, n = nrow(x2), data_name = attr(x, "data_name"), class = c("ACF", "array"))
  }
  
  acfe
}

#' @title Plot Auto-Covariance and Correlation Functions
#' @description The acf function computes the estimated
#' autocovariance or autocorrelation for both univariate and multivariate cases.
#' @author Yunxiang Zhang
#' @param x         An \code{"ACF"} object from \code{\link{ACF}}.
#' @param show.ci   A \code{bool} indicating whether to show confidence region.
#' @param ylab     A \code{text} indicating the label of y axis. 
#' @param alpha     A \code{double} indicating the confidence interval level. Default is 0.05. 
#' @param main      A \code{string} indicating the title of the plot. 
#' @param ...       Additional parameters
#' @return An \code{array} of dimensions \eqn{N \times S \times S}{N x S x S}.
#' @rdname plot.ACF
#' @export
#' @examples 
#' # Calculate the Autocorrelation
#' m = ACF(datasets::AirPassengers)
#' 
#' # Plot with 95% CI
#' plot(m) 
#' 
#' # Plot with 90% CI
#' plot(m, ci = 0.90) 
#' 
#' # Plot without 95% CI
#' plot(m, show.ci = FALSE)
plot.ACF = function(x, show.ci = TRUE, ylab = "ACF", alpha = 0.05, main = NULL, ...){
  # TO ADD AS INPUTS
  xlab = "Lags"
  ylab = ylab
  col_ci = rgb(0, 0.6, 1, 0.2)
  alpha = 0.05
  
  
  # Quiet the warnings...
  Lag = xmin = xmax = ymin = ymax = NULL 
  
  # Wide to long array transform
  x2 = as.data.frame.table(x, responseName = "ACF")
  
  colnames(x2) = c("Lag", "Signal X", "Signal Y", "ACF")
  
  # Remove character cast
  x2$Lag = as.numeric(x2$Lag)
  
  # Range
  x_range = range(x2$Lag)-1
  
  if (show.ci == TRUE){
    n = attr(x,"n")
    mult = qnorm(1-alpha/2)
    y_range = range(c(x2$ACF, 1/sqrt(n)*mult*c(-1,1)))
  }else{
    y_range = range(0:1)
  }
  
  x_ticks = seq(x_range[1], x_range[2], by = 1)
  y_ticks = seq(y_range[1], y_range[2], by = 0.05)
  par(mar = c(5.1, 5.1, 1, 2.1))
  
  
  # Title
  if (is.null(main)){
    if (is.null(attr(x,"data_name"))){
      main = paste0(as.character((x2$`Signal Y`)[1]), " ACF")
    }else{
      main = paste0(attr(x,"data_name"), " ACF")
    }
  }
  else {
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
    rect(xleft = -2, ybottom = -clim0, xright = 2*x_range[2], 
         ytop = clim0, col = col_ci, lwd = 0)
    
  }
  # Plot ACF
  segments(x0 = x_ticks, y0 = rep(0, x_range[2]), x1 = x_ticks, y1 = x2$ACF, lty = 1, lwd = 1)
}





#' @title Partial Auto-Covariance and Correlation Functions
#' @description The PACF function computes the estimated
#' partial autocovariance or autocorrelation for both univariate and multivariate cases.
#' @author Yunxiang Zhang
#' @param x      A \code{matrix} with dimensions \eqn{N \times S}{N x S} or N observations and S processes
#' @param lagmax A \code{integer} indicating the max lag.
#' @param cor    A \code{bool} indicating whether the correlation (\code{TRUE}) or covariance (\code{FALSE}) should be computed.
#' @param demean A \code{bool} indicating whether the data should be detrended (\code{TRUE}) or not (\code{FALSE})
#' @return An \code{array} of dimensions \eqn{N \times S \times S}{N x S x S}.
#' @details 
#' \code{lagmax} default is \eqn{10*log10(N/m)} where \eqn{N} is the number of
#' observations and \eqn{m} is the number of series being compared. If 
#' \code{lagmax} supplied is greater than the number of observations, then one
#' less than the total will be taken.
#' @export
#' @examples 
#' # Get Autocorrelation
#' m = PACF(datasets::AirPassengers)
#' 
#' # Get Autocovariance and do not remove trend from signal
#' m = PACF(datasets::AirPassengers, cor = FALSE, demean = FALSE)
PACF = function(x, lagmax = 0, cor = TRUE, demean = TRUE){
  
  # Change the data to matrix form
  if(is.ts(x) || is.atomic(x)){
    x2 = data.matrix(x)        
  }
  
  #Get the pacf value of the data
  pacfe = pacf(x, lagmax = lagmax , cor = cor, demean = demean, plot = FALSE)
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
  
  pacfe
  
}


#' @title Plot Partial Auto-Covariance and Correlation Functions
#' @description Plot pacf function computes the estimated
#' plot partial autocovariance or autocorrelation for both univariate and multivariate cases.
#' @author Yunxiang Zhang
#' @param x         An \code{"ACF"} object from \code{\link{ACF}}.
#' @param show.ci   A \code{bool} indicating whether to show confidence region.
#' @param ylab      A \code{string} indicating the y-axis label name. 
#' @param alpha     A \code{double} indicating the confidence interval level. Default is 0.05. 
#' @param main      A \code{string} indicating the title of the plot. 
#' @param ...       Additional parameters.
#' @return An \code{array} of dimensions \eqn{N \times S \times S}{N x S x S}.
#' @rdname plot.PACF
#' @export
#' @examples 
#' # Plot the Partial Autocorrelation
#' m = PACF(datasets::AirPassengers)
#' plot(m)
plot.PACF = function(x, show.ci = TRUE, ylab = "PACF", alpha = 0.05, main = NULL, ...){
  # TO ADD AS INPUTS
  xlab = "Lags"
  ylab = ylab
  col_ci = rgb(0, 0.6, 1, 0.2)
  alpha = 0.05
  
  
  # Quiet the warnings...
  Lag = xmin = xmax = ymin = ymax = NULL 
  
  # Wide to long array transform
  x2 = as.data.frame.table(x, responseName = "PACF")
  
  colnames(x2) = c("Lag", "Signal X", "Signal Y", "PACF")
  
  # Remove character cast
  x2$Lag = as.numeric(x2$Lag)
  
  # Range
  x_range = range(x2$Lag)-1
  
  if (show.ci == TRUE){
    n = attr(x,"n")
    mult = qnorm(1-alpha/2)
    y_range = range(c(x2$PACF, 1/sqrt(n)*mult*c(-1,1)))
  }else{
    y_range = range(0:1)
  }
  
  x_ticks = seq(x_range[1], x_range[2], by = 1)
  y_ticks = seq(y_range[1], y_range[2], by = 0.05)
  par(mar = c(5.1, 5.1, 1, 2.1))
  
  
  # Title
  if (is.null(main)){
    if (is.null(attr(x,"data_name"))){
      main = paste0(as.character((x2$`Signal Y`)[1]), " PACF")
    }else{
      main = paste0(attr(x,"data_name"), " PACF")
    }
  }
  else {
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
    rect(xleft = -2, ybottom = -clim0, xright = 2*x_range[2], 
         ytop = clim0, col = col_ci, lwd = 0)
    
  }
  # Plot PACF
  segments(x0 = x_ticks, y0 = rep(0, x_range[2]), x1 = x_ticks, y1 = x2$PACF, lty = 1, lwd = 1)
}


#' @title Correlation Analysis Functions
#' @description Correlation Analysis function computes and plots both empirical ACF and PACF
#' of both univariate and multivariate cases.
#' @author Yunxiang Zhang
#' @param x         An \code{"ts"} object.
#' @param lagmax    A \code{integer} indicating the max lag.
#' @param cor       A \code{bool} indicating whether the correlation (\code{TRUE}) or covariance (\code{FALSE}) should be computed.
#' @param demean    A \code{bool} indicating whether the data should be detrended (\code{TRUE}) or not (\code{FALSE}).
#' @param show.ci   A \code{bool} indicating whether to show confidence region.
#' @param alpha     A \code{double} indicating the confidence interval level. Default is 0.05. 
#' @param plot      A \code{bool} indicating whether to plot. 
#' @param ...       Additional parameters.
#' @return Two \code{array} of dimensions \eqn{N \times S \times S}{N x S x S}.
#' @rdname corr_analysis
#' @export
#' @examples 
#' # Plot the Partial Autocorrelation
#' corr_analysis(datasets::AirPassengers)
corr_analysis = function(x, lagmax = 0, cor = TRUE, demean = TRUE, show.ci = TRUE, alpha = 0.05, plot = TRUE,  ...){
  
  # Compute ACF and PACF
  acfe = ACF(x, lagmax = 0, cor = TRUE, demean = TRUE)
  pacfe = PACF(x, lagmax = 0, cor = TRUE, demean = TRUE)
  
  # Plots
  if (plot){
    par(mfrow=c(1,2))
    plot(acfe, show.ci = TRUE, alpha = 0.05, ylab = '', main = "Empirical ACF")
    plot(pacfe, show.ci = TRUE, alpha = 0.05, ylab = '', main = "Empirical PACF")
  }
  
  par(mfrow = c(1,1))
  
  return(list("ACF" = acfe, "PACF" = pacfe))
  
}
