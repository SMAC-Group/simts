#' @title Portmanteau Tests
#' @description Performs the Portmanteau test to assess the Null Hypothesis of Independence
#'  in a Time Series
#' @author James Balamuta, Stéphane Guerrier, Yuming Zhang
#' @param x        An \code{arima} or data set.
#' @param order    An \code{integer} indicating the degrees of freedom. If `x` is
#' not a series of residuals, then set equal to 0.
#' @param stop_lag An \code{integer} indicating the length of lags that should
#' be calculated.
#' @param stdres   A \code{boolean} indicating whether to standardize the
#' residualizes (e.g. \eqn{res/sd(res)}) or not.
#' @param test     A \code{string} indicating whether to perform Ljung-Box test or 
#' Box-Pierce test.
#' @param plot     A logical. If \code{TRUE} (the default) a plot should be produced.
#' @importFrom stats sd
#' @importFrom stats Box.test 
diag_portmanteau_ = function(x, order = NULL, stop_lag = 20, stdres = FALSE, test = "Ljung-Box", plot = TRUE){
  
  if(is.null(order)){
    stop("Need to fill in the order of ARMA")
  }
  
  kind = switch(test,
                "Ljung-Box" = "diag_ljungbox",
                "Box-Pierce" = "diag_boxpierce",
                stop("Test not supported!"))
  
  if(stdres){
    x = x/sd(x)
  }
  
  mat = matrix(NA, stop_lag, 2)
  
  diag_lags = (order+1):stop_lag
  
  for(i in diag_lags){
    active = Box.test(x, lag = i, type = test, fitdf = order)
    mat[i,] = c(active$p.value, active$statistic)
  }
  
  out = data.frame(lag = diag_lags, pvalue = mat[diag_lags, 1],
                       statistic = mat[diag_lags, 2],
                        kind = kind)
  class(out) = "dp"
  
  if (plot == TRUE){
      object = out
      maxval = max(object$pvalue)
      
      # make frame
      x_range = c(min(object$lag), max(object$lag))*1.05
      y_range = c(0, max(object$pvalue))*1.05
      make_frame(x_range, y_range, 
                 xlab = "Lag", ylab = "P-value", main = paste0(object$kind, " Results"))
      
      # add plotting
      points(object$lag, object$pvalue, pch = 16)
      lines(object$lag, object$pvalue, lty = 3)
      abline(h = 0.05, col = "blue", lty = 2)
  }
  out
}

############################################
# Test Ljung-Box Wrapper
############################################
#' @title Ljung-Box
#' @description Performs the Ljung-Box test to assess the Null Hypothesis of Independence
#'  in a Time Series
#' @author James Balamuta, Stéphane Guerrier, Yuming Zhang
#' @param x        An \code{arima} or data set.
#' @param order    An \code{integer} indicating the degrees of freedom. If `x` is
#' not a series of residuals, then set equal to 0.
#' @param stop_lag An \code{integer} indicating the length of lags that should
#' be calculated.
#' @param stdres   A \code{boolean} indicating whether to standardize the
#' residualizes (e.g. \eqn{res/sd(res)}) or not.
#' @param plot     A logical. If \code{TRUE} (the default) a plot should be produced.
#' @export
#' @rdname diag_ljungbox
diag_ljungbox = function(x, order = NULL, stop_lag = 20, stdres = FALSE, plot = TRUE){
  if (class(x) == "Arima"){
    diag_portmanteau_(x$residuals, order = length(x$coef), 
                      stop_lag = stop_lag, stdres = stdres, test = "Ljung-Box", plot = plot)
  }else{
    diag_portmanteau_(x, order = order, stop_lag = stop_lag, stdres = stdres, test = "Ljung-Box", plot = plot)
  }
}


############################################
# Test Box-Pierce Wrapper
############################################

#' @title Box-Pierce
#' @description Performs the Box-Pierce test to assess the Null Hypothesis of Independence
#'  in a Time Series
#' @author James Balamuta, Stéphane Guerrier, Yuming Zhang
#' @param x        An \code{arima} or data set.
#' @param order    An \code{integer} indicating the degrees of freedom. If `x` is
#' not a series of residuals, then set equal to 0.
#' @param stop_lag An \code{integer} indicating the length of lags that should
#' be calculated.
#' @param stdres   A \code{boolean} indicating whether to standardize the
#' residualizes (e.g. \eqn{res/sd(res)}) or not.
#' @param plot     A logical. If \code{TRUE} (the default) a plot should be produced.
#' @export
#' @rdname diag_boxpierce
diag_boxpierce = function(x, order = NULL, stop_lag = 20, stdres = FALSE, plot = TRUE){
  if (class(x) == "Arima"){
    diag_portmanteau_(x$residuals, order = length(x$coef), 
                      stop_lag = stop_lag, stdres = stdres, test = "Box-Pierce", plot = plot)
  }else{
    diag_portmanteau_(x, order = order, stop_lag = stop_lag, stdres = stdres, test = "Box-Pierce", plot = plot)
  }
}
