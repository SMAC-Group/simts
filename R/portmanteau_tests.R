diag_portmanteau_ = function(x, order = NULL, stop_lag = 20, stdres = FALSE, test = "Ljung-Box"){
  
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
  
  structure(data.frame(lag = diag_lags, pvalue = mat[diag_lags, 1],
                       statistic = mat[diag_lags, 2]),
            class = c("diag_portmanteau", kind, "data.frame"))
}

############################################
# Test Ljung-Box Wrapper
############################################

#' @title Ljung-Box
#' @description Performs the Ljung-Box test to assess the Null Hypothesis of Independence
#'  in a Time Series
#' @author James Balamuta
#' @param x        An \code{arima} or data set.
#' @param order    An \code{integer} indicating the degrees of freedom. If `x` is
#' not a series of residuals, then set equal to 0.
#' @param stop_lag An \code{integer} indicating the length of lags that should
#' be calculated.
#' @param stdres   A \code{boolean} indicating whether to standardize the
#' residualizes (e.g. \eqn{res/sd(res)}) or not.
#' @export
#' @rdname diag_ljungbox
diag_ljungbox = function(x, order = NULL, stop_lag = 20, stdres = FALSE,  ...){
  UseMethod("diag_ljungbox")
}

#' @export
#' @rdname diag_ljungbox
diag_ljungbox.Arima = function(x, stop_lag = 20, stdres = FALSE, ...){
  diag_ljungbox.default(x$residuals, order = length(x$coef), stop_lag = stop_lag, stdres = stdres, ...)
}

#' @export
#' @rdname diag_ljungbox
diag_ljungbox.default = function(x, order = NULL, stop_lag = 20, stdres = FALSE, ...){
  diag_portmanteau_(x, order = order, stop_lag = stop_lag, stdres = stdres, test = "Ljung-Box")
}


############################################
# Test Box-Pierce Wrapper
############################################

#' @title Box-Pierce
#' @description Performs the Box-Pierce test to assess the Null Hypothesis of Independence
#'  in a Time Series
#' @author James Balamuta
#' @inheritParams diag_ljungbox
#' @export
#' @rdname diag_boxpierce
diag_boxpierce = function(x, order = NULL, stop_lag = 20, stdres = FALSE, ...){
  UseMethod("diag_boxpierce")
}

#' @export
#' @rdname diag_boxpierce
diag_boxpierce.Arima = function(x, stop_lag = 20, stdres = FALSE, ...){
  diag_boxpierce.default(x$residuals, order = length(x$coef), stop_lag = stop_lag, stdres = stdres, ...)
}

#' @export
#' @rdname diag_boxpierce
diag_boxpierce.default = function(x, order = NULL, stop_lag = 20, stdres = FALSE, ...){
  diag_portmanteau_(x, order = order, stop_lag = stop_lag, stdres = stdres, test = "Box-Pierce", ...)
}


############################################
# Graph Portmanteau Test Results
############################################

#' @title Graph Portmanteau Test Results
#' @description Plots the portmanteau test results
#' @author Yuming Zhang
#' @param object A \code{diag_portmanteau} object from either
#'  \code{\link{diag_boxpierce}} or \code{\link{diag_ljungbox}}
#' @export
#' @rdname diag_portmanteau

plot.diag_portmanteau = function(object, ...){
  
  test = if(inherits(object, "diag_ljungbox")) {"Ljung-Box"} else {"Box-Pierce"}
  
  maxval = max(object[,"pvalue"])
  
  # make frame
  x_range = c(min(object$lag), max(object$lag))*1.05
  y_range = c(0, max(object$pvalue))*1.05
  make_frame(x_range, y_range, 
             xlab = "Lag", ylab = "P-value", main = paste0(test, " Results"))
  
  # add plotting
  points(object$lag, object$pvalue, pch = 16)
  lines(object$lag, object$pvalue, lty = 3)
  abline(h = 0.05, col = "blue", lty = 2)
}




