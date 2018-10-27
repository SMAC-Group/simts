################################
### fitsimts: estimate
################################

#' @title Fit a Time Series Model to Data
#' @description This function can fit a time series model to data using different methods. 
#' @param model A time series model.
#' @param Xt A \code{vector} of time series data. 
#' @param method A \code{string} indicating the method used for model fitting. 
#' Supported methods include \code{mle}, \code{yule-walker}, \code{gmwm}  and \code{rgmwm}. 
#' @param demean A \code{boolean} indicating whether the model includes a mean / intercept term or not.
#' @author Stéphane Guerrier and Yuming Zhang
#' @examples
#' Xt = gen_gts(300, AR(phi = c(0, 0, 0.8), sigma2 = 1))
#' model = estimate(AR(3), Xt)
#' model = estimate(AR(3), Xt, method = "rgmwm")
#' @export
estimate = function(model, Xt, method = "mle", demean = TRUE){
  all_method = c("mle", "yule-walker", "rgmwm", "gmwm")
  if (!(method %in% all_method)){
    stop("Only the following method are currently supported: 
         mle, yule-walker and rgmwm.")
  }
  
  # Check model
  if (!is.ts.model(model)){
    stop("The model provided is not a valid model.")
  }
  
  # Determine model
  model_code = model$obj.desc[[1]]
  
  if (sum(model_code[3:4]) > 0){
    stop("SARIMA are currently not supported.")
  }
  
  # Order of AR
  p = model_code[1]
  
  # Order of MA 
  q = model_code[2]
  if (q > 0){
    stop("MA are currently not supported.")
  }
  
  if (q == 0){
    model_type = "AR"
    model_name = paste("AR(",p,")", sep = "")
  }
  
  if (model_type == "AR"){
    if (method == "mle" || method == "yule-walker"){
      if (method == "mle"){
        meth = "ML"
      }else{
        meth = "CSS"
      }
      mod = arima(as.numeric(Xt), c(p, 0, 0), method = meth, include.mean = demean)
      sample_mean = NULL
    }else{
      if (method == "gmwm"){
        mod = gmwm(model, Xt)
      }else{
        mod = gmwm(model, Xt, robust = TRUE)
      }
      
      if (demean){
        sample_mean = mean(Xt)
      }else{
        sample_mean = NULL
      }
    }
  }
  
  out = list(mod = mod, method = method, 
             demean = demean, Xt = Xt, 
             sample_mean = sample_mean, model_name = model_name,
             model_type =  model_type)
  class(out) = "fitsimts"
  out
}

#' @export
print.fitsimts = function(out){
  print(out$mod)
}


################################
### fitsimts: check
################################

#' @title Diagnostics on Fitted Time Series Model 
#' @description This function can perform (simple) diagnostics on the fitted time series model.
#' It can output 6 diagnostic plots to assess the model, including (1) residuals plot,
#' (2) histogram of distribution of standardized residuals, (3) Normal Q-Q plot of residuals,
#' (4) ACF plot, (5) PACF plot, (6) Box test results. 
#' @param model A \code{fitsimts} object obtained from \code{estimate} function. 
#' @param simple A \code{boolean} indicating whether to return simple diagnostic plots or not. 
#' @author Stéphane Guerrier and Yuming Zhang
#' @examples
#' Xt = gen_gts(300, AR(phi = c(0, 0, 0.8), sigma2 = 1))
#' model = estimate(AR(3), Xt)
#' check(model)
#' check(model, simple = TRUE)
#' @export
check = function(model, simple = FALSE){
  if (simple){
    simple_diag_plot(model$Xt, model)
  }else{
    diag_plot(model$Xt, model)
  }
}


################################
### fitsimts: predict
################################

#' @title Time Series Prediction
#' @description This function plots the time series forecast.
#' @param model A \code{fitsimts} object obtained from \code{estimate} function. 
#' @param n.ahead An \code{integer} indicating number of units of time ahead for which to make forecasts.
#' @param show_last A \code{integer} indicating the number of last observations to show in the forecast plot.
#' @param level A \code{double} or \code{vector} indicating confidence level of prediction interval.
#' By default, it uses the levels of 0.50 and 0.95.
#' @param xlab A \code{string} for the title of x axis.
#' @param ylab A \code{string} for the title of y axis.
#' @param main A \code{string} for the over all title of the plot.
#' @author Stéphane Guerrier and Yuming Zhang
#' @examples
#' Xt = gen_gts(300, AR(phi = c(0, 0, 0.8), sigma2 = 1))
#' model = estimate(AR(3), Xt)
#' predict(model)
#' predict(model, level = 0.95)
#' 
#' x = gts(as.vector(lynx), start = 1821, end = 1934, freq = 1, 
#' unit_ts = bquote(paste(10^8," ",m^3)), name_ts = "Numbers", 
#' unit_time = "year", data_name = "Annual Numbers of Lynx Trappings")
#' model = estimate(AR(1), x)
#' predict(model, n.ahead = 20)
#' predict(model, n.ahead = 20, level = 0.95)
#' 
#' @export
#' 
predict.fitsimts = function(model, n.ahead = 10, show_last = 100, level = NULL, 
                            xlab = NULL, ylab = NULL, main = NULL){
  Xt = model$Xt
  freq = attr(Xt, 'freq')
  end_time =  attr(Xt, 'end')
  if (length(Xt) > show_last){
    Xt2 = Xt[((freq*end_time-show_last):(freq*end_time)) - attr(Xt, 'start') + 1]

    start = end_time-show_last/freq
    end = attr(Xt, 'end')
    freq = attr(Xt, 'freq')
    unit_ts = attr(Xt, 'unit_ts')
    name_ts = attr(Xt, 'name_ts')
    unit_time = attr(Xt, 'unit_time')
    name_time = attr(Xt, 'name_time')
    Time = attr(Xt, 'Time')
    data_name = attr(Xt, 'data_name')
    title_x = attr(Xt, 'print')
    simulated = attr(Xt, 'simulated')
    
    Xt = structure(Xt2, start=start, end=end, freq=freq, 
                   unit_ts=unit_ts, unit_time=unit_time, name_ts=name_ts,
                   name_time=name_time, data_name=data_name, Time=Time,
                   print=title_x, simulated=simulated,
                   class = c("gts","matrix"))
  }
  
  # plotting
  plot_pred(x = Xt, model = model$mod, n.ahead = n.ahead, level = level, 
            xlab = xlab, ylab = ylab, main = main)
  
  # Prediction 
  prediction = predict(model$mod, n.ahead = n.ahead)
  pred = prediction$pred
  se = prediction$se
  
  if(!is.null(level)){
    if(length(level) == 1){
      ci.up = pred+qnorm(1- (1-level)/2)*se
      ci.low = pred-qnorm(1- (1-level)/2)*se
      
      CI = matrix(c(ci.low, ci.up), nrow = length(ci.low), ncol = 2)
      attr(CI, "level") = level
      return(list(pred=pred, se=se, CI=CI))
    }
    if(length(level) == 2){
      ci.up1 = pred+qnorm(1- (1-level[1])/2)*se
      ci.up2 = pred+qnorm(1- (1-level[2])/2)*se
      ci.low1 = pred-qnorm(1- (1-level[1])/2)*se
      ci.low2 = pred-qnorm(1- (1-level[2])/2)*se
      ci.up = c(ci.up1, ci.up2)
      ci.low = c(ci.low1, ci.low2)
      
      CI1 = matrix(c(ci.low1, ci.up1), nrow = length(ci.low1), ncol = 2)
      CI2 = matrix(c(ci.low2, ci.up2), nrow = length(ci.low2), ncol = 2)
      attr(CI1, "level") = level[1]
      attr(CI2, "level") = level[2]
      return(list(pred=pred, se=se, CI1=CI1, CI2=CI2))
      
    }
    if(length(level) > 2){
      stop('This function can support up to 2 confidence levels of prediction.')
    }
  }else{
    level = c(0.50, 0.95)
    ci.up1 = pred+qnorm(1- (1-level[1])/2)*se
    ci.up2 = pred+qnorm(1- (1-level[2])/2)*se
    ci.low1 = pred-qnorm(1- (1-level[1])/2)*se
    ci.low2 = pred-qnorm(1- (1-level[2])/2)*se
    ci.up = c(ci.up1, ci.up2)
    ci.low = c(ci.low1, ci.low2)
    
    CI1 = matrix(c(ci.low1, ci.up1), nrow = length(ci.low1), ncol = 2)
    CI2 = matrix(c(ci.low2, ci.up2), nrow = length(ci.low2), ncol = 2)
    attr(CI1, "level") = level[1]
    attr(CI2, "level") = level[2]
    return(list(pred=pred, se=se, CI1=CI1, CI2=CI2))
  }
  
  
}




################################
### fitsimts: select
################################

#' @title Time Series Model Selection 
#' @description This function performs model fitting and calculates the model selection criteria to be plotted.
#' @param model A time series model.
#' @param Xt A \code{vector} of time series data. 
#' @param include.mean A \code{boolean} indicating whether to fit ARIMA with the mean or not.
#' @param criterion A \code{string} indicating the type of criterion to use in selecting the best model. 
#' Supported criteria include "aic" (AIC), "bic" (BIC) and "hq" (HQ).
#' @author Stéphane Guerrier and Yuming Zhang
#' @examples
#' set.seed(463)
#' Xt = gen_gts(100, AR(phi = c(0.2, -0.5, 0.4), sigma2 = 1))
#' select(AR(5), Xt)
#' @export
#' 
select = function(model, Xt, include.mean = TRUE, criterion = "aic"){
  # Check model
  if (!is.ts.model(model)){
    stop("The model provided is not a valid model.")
  }
  
  # Determine model
  model_code = model$obj.desc[[1]]
  
  if (sum(model_code[3:4]) > 0){
    stop("SARIMA are currently not supported.")
  }
  
  # Order of AR
  p = model_code[1]
  
  # Order of MA 
  q = model_code[2]
  if (q > 0){
    stop("MA are currently not supported.")
  }
  
  
  # model selection 
  out = select_arima_(Xt,
                      p = 0:p,
                      d = 0L,
                      q = 0L,
                      include.mean = include.mean)

  plot_select_ar(x=out)
  
  # return best model
  best_model(out, ic = criterion)
  
}














