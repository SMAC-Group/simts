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
#' plot(Xt)
#' estimate(AR(3), Xt)
#' 
#' Xt = gen_gts(300, ARMA(ar = c(0.8, -0.5), ma = 0.5, sigma2 = 1))
#' plot(Xt)
#' estimate(ARMA(2,1), Xt, method = "rgmwm")
#' 
#' Xt = gen_gts(300, ARIMA(ar = c(0.8, -0.5), i = 1, ma = 0.5, sigma2 = 1))
#' plot(Xt)
#' estimate(ARIMA(2,1,1), Xt, method = "mle")
#' 
#' Xt = gen_gts(1000, SARIMA(ar = c(0.5, -0.25), i = 0, ma = 0.5, sar = -0.8, si = 1, sma = 0.25, s = 24, sigma2 = 1))
#' plot(Xt)
#' estimate(SARIMA(ar = 2, i = 0, ma = 1, sar = 1, si = 1, sma = 1, s = 24), Xt, method = "rgmwm")
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
  
  # Order of AR
  p = model_code[1]
  
  # Order of MA 
  q = model_code[2]
  
  # Order of SAR
  P = model_code[3]
  
  # Order of SMA
  Q = model_code[4]
  
  # Seasonal period
  s = model_code[6]
  
  # Non-seasonal integration
  intergrated = model_code[7]
  
  # Seasonal integration
  seasonal_intergrated = model_code[8]
  
  # Get model
  model_name = simplified_print_SARIMA(p = p, i = intergrated, q = q, P = P, si = seasonal_intergrated, Q = Q, s = s)
  
  if (method == "mle" || method == "yule-walker"){
      if (method == "mle"){
        meth = "ML"
      }else{
        meth = "CSS"
      }
      mod = arima(as.numeric(Xt), c(p, intergrated, q), 
                  seasonal = list(order = c(P, seasonal_intergrated, Q), period = s),
                  method = meth, include.mean = demean)
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
  
  
  out = list(mod = mod, method = method, 
             demean = demean, Xt = Xt, 
             sample_mean = sample_mean, model_name = model_name$print,
             model_type =  model_name$simplified)
  class(out) = "fitsimts"
  out
}

#' @title Print fitsimts object
#' @description This function displays the information of a fitsimts object.
#' @method print fitsimts
#' @keywords internal
#' @param out   A \code{fitsimts} object
#' @return Text output via print
#' @author Stéphane Guerrier and Yuming Zhang
#' @export
print.fitsimts = function(out, ...){
  cat("Fitted model: ")
  cat(out$model_name)
  cat("\n")
  cat("\n")
  cat("Estimated parameters:")
  cat("\n")
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
#' @param model A \code{fitsimts}, \code{lm} or \code{gam} object. 
#' @param resids A \code{vector} of residuals for diagnostics. 
#' @param simple A \code{boolean} indicating whether to return simple diagnostic plots or not. 
#' @author Stéphane Guerrier and Yuming Zhang
#' @examples
#' Xt = gen_gts(300, AR(phi = c(0, 0, 0.8), sigma2 = 1))
#' model = estimate(AR(3), Xt)
#' check(model = model)
#' check(model = model, simple = TRUE)
#' 
#' Xt = gen_gts(300, AR(phi = c(0, 0, 0.8), sigma2 = 1))
#' model = arima(Xt, order = c(3,0,0), include.mean = TRUE)
#' residuals = resid(model)
#' check(resids = residuals)
#' 
#' Xt = gen_gts(1000, SARIMA(ar = c(0.5, -0.25), i = 0, ma = 0.5, sar = -0.8, si = 1, sma = 0.25, s = 24, sigma2 = 1))
#' model = estimate(SARIMA(ar = 2, i = 0, ma = 1, sar = 1, si = 1, sma = 1, s = 24), Xt, method = "rgmwm")
#' check(model)
#' check(model, simple=TRUE)
#' 
#' @export
#' 
check = function(model = NULL, resids = NULL, simple = FALSE){
  if(!is.null(model) & !is.null(resids)){
    warning("Both model and residuals are provided. The function will only use 'model' for diagnostics. ")
    resids = NULL
  }
  
  if(!is.null(model) & is.null(resids)){
    if(class(model) == "fitsimts"){
      if (simple){
        simple_diag_plot(Xt = model$Xt, model = model)
      }else{
        diag_plot(Xt = model$Xt, model = model)
      }
    }
    
    if("lm" %in% class(model)){
      if(simple){warning("If 'lm' model is considered, only the full diagnostic plots can be provided, not the simple version.")}
      resids = resid(model)
      diag_plot(resids = resids)
    }
    
  }
  
  if(is.null(model) & !is.null(resids)){
    if (simple){warning("If only residuals are provided, only the full diagnostic plots can be provided, not the simple version.")}
    diag_plot(resids = resids)
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
#' @param ... Additional arguments.
#' @method predict fitsimts
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
#' predict(model, n.ahead = 20, level = c(0.50, 0.80, 0.95))
#' 
#' Xt = gen_gts(300, SARIMA(ar = c(0.5, -0.25), i = 0, ma = 0.5, sar = -0.8, si = 1, sma = 0.25, s = 24, sigma2 = 1))
#' model = estimate(SARIMA(ar = 2, i = 0, ma = 1, sar = 1, si = 1, sma = 1, s = 24), Xt, method = "rgmwm")
#' predict(model, n.ahead = 10)
#' predict(model, n.ahead = 10, level = c(0.50, 0.80, 0.95))
#' 
#' @export
#' 
predict.fitsimts = function(model, n.ahead = 10, show_last = 100, level = NULL, 
                            xlab = NULL, ylab = NULL, main = NULL, ...){
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
  
  
  # Prediction 
  if(model$method == "gmwm" | model$method == "rgmwm"){
    if(model$demean==TRUE){
      a = predict(model$mod, model$Xt - model$sample_mean, n.ahead = n.ahead) 
      pred = a$pred+model$sample_mean
      se = a$se
    }else{
      a = predict(model$mod, model$Xt, n.ahead=10) 
      pred = a$pred
      se = a$se
    }
   plot_pred_gmwm(x = Xt, model=model, n.ahead = n.ahead, level = level, 
                  xlab = xlab, ylab = ylab, main = main) 
  }else{
    a = predict(model$mod, n.ahead = n.ahead)
    pred = a$pred
    se = a$se
    plot_pred(x = Xt, model = model$mod, n.ahead = n.ahead, level = level, 
              xlab = xlab, ylab = ylab, main = main)
  }
  
  if(is.null(level)){
    level = c(0.5, 0.95)
  }
  
  out = list(pred=pred, se=se)
  m = length(level)
  for (i in 1:m){
    ci.up = pred+qnorm(1- (1-level[i])/2)*se
    ci.low = pred-qnorm(1- (1-level[i])/2)*se
    CI = matrix(c(ci.low, ci.up), nrow = length(ci.low), ncol = 2)
    out[[(i + 2)]] = CI
  }
  
  names(out) = c("pred", "se", paste("CI", level, sep = ""))
  return(out)
}




################################
### fitsimts: select
################################

#' @title Time Series Model Selection 
#' @description This function performs model fitting and calculates the model selection criteria to be plotted.
#' @param model A time series model (only ARIMA are currently supported).
#' @param Xt A \code{vector} of time series data. 
#' @param include.mean A \code{boolean} indicating whether to fit ARIMA with the mean or not.
#' @param criterion A \code{string} indicating which model selection criterion should be used (possible values: \code{"aic"} (default), \code{"bic"}, \code{"hq"}).
#' @author Stéphane Guerrier and Yuming Zhang
#' @export
#' @examples
#' set.seed(763)
#' Xt = gen_gts(100, AR(phi = c(0.2, -0.5, 0.4), sigma2 = 1))
#' select(AR(5), Xt, include.mean = FALSE)
#' 
#' Xt = gen_gts(100, MA(theta = c(0.2, -0.5, 0.4), sigma2 = 1))
#' select(MA(5), Xt, include.mean = FALSE)
#' 
#' Xt = gen_gts(500, ARMA(ar = 0.5, ma = c(0.5, -0.5, 0.4), sigma2 = 1))
#' select(ARMA(5,3), Xt, criterion = "hq", include.mean = FALSE)
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
  
  # Non-seasonal integration
  intergrated = model_code[7]
  
  # model selection 
  if (q == 0){
    out = select_arima_(Xt,
                        p = 0:p,
                        d = intergrated,
                        q = 0L,
                        include.mean = include.mean)
    
    plot_select_ar(x=out)
  }else if (p == 0){
    out = select_arima_(Xt,
                        p = 0L,
                        d = intergrated,
                        q = 0:q,
                        include.mean = include.mean)
    
    plot_select_ma(x=out)
  }else{
    out = select_arima_(Xt,
                        p = 0:p,
                        d = intergrated,
                        q = 0:q,
                        include.mean = include.mean)
    
    plot_select_arma(x=out)
  }
  
  best_model(out, ic = criterion)
}














