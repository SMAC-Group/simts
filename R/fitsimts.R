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
#' Xt = gen_gts(300, MA(theta = 0.5, sigma2 = 1))
#' plot(Xt)
#' estimate(MA(1), Xt, method = "gmwm")
#' 
#' Xt = gen_gts(300, ARMA(ar = c(0.8, -0.5), ma = 0.5, sigma2 = 1))
#' plot(Xt)
#' estimate(ARMA(2,1), Xt, method = "rgmwm")
#' 
#' Xt = gen_gts(300, ARIMA(ar = c(0.8, -0.5), i = 1, ma = 0.5, sigma2 = 1))
#' plot(Xt)
#' estimate(ARIMA(2,1,1), Xt, method = "mle")
#' 
#' Xt = gen_gts(1000, SARIMA(ar = c(0.5, -0.25), i = 0, ma = 0.5, sar = -0.8, 
#' si = 1, sma = 0.25, s = 24, sigma2 = 1))
#' plot(Xt)
#' estimate(SARIMA(ar = 2, i = 0, ma = 1, sar = 1, si = 1, sma = 1, s = 24), Xt, 
#' method = "rgmwm")
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
             model_type =  model_name$simplified,
             model = model)
  class(out) = "fitsimts"
  out
}

#' Print fitsimts object
#' 
#' This function displays the information of a fitsimts object.
#' @method print fitsimts
#' @keywords internal
#' @param x   A \code{fitsimts} object
#' @param ... Other arguments passed to specific methods
#' @return Text output via print
#' @author Stéphane Guerrier and Yuming Zhang
#' @export
print.fitsimts = function(x, ...){
  cat("Fitted model: ")
  cat(x$model_name)
  cat("\n")
  cat("\n")
  cat("Estimated parameters:")
  cat("\n")
  print(x$mod)
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
#' check(model)
#' 
#' check(resids = rnorm(100))
#' 
#' Xt = gen_gts(1000, SARIMA(ar = c(0.5, -0.25), i = 0, ma = 0.5, sar = -0.8, 
#' si = 1, sma = 0.25, s = 24, sigma2 = 1))
#' model = estimate(SARIMA(ar = 2, i = 0, ma = 1, sar = 1, si = 1, sma = 1, s = 24), 
#' Xt, method = "rgmwm")
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
#' @param object A \code{fitsimts} object obtained from \code{estimate} function. 
#' @param n.ahead An \code{integer} indicating number of units of time ahead for which to make forecasts.
#' @param show_last A \code{integer} indicating the number of last observations to show in the forecast plot.
#' @param level A \code{double} or \code{vector} indicating confidence level of prediction interval.
#' By default, it uses the levels of 0.50 and 0.95.
#' @param xlab A \code{string} for the title of x axis.
#' @param ylab A \code{string} for the title of y axis.
#' @param main A \code{string} for the over all title of the plot.
#' @param plot A \code{logical} value. logical. If \code{TRUE}(the default) the predictions are plotted.
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
#' @export
#' 
predict.fitsimts = function(object, n.ahead = 10, show_last = 100, level = NULL, 
                            xlab = NULL, ylab = NULL, main = NULL, plot = TRUE, ...){
  Xt = object$Xt
  freq = attr(Xt, 'freq')
  end_time =  attr(Xt, 'end')
  if (length(Xt) > show_last){
    # Xt2 = Xt[((freq*end_time-show_last):(freq*end_time)) - attr(Xt, 'start') + 1]
    Xt2 = Xt[(length(Xt)-show_last+1):length(Xt)]

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
  if(object$method == "gmwm" | object$method == "rgmwm"){
    if(object$demean==TRUE){
      a = predict(object$mod, object$Xt - object$sample_mean, n.ahead = n.ahead) 
      pred = a$pred+object$sample_mean
      se = a$se
    }else{
      a = predict(object$mod, object$Xt, n.ahead=10) 
      pred = a$pred
      se = a$se
    }
  
    if (plot){
      plot_pred_gmwm(x = Xt, model=object, n.ahead = n.ahead, level = level, 
                     xlab = xlab, ylab = ylab, main = main) 
    }  
   
  }else{
    a = predict(object$mod, n.ahead = n.ahead)
    pred = a$pred
    se = a$se
    if (plot){
      plot_pred(x = Xt, model = object$mod, n.ahead = n.ahead, level = level, 
                xlab = xlab, ylab = ylab, main = main)
    }
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
  return(invisible(out))
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
#' @param plot A \code{boolean} indicating whether a model selection plot is returned or not.
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
select = function(model, Xt, include.mean = TRUE, criterion = "aic", plot = TRUE){
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
    
    if(plot == TRUE){plot_select_ar(x=out)}
  }else if (p == 0){
    out = select_arima_(Xt,
                        p = 0L,
                        d = intergrated,
                        q = 0:q,
                        include.mean = include.mean)
    
    if(plot == TRUE){plot_select_ma(x=out)}
  }else{
    out = select_arima_(Xt,
                        p = 0:p,
                        d = intergrated,
                        q = 0:q,
                        include.mean = include.mean)
    
    if(plot == TRUE){plot_select_arma(x=out)}
  }
  
  result = best_model(out, ic = criterion)
  class(result) = "fitsimts"
  return(invisible(result))
}

#' Summary of fitsimts object
#'
#' Displays summary information about fitsimts object
#' @method summary fitsimts
#' @param object       A \code{fitsimts} object
#' @param ...          Other arguments passed to specific methods
#' @return Estimated parameters values with confidence intervals and standard errors.
#' @export
#' @author Stéphane Guerrier
summary.fitsimts = function(object, ...){
  if (class(object$mod) == "Arima"){
    print(object)
  }else{
    cat("Fitted model: ")
    cat(object$model_name)
    cat("\n")
    cat("\n")
    cat("Estimated parameters:")
    cat("\n")
    print(object$mod)
    cat("95 % confidence intervals:")
    cat("\n")
    inter = summary(object$mod)$estimate
    print(inter)
    return(invisible(inter))
  }
}


#' Median Absolute Prediction Error
#'
#' This function calculates Median Absolute Prediction Error (MAPE), which assesses 
#' the prediction performance with respect to point forecasts of a given model. 
#' It is calculated based on one-step ahead prediction and reforecasting. 
#' @param model  A time series model.
#' @param Xt     A \code{vector} of time series data. 
#' @param start  A \code{numeric} indicating the starting proportion of the data
#' that is used for prediction. 
#' @param plot   A \code{boolean} indicating whether a model accuracy plot based
#' on MAPE is returned or not. 
#' @return The MAPE calculated based on one-step ahead prediction and reforecasting
#' is returned along with its standard deviation. 
#' @importFrom stats median
#' @export
#' @author Stéphane Guerrier and Yuming Zhang
MAPE = function(model, Xt, start = 0.8, plot = TRUE){
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
  
  # Non-seasonal integration
  intergrated = model_code[7]
  
  # Non-supported models
  if (sum(model_code[3:4]) > 0){
    stop("SARIMA are currently not supported.")
  }
  
  if (intergrated  > 0){
    stop("ARIMA are currently not supported.")
  }
  
  if (q > 0 && p > 0){
    stop("ARMA are currently not supported.")
  }
  
  n = length(Xt)
  index_start = floor(start*n)
  m = n - index_start
  ord = max(p, q)
  pred = matrix(NA, m, ord)
  mape = mape_sd = rep(NA, ord)

  for (i in 1:ord){
    for (j in 1:m){
      if (q == 0){
        pred[j,i] = as.numeric(predict(arima(Xt[1:(index_start+j-1)], c(i,0,0)), se.fit = FALSE))
      }else{
        pred[j,i] = as.numeric(predict(arima(Xt[1:(index_start+j-1)], c(0,0,i)), se.fit = FALSE))
      }
    }
    diff_pred = abs(pred[,i] - Xt[(index_start+1):n])
    mape[i] = median(diff_pred)
    mape_sd[i] = np_boot_sd_med(diff_pred)
  }
  
  if(plot){
  if (q == 0){
    myxlab = "Order of AR process"
  }else{
    myxlab = "Order of MA process"  
  }
    
  make_frame(x_range = c(1, ord), y_range = c(0.95*min(mape - mape_sd), 1.05*max(mape + mape_sd)), 
             xlab = myxlab, ylab = "MAPE",
             main = "Model Accuracy (MAPE)")
  
  polygon(c(1:ord, rev(1:ord)), c(mape - mape_sd, rev(mape + mape_sd)), 
          col = rgb(red = 0, green = 0.6, blue = 1, 0.15), border = NA)
  }
  
  lines(1:ord, mape, type = "b", pch = 16, cex = 1.25, col = "darkblue")
  
  min_mape = which.min(mape)
  min_upper =  mape[min_mape] + mape_sd[min_mape]
  min_one_sd_rule = which.max((mape < min_upper)*(ord:1))
  points(min_mape, mape[min_mape], pch = 16, col = "red2", cex = 2)
  abline(h = min_upper, lty = 2, col = "darkblue")
  if (min_mape != min_one_sd_rule){
    points(min_one_sd_rule, mape[min_one_sd_rule], pch = 16, col = "green3", cex = 2)
    legend("topright", c("MAPE", "Min MAPE", "One SD rule"), 
           lwd = c(1, NA, NA), pch = c(16, 16, 16), pt.cex = c(1.25, 1.5, 1.5),
           col = c("darkblue", "red2", "green3"), inset = c(0,0.10), bty = "n")
  }else{
    legend("topright", c("MAPE", "Min MAPE", "One SD rule"), 
           lwd = c(1, NA, NA), pch = c(16, 16, 16), pt.cex = c(1.25, 1.5, 1.5),
           col = c("darkblue", "red2", "red2"), inset = c(0,0.10), bty = "n")
  }
  
  return(invisible(list(mape = mape, sd = mape_sd)))
}

#' @title Bootstrap standard error for the median 
#'
#' @description Non-parametric bootstrap to obtain the standard of the median of
#' iid data.
#' @param x  A \code{vector} of data. 
#' @param B  A \code{numeric} indicating the number of simulations.
#' @return Bootstrap standard error for the median
#' @export
#' @importFrom stats median
np_boot_sd_med = function(x, B = 5000){
  set.seed(1982)
  res = rep(NA, B)
  n = length(x)
  
  for (i in 1:B){
    x_star = sample(x,replace = TRUE)
    res[i] = median(x_star)
  }
  sd(res)
}

#' Evalute a time series or a list of time series models
#'
#' This function calculates AIC, BIC and HQ or the MAPE for a list of time series
#' models. This function currently only supports models estimated by the MLE. 
#' @param models       A time series model or a list of time series models.
#' @param Xt           A time series (i.e gts object).
#' @param criterion    Either "IC" for AIC, BIC and HQ or "MAPE" for MAPE.
#' @param start        A \code{numeric} indicating the starting proportion of the data that 
#' is used for prediction (assuming criterion = "MAPE").
#' @param demean       A \code{boolean} indicating whether the model includes a mean / intercept term or not.
#' @param print         logical. If \code{TRUE} (the default) results are printed.
#' @return AIC, BIC and HQ or MAPE
#' @importFrom stats AIC median
#' @export
#' @author Stéphane Guerrier
#' @examples 
#' set.seed(18)
#' n = 300
#' Xt = gen_gts(n, AR(phi = c(0, 0, 0.8), sigma2 = 1))
#' evaluate(AR(1), Xt)
#' evaluate(list(AR(1), AR(3), MA(3), ARMA(1,2), 
#' SARIMA(ar = 1, i = 0, ma = 1, sar = 1, si = 1, sma = 1, s = 12)), Xt)
#' evaluate(list(AR(1), AR(3)), Xt, criterion = "MAPE")
evaluate = function(models, Xt, criterion = "IC", start = 0.8, demean = TRUE, print = TRUE){
  # Make Xt an gts obj
  if (!("gts" %in% class(Xt))){
    Xt = gts(Xt)
  }
  
  if (class(models) == "ts.model"){
    nb_models = 1
    models = list(models)
  }else if (class(models) == "list"){
    nb_models = length(models)
    if (sum(sapply(models, class) != "ts.model") > 0){
      stop("This funciton only supports list of time series models.")
    }
  }else{
    stop("This funciton only supports list of time series models or directley a time series model.")
  }
  
  if (criterion == "IC"){
    output = matrix(NA, nb_models, 3)
    dimnames(output)[[2]] = c("AIC", "BIC", "HQ")
  }else if(criterion == "MAPE"){
    output = matrix(NA, nb_models, 2)
    dimnames(output)[[2]] = c("MAPE", "SD")
  }else{
    stop("This funciton only supports the option criterion = 'IC' (information criteria) or 'MAPE'.")
  }
  
  model_names = rep("NA", nb_models)
  
  # Sample size
  n = length(Xt)
  
  if (criterion == "IC"){
    for (i in 1:nb_models){
      fit_current = estimate(models[[i]], Xt, demean = demean)
      model_names[i] = fit_current$model_name
      output[i, ] = c(AIC(fit_current$mod), AIC(fit_current$mod, k = log(n)), AIC(fit_current$mod, k = 2*log(log(n))))
    }
    dimnames(output)[[1]] = model_names
  }else{
    index_start = floor(start*n)
    m = n - index_start
    pred = matrix(NA, m, nb_models)
    
    for (i in 1:nb_models){
      for (j in 1:m){
        fit_current = estimate(models[[i]], gts(Xt[1:(index_start+j-1)]), demean = demean)
        pred[j,i] = as.numeric(predict(fit_current, plot = FALSE)$pred[1])
      }
      model_names[i] = fit_current$model_name
      diff_pred = abs(pred[,i] - Xt[(index_start+1):n])
      output[i,] = c(median(diff_pred), np_boot_sd_med(diff_pred))
    }
  }
  dimnames(output)[[1]] = model_names
  
  if (print == TRUE){
    print(output)
    cat("\n")
    k = ncol(output)
    
    if (nb_models > 1){
      if (criterion == "MAPE"){
        cat("MAPE suggests: ")
        cat(model_names[which.min(output[,1])])
      }else{
        cat("AIC suggests: ")
        cat(model_names[which.min(output[,1])])
        cat("\n")
        cat("BIC suggests: ")
        cat(model_names[which.min(output[,2])])
        cat("\n")
        cat("HQ suggests : ")
        cat(model_names[which.min(output[,3])])
      }
      cat("\n")
    }
  }
  
  invisible(output)
}


#' Akaike's Information Criterion
#'
#' This function calculates AIC, BIC or HQ for a fitsimts object. This function currently
#' only supports models estimated by the MLE. 
#' @param object  A fitsimts object.
#' @param k	      The penalty per parameter to be used; the default k = 2 is the classical AIC.
#' @param ...     Optionally more fitted model objects.
#' @return AIC, BIC or HQ
#' @importFrom stats AIC
#' @export
#' @author Stéphane Guerrier
#' @examples 
#' set.seed(1)
#' n = 300
#' Xt = gen_gts(n, AR(phi = c(0, 0, 0.8), sigma2 = 1))
#' mod = estimate(AR(3), Xt)
#' 
#' # AIC
#' AIC(mod)
#' 
#' # BIC
#' AIC(mod, k = log(n))
#' 
#' # HQ
#' AIC(mod, k = 2*log(log(n)))
AIC.fitsimts = function(object, k = 2, ...){
  if(object$method == "mle"){
    AIC(object$mod, k = k)
  }else{
    stop("This function is currently only implemented with ML estimates.")
  }
}



