################################
### fitsimts: estimate
################################

#' @title Fit a Time Series Model to Data
#' @description This function can fit a time series model to data using different methods. 
#' @param model A time series model.
#' @param Xt A \code{vector} of time series data. 
#' @param method A \code{string} indicating the method used for model fitting. 
#' Supported methods include \code{mle}, \code{yule-walker}, and \code{rgmwm}. 
#' @param demean A \code{boolean} indicating whether the model includes a mean / intercept term or not.
#' @note If you are going to use \code{rgmwm} as your model fitting method, please be sure that 
#' the R package \code{gmwm2} is loaded. \code{gmwm2} can be downloaded using R command: devtools::install_github("smac-group/gmwm2"). 
#' @author Stéphane Guerrier
#' @examples
#' Xt = gen_gts(300, AR(phi = c(0, 0, 0.8), sigma2 = 1))
#' model = estimate(AR(3), Xt)
#' model = estimate(AR(3), Xt, method = "rgmwm")
#' @export

estimate = function(model, Xt, method = "mle", demean = TRUE){
  all_method = c("mle", "yule-walker", "rgmwm")
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
      mod = gmwm(model, Xt, robust = TRUE)
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
#' @author Stéphane Guerrier
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


