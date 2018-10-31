#' Generalized Method of Wavelet Moments (GMWM) for IMUs, ARMA, SSM, and Robust
#'
#' Performs estimation of time series models by using the GMWM estimator.
#' @param model      A \code{ts.model} object containing one of the allowed models.
#' @param data       A \code{matrix} or \code{data.frame} object with only column
#'                   (e.g. \eqn{N \times 1}{ N x 1 }), a \code{lts} object,
#'                   or a \code{gts} object.
#' @param model.type A \code{string} containing the type of GMWM needed:
#'                   \code{"imu"} or \code{"ssm"}.
#' @param compute.v  A \code{string} indicating the type of covariance matrix
#'                   solver. Valid values are:
#'                    \code{"fast"}, \code{"bootstrap"},
#'                    \code{"diag"} (asymptotic diag),
#'                    \code{"full"} (asymptotic full). By default, the program
#'                   will fit a "fast" model.
#' @param alpha      A \code{double} between 0 and 1 that correspondings to the
#'                   \eqn{\frac{\alpha}{2}}{alpha/2} value for the wavelet
#'                   confidence intervals.
#' @param robust     A \code{boolean} indicating whether to use the robust
#'                   computation (\code{TRUE}) or not (\code{FALSE}).
#' @param eff        A \code{double} between 0 and 1 that indicates the
#'                   efficiency.
#' @param G          An \code{integer} to sample the space for IMU and SSM
#'                   models to ensure optimal identitability.
#' @param K          An \code{integer} that controls how many times the
#'                   bootstrapping procedure will be initiated.
#' @param H          An \code{integer} that indicates how many different
#'                   samples the bootstrap will be collect.
#' @param seed       An \code{integer} that controls the reproducibility of the
#'                   auto model selection phase.
#' @param freq       A \code{double} that indicates the sampling frequency. By
#'                   default, this is set to 1 and only is important if \code{GM()}
#'                   is in the model
#' @return A \code{gmwm} object with the structure:
#' \describe{
#'  \item{estimate}{Estimated Parameters Values from the GMWM Procedure}
#'  \item{init.guess}{Initial Starting Values given to the Optimization Algorithm}
#'  \item{wv.empir}{The data's empirical wavelet variance}
#'  \item{ci.low}{Lower Confidence Interval}
#'  \item{ci.high}{Upper Confidence Interval}
#'  \item{orgV}{Original V matrix}
#'  \item{V}{Updated V matrix (if bootstrapped)}
#'  \item{omega}{The V matrix inversed}
#'  \item{obj.fun}{Value of the objective function at Estimated Parameter Values}
#'  \item{theo}{Summed Theoretical Wavelet Variance}
#'  \item{decomp.theo}{Decomposed Theoretical Wavelet Variance by Process}
#'  \item{scales}{Scales of the GMWM Object}
#'  \item{robust}{Indicates if parameter estimation was done under robust or classical}
#'  \item{eff}{Level of efficiency of robust estimation}
#'  \item{model.type}{Models being guessed}
#'  \item{compute.v}{Type of V matrix computation}
#'  \item{augmented}{Indicates moments have been augmented}
#'  \item{alpha}{Alpha level used to generate confidence intervals}
#'  \item{expect.diff}{Mean of the First Difference of the Signal}
#'  \item{N}{Length of the Signal}
#'  \item{G}{Number of Guesses Performed}
#'  \item{H}{Number of Bootstrap replications}
#'  \item{K}{Number of V matrix bootstraps}
#'  \item{model}{\code{ts.model} supplied to gmwm}
#'  \item{model.hat}{A new value of \code{ts.model} object supplied to gmwm}
#'  \item{starting}{Indicates whether the procedure used the initial guessing approach}
#'  \item{seed}{Randomization seed used to generate the guessing values}
#'  \item{freq}{Frequency of data}
#' }
#' @export
#' @details
#' This function is under work. Some of the features are active. Others... Not so much.
#'
#' The V matrix is calculated by:
#' \eqn{diag\left[ {{{\left( {Hi - Lo} \right)}^2}} \right]}{diag[(Hi-Lo)^2]}.
#'
#' The function is implemented in the following manner:
#' 1. Calculate MODWT of data with levels = floor(log2(data))
#' 2. Apply the brick.wall of the MODWT (e.g. remove boundary values)
#' 3. Compute the empirical wavelet variance (WV Empirical).
#' 4. Obtain the V matrix by squaring the difference of the WV Empirical's Chi-squared confidence interval (hi - lo)^2
#' 5. Optimize the values to obtain \eqn{\hat{\theta}}{theta^hat}
#' 6. If FAST = TRUE, return these results. Else, continue.
#'
#'Loop  k = 1 to K
#' Loop h = 1 to H
#' 7. Simulate xt under \eqn{F_{\hat{\theta}}}{F_theta^hat}
#' 8. Compute WV Empirical
#' END
#' 9. Calculate the covariance matrix
#' 10. Optimize the values to obtain \eqn{\hat{\theta}}{theta^hat}
#'END
#' 11. Return optimized values.
#'
#'
#' The function estimates a variety of time series models. If type = "imu" or "ssm", then
#' parameter vector should indicate the characters of the models that compose the latent or state-space model. The model
#' options are:
#' \describe{
#'   \item{"AR1"}{a first order autoregressive process with parameters \eqn{(\phi,\sigma^2)}{phi, sigma^2}}
#'   \item{"GM"}{a guass-markov process \eqn{(\beta,\sigma_{gm}^2)}{beta, sigma[gm]^2}}
#'   \item{"ARMA"}{an autoregressive moving average process with parameters \eqn{(\phi _p, \theta _q, \sigma^2)}{phi[p], theta[q], sigma^2}}
#'   \item{"DR"}{a drift with parameter \eqn{\omega}{omega}}
#'   \item{"QN"}{a quantization noise process with parameter \eqn{Q}}
#'   \item{"RW"}{a random walk process with parameter \eqn{\sigma^2}{sigma^2}}
#'   \item{"WN"}{a white noise process with parameter \eqn{\sigma^2}{sigma^2}}
#' }
#' If only an ARMA() term is supplied, then the function takes conditional least squares as starting values
#' If robust = TRUE the function takes the robust estimate of the wavelet variance to be used in the GMWM estimation procedure.
gmwm = function(model, data, model.type="ssm", compute.v="auto",
                robust=FALSE, eff=0.6, alpha = 0.05, seed = 1337, G = NULL, K = 1, H = 100,
                freq = 1){


  # Check data object
  if(simts::is.gts(data)){
    freq = attr(data, 'freq')
    data = data[,1]

  } else if(simts::is.lts(data)){
    freq = attr(data, 'freq')
    data = data[ ,ncol(data)]

  } else if((simts::is.imu(data) || is.data.frame(data) || is.matrix(data))){
    if(ncol(data) > 1){
      stop("`gmwm` and `gmwm.imu` can only process one signal at a time.")
    }
    if(simts::is.imu(data)){
      freq = attr(data, 'freq')
    }
  } else if(is.ts(data)){
    freq = attr(data,'tsp')[3]
  }

  # Do we have a valid model?
  if(!simts::is.ts.model(model)){
    stop("`model` must be created from a `ts.model` object using a supported component (e.g. AR1(), ARMA(p,q), DR(), RW(), QN(), and WN(). ")
  }

  # Information Required by GMWM:
  desc = model$desc

  obj = model$obj.desc

  np = model$plength

  N = length(data)

  starting = model$starting

  # Input guessing
  if((is.null(G) & starting) || !simts::is.whole(G)){
    if(N > 10000){
      G = 1e6
    }else{
      G = 20000
    }
  }else if(!starting){
    G = 0
  }

  # For reproducibility
  set.seed(seed)

  num.models = count_models(desc)

  # Identifiability issues
  if(any(num.models[c("DR","QN","RW","WN")]  > 1)){
    stop("Two instances of either: DR, QN, RW, or WN has been detected. As a result, the model will have identifiability issues. Please submit a new model.")
  }

  if(num.models["GM"]> 0 & num.models["AR1"] > 0){
    stop("Please either use `GM()` or `AR1()` model components. Do not mix them.")
  }

  # Model type issues
  model.type = tolower(model.type)
  if(model.type != "imu" && model.type != "ssm"){
    stop("Model Type must be either `ssm` or `imu`!")
  }

  # Verify Scales and Parameter Space
  nlevels =  floor(log2(length(data)))

  if(np > nlevels){
    stop("Please supply a longer signal / time series in order to use the GMWM.",
         "This is because we need at least the same number of scales as",
         "parameters to estimate.")
  }

  if(robust){
    np = np+1
    if(np > nlevels){
      stop("Please supply a longer signal / time series in order to use the GMWM. This is because we at least need the same number of scales as parameters to estimate.")
    }

    if(eff > 0.99){
      stop("The efficiency specified is too close to the classical case. Use `robust = FALSE`")
    }
  }


  # Compute fast covariance if large sample, otherwise, bootstrap.
  if(compute.v == "auto" || ( compute.v != "fast" && compute.v != "diag" &&
                              compute.v != "full" && compute.v != "bootstrap" )){
    compute.v = "fast"
  }

  theta = model$theta

  detected_gm = any(model$desc == "GM")

  if(detected_gm && freq == 1){
    warning("'freq' is set to 1 by default this impacts the `GM()` parameters. See ?GM for more details.")
  }

  # Convert from GM to AR1
  if(!starting && detected_gm){
    theta = conv.gm.to.ar1(theta, model$process.desc, freq)
  }

  out = gmwm_master_cpp(data, theta, desc, obj, model.type, model$starting,
              alpha = alpha, compute_v = compute.v, K = K, H = H, G = G,
              robust=robust, eff = eff)

  estimate = out[[1]]
  rownames(estimate) = model$process.desc
  colnames(estimate) = "Estimates"

  init.guess = out[[2]]
  rownames(init.guess) = model$process.desc
  colnames(init.guess) = "Starting"

  # Convert from AR1 to GM
  if(detected_gm){
    estimate[,1] = conv.ar1.to.gm(estimate[,1], model$process.desc, freq)
    init.guess[,1] = conv.ar1.to.gm(init.guess[,1], model$process.desc, freq)
  }

  # Wrap this into the C++ Lib
  scales = scales_cpp(nlevels)

  # Create a new model object.
  model.hat = model

  model.hat$starting = F

  model.hat$theta = as.numeric(estimate)

  # Release model
  out = structure(list(estimate = estimate,
                       init.guess = init.guess,
                       wv.empir = out[[3]],
                       ci.low = out[[4]],
                       ci.high = out[[5]],
                       orgV = out[[7]],
                       V = out[[6]],
                       omega = out[[12]],
                       obj.fun = out[[11]],
                       theo = out[[9]],
                       decomp.theo = out[[10]],
                       scales = scales,
                       robust = robust,
                       eff = eff,
                       model.type = model.type,
                       compute.v = compute.v,
                       alpha = alpha,
                       expect.diff = out[[8]],
                       N = N,
                       G = G,
                       H = H,
                       K = K,
                       model = model,
                       model.hat = model.hat,
                       starting = model$starting,
                       seed = seed,
                       freq = freq,
                       dr.slope = out[[13]]), class = "gmwm")
  invisible(out)
}

#' Update (Robust) GMWM object for IMU or SSM
#'
#' Provides a way to estimate different models over the previously estimated
#' wavelet variance values and covariance matrix.
#' @param object  A \code{gmwm} object.
#' @param model   A \code{ts.model} object containing one of the allowed models
#' @param ...     Additional parameters (not used)
#' @return A \code{gmwm} object with the structure:
#' \describe{
#'  \item{estimate}{Estimated Parameters Values from the GMWM Procedure}
#'  \item{init.guess}{Initial Starting Values given to the Optimization Algorithm}
#'  \item{wv.empir}{The data's empirical wavelet variance}
#'  \item{ci.low}{Lower Confidence Interval}
#'  \item{ci.high}{Upper Confidence Interval}
#'  \item{orgV}{Original V matrix}
#'  \item{V}{Updated V matrix (if bootstrapped)}
#'  \item{omega}{The V matrix inversed}
#'  \item{obj.fun}{Value of the objective function at Estimated Parameter Values}
#'  \item{theo}{Summed Theoretical Wavelet Variance}
#'  \item{decomp.theo}{Decomposed Theoretical Wavelet Variance by Process}
#'  \item{scales}{Scales of the GMWM Object}
#'  \item{robust}{Indicates if parameter estimation was done under robust or classical}
#'  \item{eff}{Level of efficiency of robust estimation}
#'  \item{model.type}{Models being guessed}
#'  \item{compute.v}{Type of V matrix computation}
#'  \item{augmented}{Indicates moments have been augmented}
#'  \item{alpha}{Alpha level used to generate confidence intervals}
#'  \item{expect.diff}{Mean of the First Difference of the Signal}
#'  \item{N}{Length of the Signal}
#'  \item{G}{Number of Guesses Performed}
#'  \item{H}{Number of Bootstrap replications}
#'  \item{K}{Number of V matrix bootstraps}
#'  \item{model}{\code{ts.model} supplied to gmwm}
#'  \item{model.hat}{A new value of \code{ts.model} object supplied to gmwm}
#'  \item{starting}{Indicates whether the procedure used the initial guessing approach}
#'  \item{seed}{Randomization seed used to generate the guessing values}
#'  \item{freq}{Frequency of data}
#' }
update.gmwm = function(object, model, ...){
  # Do we have a valid model?
  if(!is.ts.model(model)){
    stop("`model` must be created from a `ts.model` object using a supported component (e.g. AR1(), ARMA(p,q), DR(), RW(), QN(), and WN(). ")
  }

  # Information Required by GMWM:
  desc = model$desc

  obj = model$obj.desc

  np = model$plength

  # Information used in summary.gmwm:
  summary.desc = model$desc

  num.models = count_models(desc)

  # Set seed for reproducibility

  set.seed(object$seed)

  # Identifiability issues
  if(any( num.models[c("DR","QN","RW","WN")] >1)){
    stop("Two instances of either: DR, QN, RW, or WN has been detected. As a result, the model will have identifiability issues. Please submit a new model.")
  }

  if(num.models["GM"]> 0 & num.models["AR1"] > 0){
    stop("Please either use `GM()` or `AR1()` model components. Do not mix them.")
  }

  # ID error:
  if( sum(num.models) == 1 & num.models["SARIMA"] == 1 & model$starting){
    warning("ARMA starting guesses using update.gmwm are NOT based on CSS but an alternative algorithm.")
  }

  if(np > length(object$scales)){
    stop("Please supply a longer signal / time series in order to use the GMWM. This is because we need  at least  the same number of scales as parameters to estimate.")
  }

  if(object$robust){
    np = np+1
    if(np > length(object$scales)){
      stop("Please supply a longer signal / time series in order to use the GMWM. This is because we need one additional scale since robust requires the amount of parameters + 1 to estimate.")
    }
  }


  detected_gm = any(model$desc == "GM")

  # Convert from GM to AR1
  if(!object$starting && detected_gm){
    model$theta = conv.gm.to.ar1(model$theta, model$process.desc, object$freq)
  }

  out = gmwm_update_cpp(model$theta,
              desc, obj,
              object$model.type, object$N, object$expect.diff, object$dr.slope,
              object$orgV, object$scales, cbind(object$wv.empir,object$ci.low,object$ci.high), # needed WV info
              model$starting,
              object$compute.v, object$K, object$H,
              object$G,
              object$robust, object$eff)

  estimate = out[[1]]

  model.hat = model

  model.hat$starting = F

  model.hat$theta = as.numeric(estimate)

  object$model.hat = model.hat

  rownames(estimate) = model$process.desc
  init.guess = out[[2]]
  rownames(init.guess) = model$process.desc

  # Convert from AR1 to GM
  if(detected_gm){
    estimate[,1] = conv.ar1.to.gm(estimate[,1], model$process.desc, object$freq)
    init.guess[,1] = conv.ar1.to.gm(init.guess[,1], model$process.desc, object$freq)
  }

  object$estimate = estimate
  object$init.guess = init.guess

  object$V = out[[3]]
  object$theo = out[[4]]
  object$decomp.theo = out[[5]]

  object$starting = model$starting

  invisible(object)
}


#' GMWM for (Robust) Inertial Measurement Units (IMUs)
#'
#' Performs the GMWM estimation procedure using a parameter transform and sampling
#' scheme specific to IMUs.
#' @param model     A \code{ts.model} object containing one of the allowed models.
#' @param data      A \code{matrix} or \code{data.frame} object with only column (e.g. \eqn{N \times 1}{ N x 1 }), or a \code{lts} object, or a \code{gts} object.
#' @param compute.v A \code{string} indicating the type of covariance matrix solver. "fast", "bootstrap", "asymp.diag", "asymp.comp", "fft"
#' @param robust    A \code{boolean} indicating whether to use the robust computation (TRUE) or not (FALSE).
#' @param eff       A \code{double} between 0 and 1 that indicates the efficiency.
#' @param ...       Other arguments passed to the main gmwm function
#' @details
#' This version of the gmwm function has customized settings
#' ideal for modeling with an IMU object. If you seek to model with an Gauss
#' Markov, \code{GM}, object. Please note results depend on the
#' \code{freq} specified in the data construction step within the
#' \code{imu}. If you wish for results to be stable but lose the
#' ability to interpret with respect to \code{freq}, then use
#' \code{AR1} terms.
#' @return A \code{gmwm} object with the structure:
#' \describe{
#'  \item{estimate}{Estimated Parameters Values from the GMWM Procedure}
#'  \item{init.guess}{Initial Starting Values given to the Optimization Algorithm}
#'  \item{wv.empir}{The data's empirical wavelet variance}
#'  \item{ci.low}{Lower Confidence Interval}
#'  \item{ci.high}{Upper Confidence Interval}
#'  \item{orgV}{Original V matrix}
#'  \item{V}{Updated V matrix (if bootstrapped)}
#'  \item{omega}{The V matrix inversed}
#'  \item{obj.fun}{Value of the objective function at Estimated Parameter Values}
#'  \item{theo}{Summed Theoretical Wavelet Variance}
#'  \item{decomp.theo}{Decomposed Theoretical Wavelet Variance by Process}
#'  \item{scales}{Scales of the GMWM Object}
#'  \item{robust}{Indicates if parameter estimation was done under robust or classical}
#'  \item{eff}{Level of efficiency of robust estimation}
#'  \item{model.type}{Models being guessed}
#'  \item{compute.v}{Type of V matrix computation}
#'  \item{augmented}{Indicates moments have been augmented}
#'  \item{alpha}{Alpha level used to generate confidence intervals}
#'  \item{expect.diff}{Mean of the First Difference of the Signal}
#'  \item{N}{Length of the Signal}
#'  \item{G}{Number of Guesses Performed}
#'  \item{H}{Number of Bootstrap replications}
#'  \item{K}{Number of V matrix bootstraps}
#'  \item{model}{\code{ts.model} supplied to gmwm}
#'  \item{model.hat}{A new value of \code{ts.model} object supplied to gmwm}
#'  \item{starting}{Indicates whether the procedure used the initial guessing approach}
#'  \item{seed}{Randomization seed used to generate the guessing values}
#'  \item{freq}{Frequency of data}
#' }
gmwm_imu = function(model, data, compute.v = "fast", robust = F, eff = 0.6, ...){

  x = gmwm(model = model,
           data = data,
           compute.v = compute.v,
           model.type = "imu",
           robust = robust,
           eff = eff,
           ...
  )
  class(x) = c("gmwm_imu","gmwm")

  x
}


#' GMWM for Robust/Classical Comparison
#'
#' Creates a \code{rgmwm} object to compare the results generated by robust/classical method.
#' @param model A \code{ts.model} object containing one of the allowed models.
#' @param data  A \code{matrix} or \code{data.frame} object with only one column (e.g. \eqn{N \times 1}{ N x 1 }), or a \code{lts} object, or a \code{gts} object.
#' @param eff   A \code{double vector} between 0 and 1 that indicates the efficiency.
#' @param ...   Other arguments passed to the main \code{gmwm} function.
#' @return A \code{rgmwm} object
#' @details
#' By default, the \code{rgmwm} function will fit a classical \code{gmwm}
#' object. From there, the user has the ability to specify any \code{eff} that is
#' less than or equal to 0.99.
rgmwm = function(model, data, eff = c(0.9, 0.8, 0.6), ...){

  len = length(eff) + 1
  obj.list = vector('list', length = len)

  # Allocate i = 1 for classical, i > 1 are varying robust efficiencies.
  for(i in seq_len(len)) {
    if(i != 1L) {
      obj.list[[i]] = gmwm(model = model, data = data,
                           robust = TRUE, eff = eff[i-1], ...)
    } else {
      obj.list[[i]] = gmwm(model = model, data = data, robust = FALSE, ...)
    }
  }

  class(obj.list) = 'rgmwm'
  obj.list
}

#' Print gmwm object
#'
#' Displays information about GMWM object
#' @method print gmwm
#' @keywords internal
#' @param x   A \code{GMWM} object
#' @param ... Other arguments passed to specific methods
#' @return Text output via print
#' @export
#' @author JJB
print.gmwm = function(x, ...){
  cat("Model Information: \n")
  print(x$estimate)

  cat("\n* The initial values of the parameters used in the minimization of the GMWM objective function \n  were",
      {if(x$starting) paste0("generated by the program underneath seed: ",x$seed,".") else "supplied by YOU!"},"\n\n")
}


#' Summary of GMWM object
#'
#' Displays summary information about GMWM object
#' @method summary gmwm
#' @param object       A \code{GMWM} object
#' @param inference    A value containing either: NULL (auto), TRUE, or FALSE
#' @param bs.gof       A value containing either: NULL (auto), TRUE, FALSE
#' @param bs.gof.p.ci  A value containing either: NULL (auto), TRUE, FALSE
#' @param bs.theta.est A value containing either: NULL (auto), TRUE, FALSE
#' @param bs.ci        A value containing either: NULL (auto), TRUE, FALSE
#' @param B            An \code{int} that indicates how many bootstraps should be performed.
#' @param ...          Other arguments passed to specific methods
#' @return A \code{summary.gmwm} object with:
#' \describe{
#'  \item{estimate}{Estimated Theta Values}
#'  \item{testinfo}{Goodness of Fit Information}
#'  \item{inference}{Inference performed? T/F}
#'  \item{bs.gof}{Bootstrap GOF? T/F}
#'  \item{bs.gof.p.ci}{Bootstrap GOF P-Value CI? T/F}
#'  \item{bs.theta.est}{Bootstrap Theta Estimates? T/F}
#'  \item{bs.ci}{Bootstrap CI? T/F}
#'  \item{starting}{Indicates if program supplied initial starting values}
#'  \item{seed}{Seed used during guessing / bootstrapping}
#'  \item{obj.fun}{Value of obj.fun at minimized theta}
#'  \item{N}{Length of Time Series}
#' }
#' @export
#' @author JJB
summary.gmwm = function(object, inference = NULL,
                        bs.gof = NULL,  bs.gof.p.ci = NULL,
                        bs.theta.est = NULL, bs.ci = NULL,
                        B = 100, ...){

  # Set a different seed to avoid dependency.
  set.seed(object$seed+5)

  out = object$estimate

  N = object$N

  # Enable values if small time series.
  auto = if(N > 10000) FALSE else TRUE

  # Auto set values
  if(is.null(inference)){
    inference = auto
  }

  if(is.null(bs.gof)){
    bs.gof= if(inference) auto else F
  }

  if(is.null(bs.gof.p.ci)){
    bs.gof.p.ci = if(inference) auto else F
  }

  if(is.null(bs.theta.est)){
    bs.theta.est = if(inference) auto else F
  }

  if(is.null(bs.ci)){
    bs.ci = if(inference) auto else F
  }

  if("ARMA" %in% object$model$desc){
    if(bs.ci == FALSE){
      warning(paste0("The numerical derivative of ARMA(p,q), where p > 1 and q > 1, may be inaccurate leading to inappropriate CIs.\n",
                     "Consider using the bs.ci = T option on the summary function."))
    }
  }

  if(inference){

    # Convert from GM to AR1
    if(any(object$model$desc == "GM")){
      object$estimate[,1] = conv.gm.to.ar1(object$estimate[,1], object$model$process.desc, object$freq)
    }

    mm = get_summary(object$estimate,
               object$model$desc, object$model$obj.desc,
               object$model.type,
               object$wv.empir, object$theo,object$scales,
               object$V, solve(object$orgV), object$obj.fun,
               N, object$alpha,
               object$robust, object$eff,
               inference, F, # fullV is always false. Need same logic updates.
               bs.gof, bs.gof.p.ci, bs.theta.est, bs.ci,
               B)
  }else{
    mm = vector('list',3)
    mm[1:3] = NA
  }

  if(inference){
    out.coln = colnames(out)
    out = cbind(out, mm[[1]])
    colnames(out) = c(out.coln, "CI Low", "CI High", "SE")
    # Convert from AR1 to GM
    idx_gm = (object$model$desc == "GM")
    if(any(idx_gm)) {
      out[,2:3] = apply(out[,2:3], 2, FUN = conv.ar1.to.gm,
                        process.desc = object$model$process.desc,
                        freq = object$freq)
      # To do: Add delta method transform here for sigma2
    }

  }

  x = structure(list(estimate=out,
                     testinfo=mm[[2]],
                     inference = inference,
                     bs.gof = bs.gof,
                     bs.gof.p.ci = bs.gof.p.ci,
                     bs.theta.est = bs.theta.est,
                     bs.ci = bs.ci,
                     starting = object$starting,
                     seed = object$seed,
                     obj.fun = object$obj.fun,
                     N = N,
                     freq = object$freq), class = "summary.gmwm")

  x
}

#' Print summary.gmwm object
#'
#' Displays summary information about GMWM object
#' @method print summary.gmwm
#' @keywords internal
#' @param x   A \code{GMWM} object
#' @param ... Other arguments passed to specific methods
#' @return Text output via print
#' @author JJB
print.summary.gmwm = function(x, ...){

  cat("Model Information: \n")
  print(x$estimate)
  if(x$bs.theta.est){
    cat("\n> The parameter estimates shown are bootstrapped! To use these results, please save the summary object.")
  }

  cat("\n* The initial values of the parameters used in the minimization of the GMWM objective function \n  were",
      {if(x$starting) paste0("generated by the program underneath seed: ",x$seed,".") else "given by YOU!"},"\n\n")

  cat(paste0("Objective Function: ", round(x$obj.fun,4),"\n\n"))


  if(x$inference){
    cat(paste0({if(x$bs.gof) "Bootstrapped" else "Asymptotic"}," Goodness of Fit: \n"))
    if(x$bs.gof){
      cat(paste0("Test Statistic: ", round(x$obj.fun,2),"\n",
                 "P-Value: ", round(x$testinfo[1],4)),
          {if(x$bs.gof.p.ci) paste0(" CI: (", round(x$testinfo[2],4),", ", round(x$testinfo[3],4), ")")})

    }else{
      cat(paste0("Test Statistic: ", round(x$testinfo[1],2),
                 " on ",x$testinfo[3]," degrees of freedom\n",
                 "The resulting p-value is: ", round(x$testinfo[2],4)))
    }
    cat("\n\n")
  }

  if(x$bs.gof || x$bs.theta.est)
    cat(paste0("\nTo replicate the results, use seed: ",x$seed, "\n"))
}

#' Predict future points in the time series using the solution of the
#' Generalized Method of Wavelet Moments
#'
#' Creates a prediction using the estimated values of GMWM through the
#' ARIMA function within R.
#' @param object       A \code{gmwm} object
#' @param data.in.gmwm The data SAME EXACT DATA used in the GMWM estimation
#' @param n.ahead      Number of observations to forecast
#' @param ...          Additional parameters passed to ARIMA Predict
#' @return A \code{predict.gmwm} object with:
#' \describe{
#' \item{pred}{Predictions}
#' \item{se}{Standard Errors}
#' \item{resid}{Residuals from ARIMA ML Fit}
#' }
#' @export
predict.gmwm = function(object, data.in.gmwm, n.ahead = 1, ...){

  ts.mod = object$model

  if(length(ts.mod$desc) > 1 || ts.mod$desc != "SARIMA")
    stop("The predict function only works with stand-alone SARIMA models.")

  objdesc = ts.mod$obj.desc[[1]]

  # Unpack ts object
  p = objdesc[1]
  q = objdesc[2]
  P = objdesc[3]
  Q = objdesc[4]
  s = objdesc[6] # Set to 0 (handled in ARIMA)
  d = objdesc[7]
  D = objdesc[8]

  # Make an ARIMA object
  mod = arima(data.in.gmwm, order = c(p, d, q),
              list(order = c(P, D, Q), period = s),
              method = "ML",
              fixed = object$estimate[1:(p+q+P+Q)],
              transform.pars = F,
              include.mean = F)

  # Predict off of ARIMA
  pred = predict(mod, n.ahead = n.ahead, newxreg = NULL,
                 se.fit = TRUE, ...)

  # Format Results
  structure(list(pred = pred$pred,
                 se = pred$se,
                 resid = mod$residuals),
            class = "predict.gmwm")

}
