#' Generalized Method of Wavelet Moments (GMWM)
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
#' \itemize{
#'  \item estimate: Estimated Parameters Values from the GMWM Procedure
#'  \item init.guess: Initial Starting Values given to the Optimization Algorithm
#'  \item wv.empir: The data's empirical wavelet variance
#'  \item ci_low: Lower Confidence Interval
#'  \item ci_high: Upper Confidence Interval
#'  \item orgV: Original V matrix
#'  \item V: Updated V matrix (if bootstrapped)
#'  \item omega: The V matrix inversed
#'  \item obj.fun: Value of the objective function at Estimated Parameter Values
#'  \item theo: Summed Theoretical Wavelet Variance
#'  \item decomp.theo: Decomposed Theoretical Wavelet Variance by Process
#'  \item scales: Scales of the GMWM Object
#'  \item robust: Indicates if parameter estimation was done under robust or classical
#'  \item eff: Level of efficiency of robust estimation
#'  \item model.type: Models being guessed
#'  \item compute.v: Type of V matrix computation
#'  \item augmented: Indicates moments have been augmented
#'  \item alpha: Alpha level used to generate confidence intervals
#'  \item expect.diff: Mean of the First Difference of the Signal
#'  \item N: Length of the Signal
#'  \item G: Number of Guesses Performed
#'  \item H: Number of Bootstrap replications
#'  \item K: Number of V matrix bootstraps
#'  \item model: \code{ts.model} supplied to gmwm
#'  \item model.hat: A new value of \code{ts.model} object supplied to gmwm
#'  \item starting: Indicates whether the procedure used the initial guessing approach
#'  \item seed: Randomization seed used to generate the guessing values
#'  \item freq: Frequency of data
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
#' \itemize{
#'   \item "AR1": a first order autoregressive process with parameters \eqn{(\phi,\sigma^2)}{phi, sigma^2}
#'   \item "GM": a guass-markov process \eqn{(\beta,\sigma_{gm}^2)}{beta, sigma[gm]^2}
#'   \item "ARMA": an autoregressive moving average process with parameters \eqn{(\phi _p, \theta _q, \sigma^2)}{phi[p], theta[q], sigma^2}
#'   \item "DR": a drift with parameter \eqn{\omega}{omega}
#'   \item "QN": a quantization noise process with parameter \eqn{Q}
#'   \item "RW": a random walk process with parameter \eqn{\sigma^2}{sigma^2}
#'   \item "WN": a white noise process with parameter \eqn{\sigma^2}{sigma^2}
#' }
#' If only an ARMA() term is supplied, then the function takes conditional least squares as starting values
#' If robust = TRUE the function takes the robust estimate of the wavelet variance to be used in the GMWM estimation procedure.
gmwm = function(model, data, model.type="imu", compute.v="auto",
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
                       ci_low = out[[4]],
                       ci_high = out[[5]],
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
#' \itemize{
#'  \item{estimate}{Estimated Parameters Values from the GMWM Procedure}
#'  \item{init.guess}{Initial Starting Values given to the Optimization Algorithm}
#'  \item{wv.empir}{The data's empirical wavelet variance}
#'  \item{ci_low}{Lower Confidence Interval}
#'  \item{ci_high}{Upper Confidence Interval}
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
              object$orgV, object$scales, cbind(object$wv.empir,object$ci_low,object$ci_high), # needed WV info
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
#' \itemize{
#'  \item{estimate}{Estimated Parameters Values from the GMWM Procedure}
#'  \item{init.guess}{Initial Starting Values given to the Optimization Algorithm}
#'  \item{wv.empir}{The data's empirical wavelet variance}
#'  \item{ci_low}{Lower Confidence Interval}
#'  \item{ci_high}{Upper Confidence Interval}
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
#' \itemize{
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
#' \itemize{
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

#' @title Plot the GMWM with the Wavelet Variance
#'
#' @description
#' Displays a plot of the Wavelet Variance (WV) with the CI values and the WV implied by the estimated parameters.
#' @method plot gmwm
#' @param x                A \code{gmwm} object.
#' @param decomp           A \code{boolean} that determines whether the contributions of each individual model are plotted.
#' @param units            A \code{string} that specifies the units of time plotted on the x axis.
#' @param xlab             A \code{string} that gives a title for the x axis.
#' @param ylab             A \code{string} that gives a title for the y axis.
#' @param main             A \code{string} that gives an overall title for the plot.
#' @param col_wv           A \code{string} that specifies the color of the wavelet variance line.
#' @param col_ci           A \code{string} that specifies the color of the shaded area covered by the confidence intervals.
#' @param nb_ticks_x       An \code{integer} that specifies the maximum number of ticks for the x-axis.
#' @param nb_ticks_y       An \code{integer} that specifies the maximum number of ticks for the y-axis.
#' @param legend_position  A \code{string} that specifies the position of the legend (use \code{legend_position = NA} to remove legend).
#' @param ci_wv            A \code{boolean} that determines whether to plot the confidence interval shaded area.
#' @param point_cex        A \code{double} that specifies the size of each symbol to be plotted.
#' @param point_pch        A \code{double} that specifies the symbol type to be plotted.
#' @param ...              Additional arguments affecting the plot.
#' @return Plot of WV and relative confidence intervals for each scale.
#' @author Stephane Guerrier and Yuming Zhang 
#' @export
plot.gmwm = function(x, decomp = FALSE, units = NULL, xlab = NULL, ylab = NULL, main = NULL, 
                     col_wv = NULL, col_ci = NULL, nb_ticks_x = NULL, nb_ticks_y = NULL,
                     legend_position = NULL, ci_wv = NULL, point_cex = NULL, 
                     point_pch = NULL, ...){
  
  # Labels
  if (is.null(xlab)){
    if (is.null(units)){
      xlab = expression(paste("Scale ", tau, sep =""))
    }else{
      xlab = bquote(paste("Scale ", tau, " [", .(units), "]", sep = " "))
    }
  }
  
  if (is.null(ylab)){
    ylab = expression(paste("Wavelet Variance ", nu^2, sep = ""))
  }else{
    ylab = ylab
  }
  
  # Main Title
  if (is.null(main)){
    main = "Haar Wavelet Variance Representation"
  }
  
  # Line and CI colors
  if (is.null(col_wv)){
    col_wv = "darkblue"
  }
  
  if (is.null(col_ci)){
    col_ci = hcl(h = 210, l = 65, c = 100, alpha = 0.15)
  }
  
  # Range
  x_range = range(x$scales)
  x_low = floor(log10(x_range[1]))
  x_high = ceiling(log10(x_range[2]))
  
  y_range = range(c(x$ci_low, x$ci_high))
  y_low = floor(log10(y_range[1]))
  y_high = ceiling(log10(y_range[2]))
  
  # Axes
  if (is.null(nb_ticks_x)){
    nb_ticks_x = 6
  }
  
  if (is.null(nb_ticks_y)){
    nb_ticks_y = 5
  }
  
  x_ticks = seq(x_low, x_high, by = 1)
  if (length(x_ticks) > nb_ticks_x){
    x_ticks = x_low + ceiling((x_high - x_low)/(nb_ticks_x + 1))*(0:nb_ticks_x)
  }
  x_labels = sapply(x_ticks, function(i) as.expression(bquote(10^ .(i))))
  
  y_ticks <- seq(y_low, y_high, by = 1)
  if (length(y_ticks) > nb_ticks_y){
    y_ticks = y_low + ceiling((y_high - y_low)/(nb_ticks_y + 1))*(0:nb_ticks_y)
  }
  y_labels <- sapply(y_ticks, function(i) as.expression(bquote(10^ .(i))))
  
  # Legend Position
  if (is.null(legend_position)){
    # if (which.min(abs(c(y_low, y_high) - log2(x$wv.empir[1]))) == 1){
    #   legend_position = "topleft"
    # }else{
    #   legend_position = "bottomleft"
    # }
    legend_position = "bottomleft"
  }   
  
  # Main Plot                     
  plot(NA, xlim = x_range, ylim = y_range, xlab = xlab, ylab = ylab, 
       log = "xy", xaxt = 'n', yaxt = 'n', bty = "n", ann = FALSE)
  win_dim = par("usr")
  
  par(new = TRUE)
  plot(NA, xlim = x_range, ylim = 10^c(win_dim[3], win_dim[4] + 0.22*(win_dim[4] - win_dim[3])),
       xlab = xlab, ylab = ylab, log = "xy", xaxt = 'n', yaxt = 'n', bty = "n")
  win_dim = par("usr")
  
  # Add Grid
  abline(v = 10^x_ticks, lty = 1, col = "grey95")
  abline(h = 10^y_ticks, lty = 1, col = "grey95")
  
  # Add Title
  x_vec = 10^c(win_dim[1], win_dim[2], win_dim[2], win_dim[1])
  y_vec = 10^c(win_dim[4], win_dim[4],
               win_dim[4] - 0.09*(win_dim[4] - win_dim[3]), 
               win_dim[4] - 0.09*(win_dim[4] - win_dim[3]))
  polygon(x_vec, y_vec, col = "grey95", border = NA)
  text(x = 10^mean(c(win_dim[1], win_dim[2])), y = 10^(win_dim[4] - 0.09/2*(win_dim[4] - win_dim[3])), main)
  
  # Add Axes and Box
  lines(x_vec[1:2], rep(10^(win_dim[4] - 0.09*(win_dim[4] - win_dim[3])),2), col = 1)
  #y_ticks = y_ticks[(2^y_ticks) < 10^(win_dim[4] - 0.09*(win_dim[4] - win_dim[3]))]
  y_labels = y_labels[1:length(y_ticks)]
  box()
  axis(1, at = 10^x_ticks, labels = x_labels, padj = 0.3)
  axis(2, at = 10^y_ticks, labels = y_labels, padj = -0.2)  
  
  # CI for WV
  if (ci_wv == TRUE || is.null(ci_wv)){
    polygon(c(x$scales, rev(x$scales)), c(x$ci_low, rev(x$ci_high)),
            border = NA, col = col_ci)
  }
  
  # Add legend
  CI_conf = 1 - x$alpha
  
  if (x$robust == TRUE){
    wv_title_part1 = "Empirical Robust WV "
  }else{
    wv_title_part1 = "Empirical WV "
  }
  
  # Add WV
  lines(x$scales, x$wv.empir, type = "l", col = col_wv, pch = 16)
  
  if (is.null(point_pch)){
    point_pch = 16
  }
  
  if (is.null(point_cex)){
    point_cex = 1.25
  }
  lines(x$scales, x$wv.empir, type = "p", col = col_wv, pch = point_pch, cex = point_cex)
  
  # Add model based WV
  if (decomp == TRUE){
    processes_theo = x$decomp.theo
    m = ncol(processes_theo)
    col_decomp = hcl(h = seq(100, 375, length = m + 1), l = 65, c = 200, alpha = 1)[1:m]
    for (i in 1:m){
      lines(x$scales, processes_theo[,i], col = col_decomp[i])
    }
  }
  col_fit = "#F47F24"
  lines(x$scales,x$theo, type = "b", lwd = 2, col = col_fit, pch = 1, cex = 1.5)
  
  
  if (!is.na(legend_position)){
    if (legend_position == "topleft"){
      legend_position = 10^c(1.1*win_dim[1], 0.98*(win_dim[4] - 0.09*(win_dim[4] - win_dim[3])))
      
      if (decomp == TRUE){
        legend(x = legend_position[1], y = legend_position[2],
               legend = c(as.expression(bquote(paste(.(wv_title_part1), hat(nu)^2))),
                          as.expression(bquote(paste("CI(",hat(nu)^2,", ",.(CI_conf),")"))),
                          x$model$desc, "Model-based WV"),
               pch = c(16, 15, rep(NA, m), 1), 
               lty = c(1, NA, rep(1, m), 1), 
               col = c(col_wv, col_ci, col_decomp, col_fit), 
               cex = 1, pt.cex = c(1.25, 3, rep(1, m), 1.35), bty = "n")
      }else{
        legend(x = legend_position[1], y = legend_position[2],
               legend = c(as.expression(bquote(paste(.(wv_title_part1), hat(nu)^2))),
                          as.expression(bquote(paste("CI(",hat(nu)^2,", ",.(CI_conf),")"))),
                          "Model-based WV"),
               pch = c(16, 15, 1), 
               lty = c(1, NA, 1), 
               col = c(col_wv, col_ci, col_fit), 
               cex = 1, pt.cex = c(1.25, 3, 1.35), bty = "n")
      }
      
    }else{
      if (legend_position == "topright"){
        legend_position = 10^c(0.7*win_dim[2], 0.98*(win_dim[4] - 0.09*(win_dim[4] - win_dim[3])))
        if (decomp == TRUE){
          legend(x = legend_position[1], y = legend_position[2],
                 legend = c(as.expression(bquote(paste(.(wv_title_part1), hat(nu)^2))),
                            as.expression(bquote(paste("CI(",hat(nu)^2,", ",.(CI_conf),")"))),
                            x$model$desc, "Model-based WV"),
                 pch = c(16, 15, rep(NA, m), 1), 
                 lty = c(1, NA, rep(1, m), 1), 
                 col = c(col_wv, col_ci, col_decomp, col_fit), 
                 cex = 1, pt.cex = c(1.25, 3, rep(1, m), 1.35), bty = "n")
        }else{
          legend(x = legend_position[1], y = legend_position[2],
                 legend = c(as.expression(bquote(paste(.(wv_title_part1), hat(nu)^2))),
                            as.expression(bquote(paste("CI(",hat(nu)^2,", ",.(CI_conf),")"))),
                            "Model-based WV"),
                 pch = c(16, 15, 1), 
                 lty = c(1, NA, 1), 
                 col = c(col_wv, col_ci, col_fit), cex = 1, pt.cex = c(1.25, 3, 1.35), bty = "n")
        }
      }else{
        if (decomp == TRUE){
          legend(x = legend_position[1], y = legend_position[2],
                 legend = c(as.expression(bquote(paste(.(wv_title_part1), hat(nu)^2))),
                            as.expression(bquote(paste("CI(",hat(nu)^2,", ",.(CI_conf),")"))),
                            x$model$desc, "Model-based WV"),
                 pch = c(16, 15, rep(NA, m), 1), 
                 lty = c(1, NA, rep(1, m), 1), 
                 col = c(col_wv, col_ci, col_decomp, col_fit), 
                 cex = 1, pt.cex = c(1.25, 3, rep(1, m), 1.35), bty = "n")
        }else{
          legend(x = legend_position[1], y = legend_position[2],
                 legend = c(as.expression(bquote(paste(.(wv_title_part1), hat(nu)^2))),
                            as.expression(bquote(paste("CI(",hat(nu)^2,", ",.(CI_conf),")"))),
                            "Model-based WV"),
                 pch = c(16, 15, 1), 
                 lty = c(1, NA, 1), 
                 col = c(col_wv, col_ci, col_fit), cex = 1, pt.cex = c(1.25, 3, 1.35), bty = "n")
        }
      }
    }
  }
}
