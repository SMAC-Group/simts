#' @title Generate AR(1) Block Process
#' @description 
#' This function allows us to generate a non-stationary AR(1) block process.
#' @export
#' @usage gen_ar1blocks(phi, sigma2, n_total, n_block, scale = 10, 
#' title = NULL, seed = 135, ...)
#' @param phi     A \code{double} value for the autocorrection parameter \eqn{\phi}{phi}.
#' @param sigma2  A \code{double} value for the variance parameter \eqn{\sigma ^2}{sigma^2}.
#' @param n_total An \code{integer} indicating the length of the simulated AR(1) block process.
#' @param n_block An \code{integer} indicating the length of each block of the AR(1) block process.
#' @param scale   An \code{integer} indicating the number of levels of decomposition. The default value is 10.
#' @param title   A \code{string} indicating the name of the time series data. 
#' @param seed    An \code{integer} defined for simulation replication purposes.
#' @param ...     Additional parameters. 
#' @return A \code{vector} containing the AR(1) block process.
#' @note This function generates a non-stationary AR(1) block process whose 
#' theoretical maximum overlapping allan variance (MOAV) is different 
#' from the theoretical MOAV of a stationary AR(1) process. This difference in the value of the allan variance 
#' between stationary and non-stationary processes has been shown through the 
#' calculation of the theoretical allan variance given in  "A Study of the Allan Variance for 
#' Constant-Mean Non-Stationary Processes" by Xu et al. (IEEE Signal Processing Letters, 2017), 
#' preprint available: \url{https://arxiv.org/abs/1702.07795}.
#' @author Yuming Zhang and Haotian Xu
#' @examples
#' Xt = gen_ar1blocks(phi = 0.9, sigma2 = 1, 
#' n_total = 1000, n_block = 10, scale = 100)
#' plot(Xt)
#' 
#' Yt = gen_ar1blocks(phi = 0.5, sigma2 = 5, n_total = 800, 
#' n_block = 20, scale = 50)
#' plot(Yt)
gen_ar1blocks = function(phi, sigma2, n_total, n_block, 
                         scale = 10, title = NULL, seed = 135, ...){
  
  set.seed(seed)
  ar = NULL
  
  for (i in (1:(n_total / n_block))) {
    xt = gen_ar1(N = n_block * scale, phi = phi, sigma2 = sigma2)
    x0 = xt[(n_block*(scale-1))]
    xt = xt[(n_block*(scale-1)+1): (n_block*scale)] 
    ar = c(ar, xt)
  }
  
  if (is.null(title)){
    title = "Simulated AR(1) Blocks Process"
  }
  
  ar = gts(ar, data_name = title)
  return(ar)
}



#' @title Generate Non-Stationary White Noise Process
#' @description 
#' This function allows to generate a non-stationary white noise process.
#' @export
#' @usage gen_nswn(n_total, title = NULL, seed = 135, ...)
#' @param n_total An \code{integer} indicating the length of the simulated non-stationary white noise process.
#' @param title   A \code{string} defining the name of the time series data. 
#' @param seed    An \code{integer} defined for simulation replication purposes.
#' @param ...     Additional parameters. 
#' @return A \code{vector} containing the non-stationary white noise process.
#' @note This function generates a non-stationary white noise process whose theoretical maximum overlapping allan variance (MOAV) corresponds to the 
#' theoretical MOAV of the stationary white noise process. This example confirms that the allan 
#' variance is unable to distinguish between a stationary white noise process and a white noise 
#' process whose second-order behavior is non-stationary, as pointed out in the paper "A Study of 
#' the Allan Variance for Constant-Mean Non-Stationary Processes" by Xu et al. (IEEE Signal Processing 
#' Letters, 2017), preprint available: \url{https://arxiv.org/abs/1702.07795}.
#' @author Yuming Zhang
#' @examples
#' Xt = gen_nswn(n_total = 1000)
#' plot(Xt)
#' 
#' Yt = gen_nswn(n_total = 2000, title = "non-stationary 
#' white noise process", seed = 1960)
#' plot(Yt)
gen_nswn = function(n_total, title = NULL, seed = 135, ...){
  set.seed(seed)
  wn = NULL    
  
  for (i in (1:n_total)){
    y = rnorm(n = 1, mean = 0, sd = sqrt(i))
    wn = c(wn, y)
  }
  
  if (is.null(title)){
    title = "Simulated Non-Stationary White Noise Process"
  }
  
  wn = gts(wn, data_name = title)
  return(wn)
}


#' @title Generate Bias-Instability Process
#' @description 
#' This function allows to generate a non-stationary bias-instability process.
#' @export
#' @usage gen_bi(sigma2, n_total, n_block, title = NULL, seed = 135, ...)
#' @param sigma2  A \code{double} value for the variance parameter \eqn{\sigma ^2}{sigma^2}.
#' @param n_total An \code{integer} indicating the length of the simulated bias-instability process.
#' @param n_block An \code{integer} indicating the length of each block of the bias-instability process.
#' @param title   A \code{string} defining the name of the time series data.
#' @param seed    An \code{integer} defined for simulation replication purposes.
#' @param ...     Additional parameters. 
#' @return A \code{vector} containing the bias-instability process.
#' @note This function generates a non-stationary bias-instability process
#' whose theoretical maximum overlapping allan variance (MOAV) is close to the theoretical
#' MOAV of the best approximation of this process through a stationary AR(1) process over some scales. However, this approximation 
#' is not good enough when considering the logarithmic representation of the allan variance.
#' Therefore, the exact form of the allan variance of this non-stationary process allows us 
#' to better interpret the signals characterized by bias-instability, as shown in "A Study 
#' of the Allan Variance for Constant-Mean Non-Stationary Processes" by Xu et al. (IEEE Signal 
#' Processing Letters, 2017), preprint available: \url{https://arxiv.org/abs/1702.07795}.
#' @author Yuming Zhang
#' @examples
#' Xt = gen_bi(sigma2 = 1, n_total = 1000, n_block = 10)
#' plot(Xt)
#' 
#' Yt = gen_bi(sigma2 = 0.8, n_total = 800, n_block = 20,
#' title = "non-stationary bias-instability process")
#' plot(Yt)
gen_bi = function(sigma2, n_total, n_block, title = NULL, seed = 135, ...){
  set.seed(seed)
  bi = NULL 
  
  for (i in (1:(n_total / n_block))){
    x = rnorm(n = 1, mean = 0, sd = sqrt(sigma2))
    bi = c(bi, rep(x, n_block))
  }
  
  if (is.null(title)){
    title = "Simulated Bias-Instability Process"
  }
  
  bi = gts(bi, data_name = title)
  return(bi)
}