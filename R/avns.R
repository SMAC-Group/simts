# Copyright (C) 2017 James Balamuta, Stephane Guerrier, Roberto Molinari, Justin Lee
#
# This file is part of simts R Methods Package
#
# The `simts` R package is free software: you can redistribute it and/or modify
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

#' @title Generate AR(1) Blocks Process
#' @description 
#' This function allows us to generate a non-stationary AR(1) blocks process.
#' @export
#' @usage gen_ar1blocks(phi, sigma2, n_total, n_block, scale = 10)
#' @param phi A \code{double} value for the autocorrection parameter \eqn{\phi}{phi}.
#' @param sigma2 A \code{double} value for the variance parameter \eqn{\sigma ^2}{sigma^2}.
#' @param n_total The length of the whole AR(1) blocks process.
#' @param n_block The length of each block of the AR(1) blocks process.
#' @param scale A \code{integer} indicating the amount of decomposition performed at each level. 
#' The default value is 10.
#' @return A \code{vector} containing the AR(1) blocks process.
#' @note This function helps generate a non-stationary process example, AR(1) blocks, whose 
#' theoretical maximum overlapping allan variance (MOAV) is different and can be distinguished 
#' from the theoretical MOAV of a stationary AR(1) process. This difference of allan variance 
#' between stationary and non-stationary processes is proved to be able to be captured by the 
#' calculation of theoretical allan variance raised in  "A Study of the Allan Variance for 
#' Constant-Mean Non-Stationary Processes" by Xu et al., arXiv preprint arXiv:1702.07795 (2017).
#' @author Yuming Zhang, Haotian Xu
#' @examples
#' Xt = gen_ar1blocks(phi = 0.9, sigma2 = 1, 
#' n_total = 1000, n_block = 10, scale = 100)
#' Yt = gen_ar1blocks(phi = 0.5, sigma2 = 5, n_total = 800, 
#' n_block = 20, scale = 50)
#' plot(Yt)
gen_ar1blocks = function(phi, sigma2, n_total, n_block, 
                         scale = 10, title = NULL, seed = 1995, ...){
  
  set.seed(seed)
  ar = NULL
  
  for (i in (1:(n_total / n_block))) {
    xt = gen_ar1(N = n_block * scale, phi = phi, sigma2 = sigma2)
    x0 = xt[(n_block*(scale-1))]
    xt = xt[(n_block*(scale-1)+1): (n_block*scale)] 
    ar = c(ar, xt)
  }
  
  if (is.null(title)){
    title = "Simulated block AR(1) process"
  }
  
  ar = gts(ar, data_name = title)
  return(ar)
}
