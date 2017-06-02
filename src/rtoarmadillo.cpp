
/* Copyright (C) 2014 - 2015  James Balamuta
 *
 * This file is part of simts R Methods Package
 *
 * The file uses methods in the r-to-armadillo project and is free software: you can redistribute it and/or modify it
 * under the terms of the MIT License.
 *
 * The r-to-armadillo project is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
 * 
 */


#include <RcppArmadillo.h>

#include "rtoarmadillo.h"
#include "armadillo_manipulations.h"

/* ----------------- R to Armadillo Functions ------------------ */

// A special define is included in rtoarmadillo.h used in these functions...



//' @title Lagged Differences in Armadillo
//' @description Returns the ith difference of a time series of rth lag.
//' @param x A \code{vec} that is the time series
//' @param lag A \code{unsigned int} that indicates the lag
//' @param differences A \code{dif} that indicates how many differences should be taken
//' @return A \code{vector} containing the differenced time series.
//' @author James Balamuta
//' @keywords internal
//' @export
//' @examples
//' x = rnorm(10, 0, 1)
//' diff_cpp(x,1,1)
// [[Rcpp::export]]
arma::vec diff_cpp(arma::vec x, unsigned int lag, unsigned int differences){
  
  // Difference the series i times
  for(unsigned int i=0; i < differences; i++){
    // Each difference will shorten series length
    unsigned int n=x.n_elem;
    // Take the difference based on number of lags
    x = (x.rows(lag,n-1) - x.rows(0,n-lag-1));
  }
  
  // Return differenced series:
  return x;
}

//' @title Time Series Convolution Filters
//' @description Applies a convolution filter to a univariate time series.
//' @param x A \code{column vector} of length T
//' @param filter A \code{column vector} of length f
//' @param sides An \code{int} that takes either 1:for using past values only or 2: filter coefficients are centered around lag 0.
//' @param circular A \code{bool} that indicates if the filter should be wrapped around the ends of the time series.
//' @return A \code{column vec} that contains the results of the filtering process.
//' @details This is a port of the cfilter function harnessed by the filter function in stats. 
//' It is about 5-7 times faster than R's base function. The benchmark was done on iMac Late 2013 using vecLib as the BLAS.
//' @author R Core Team and James Balamuta
//' @keywords internal
//' @export
//' @examples
//' x = 1:15
//' # 
//' cfilter(x, rep(1, 3), sides = 2, circular = FALSE)
//' # Using R's function
//' filter(x, rep(1, 3))
//' #
//' cfilter(x, rep(1, 3), sides = 1, circular = FALSE)
//' # Using R's function
//' filter(x, rep(1, 3), sides = 1)
//' #
//' cfilter(x, rep(1, 3), sides = 1, circular = TRUE)
//' # Using R's function
//' filter(x, rep(1, 3), sides = 1, circular = TRUE)
// [[Rcpp::export]]
arma::vec cfilter(arma::vec x, arma::vec filter, int sides, bool circular)
{
  
  int nx = x.n_elem;
  int nf = filter.n_elem;
  int nshift;
  
  if(sides == NA_INTEGER || circular == NA_LOGICAL)  Rcpp::stop("invalid input");
  
  double z, tmp;
  
  if(sides == 2){
    nshift = nf /2;
  }
  else{
    nshift = 0;
  }
    
  arma::vec out = arma::zeros<arma::vec>(nx);
  
  if(!circular) {
    for(int i = 0; i < nx; i++) {
      z = 0;
      if(i + nshift - (nf - 1) < 0 || i + nshift >= nx) {
        out(i) = NA_REAL;
        continue;
      }
      for(int j = std::max(0, nshift + i - nx); j < std::min(nf, i + nshift + 1) ; j++) {
        tmp = x(i + nshift - j);
        z += filter(j) * tmp;
      }
      out(i) = z;
    }
  } else { /* circular */
  for(int i = 0; i < nx; i++)
  {
    z = 0;
    for(int j = 0; j < nf; j++) {
      int ii = i + nshift - j;
      if(ii < 0) ii += nx;
      if(ii >= nx) ii -= nx;
      tmp = x(ii);
      z += filter(j) * tmp;
    }
    out(i) = z;
  }
  }
  return out;
}


//' @title Time Series Recursive Filters
//' @description Applies a recursive filter to a univariate time series.
//' @usage rfilter(x, filter, init)
//' @param x A \code{column vector} of length T
//' @param filter A \code{column vector} of length f
//' @param init A \code{column vector} of length f that contains the initial values of the time series in reverse.
//' @return x A \code{column vector} with its contents reversed.
//' @details Note: The length of 'init' must be equal to the length of 'filter'.
//' This is a port of the rfilter function harnessed by the filter function in stats. 
//' It is about 6-7 times faster than R's base function. The benchmark was done on iMac Late 2013 using vecLib as the BLAS.
//' @author R Core Team and James Balamuta
//' @keywords internal
//' @export
//' @examples
//' x = 1:15
//' # 
//' rfilter(x, rep(1, 3), rep(1, 3))
//' # Using R's function
//' filter(x, rep(1, 3), method="recursive", init=rep(1, 3))
// [[Rcpp::export]]
arma::vec rfilter(arma::vec x, arma::vec filter, arma::vec init)
{
 
  int nx = x.n_elem, nf = filter.n_elem;
    
  double sum;
  arma::vec r = arma::join_cols(reverse_vec(init), arma::zeros<arma::vec>(nx) ); 
  // see filter source
  // .Call(C_rfilter, x, filter, c(rev(init[, 1L]), double(n)))[-ind]
  // r is then c(rev(init[, 1L]), double(n))
  arma::vec rx = x;
  arma::vec rf = filter;
  
  for(int i = 0; i < nx; i++) {
    sum = rx(i);
    for (int j = 0; j < nf; j++) {
      if(nf + i - j - 1 >= 0){
        sum += r(nf + i - j - 1) * rf(j);
      }else{
        r[nf + i] = NA_REAL; goto bad3; 
      }
    }
    r(nf + i) = sum;
    bad3:
  continue;
  }

  return r.rows(nf,r.n_elem-1); // returns truncated vec (discards filter)
}


//' @title Mean of the First Difference of the Data
//' @description The mean of the first difference of the data
//' @param x A \code{vec} containing the data 
//' @return A \code{double} that contains the mean of the first difference of the data.
//' @keywords internal
//' @export
//' @examples
//' x=rnorm(10)
//' mean_diff(x)
// [[Rcpp::export]]
double mean_diff(const arma::vec& x){
  return arma::mean(diff_cpp(x, 1, 1));
}


//' @rdname diff_inv
// [[Rcpp::export]]
arma::vec intgr_vec(const arma::vec& x, const arma::vec& xi, unsigned int lag){
  
  unsigned int lagn = x.n_elem + lag;
  arma::vec y = arma::zeros<arma::vec>(lagn);
  y.rows(0,lag-1) = xi; 
  
  for (unsigned int i = lag; i < lagn; i++){
    y(i) = x(i - lag) + y(i - lag); 
  }
  
  return y;
}

//' @param xi A \code{vec} with length \eqn{lag*d} that provides initial values for the integration.
//' @rdname diff_inv
// [[Rcpp::export]]
arma::vec diff_inv_values(const arma::vec& x, unsigned int lag, unsigned int d, const arma::vec& xi){
  
  if(lag*d != xi.n_elem){
    Rcpp::stop("length of `xi` must be `lag*d`.");
  }
  
  if (d == 1) {
    return intgr_vec(x, xi, lag);
  }
  
  arma::vec dec_xi = diff_cpp(xi, lag, 1);
  
  arma::vec new_xi = xi.rows(0,lag-1);
  
  return diff_inv_values(diff_inv_values(x, lag, d - 1, dec_xi),
                         lag, 1, new_xi);
}

//' Discrete Intergral: Inverse Difference
//' 
//' Takes the inverse difference (e.g. goes from diff() result back to previous vector)
//' @param x   A \code{vec} containing the data
//' @param lag An \code{unsigned int} indicating the lag between observations. 
//' @param d   An \code{unsigned int} which gives the number of "differences" to invert.
//' @keywords internal
// [[Rcpp::export]]
arma::vec diff_inv(const arma::vec& x, unsigned int lag, unsigned int d){
  arma::vec xi =  arma::zeros<arma::vec>(lag*d);
  return diff_inv_values(x, lag, d, xi);
}



// helper function
inline void sweep_col_mean(arma::mat& x){
  
  for(unsigned int i = 0; i < x.n_cols; i++){
    arma::vec act = x.col(i);
    double mu = mean(act);
    x.col(i) = act - mu;
  }
  
}


/* ------------------ End R to Armadillo Functions ----------------------- */