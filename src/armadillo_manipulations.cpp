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
#include "armadillo_manipulations.h"

//' @title Reverse Armadillo Vector
//' @description Reverses the order of an Armadillo Vector
//' @usage reverse_vec(x)
//' @param x A \code{column vector} of length N
//' @return x A \code{column vector} with its contents reversed.
//' @details Consider a vector x=[1,2,3,4,5], the function would output x=[5,4,3,2,1].
//' @author James Balamuta
//' @examples
//' x = 1:5
//' reverse_vec(x)
//' @keywords internal
// [[Rcpp::export]]
arma::vec reverse_vec(arma::vec x) {
   std::reverse(x.begin(), x.end());
   return x;
}
