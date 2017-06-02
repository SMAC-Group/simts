/* Copyright (C) 2014 - 2017 James Balamuta, Stephane Guerrier, Roberto Molinari
 *
 * This file is part of simts R Methods Package
 *
 * The `simts` R package is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as
 * published by the Free Software Foundation, either version 3 of the
 * License, or (at your option) any later version.
 *
 * The `simts` R package is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 *  
 */

#include <RcppArmadillo.h>

#include "ts_checks.h"

// Include polyroot for invertibility
#include "polyroot.h"

// Complex tools
#include "complex_tools.h"

//' @title Obtain the smallest polynomial root
//' @description Calculates all the roots of a polynomial and returns the root that is the smallest.
//' @param x A \code{cx_vec} that has a 1 appended before the coefficents. (e.g. c(1, x))
//' @return A \code{double} with the minimum root value.
//' @keywords internal
// [[Rcpp::export]]
double minroot(const arma::cx_vec& x){
  return min(
    // sqrt(x^2 + y^2)
    Mod_cpp(
      // Return roots
      do_polyroot_arma(x)
    )
  );
}

int map_acc(int lhs, const std::pair<std::string, int> & rhs)
{
  return lhs + rhs.second;
}

int calc_map_sum(const std::map<std::string, int>& m){
  return std::accumulate(m.begin(), m.end(), 0, map_acc);  
}


//' @title Count Models
//' @description Count the amount of models that exist.
//' @param desc A \code{vector<string>} that contains the model's components.
//' @return A \code{map<string, int>} containing how frequent the model component appears.
//' @keywords internal
//' @export
// [[Rcpp::export]]
std::map<std::string, int> count_models(const std::vector<std::string>& desc){    
  std::map<std::string, int> w;	
  
  // We want to see the only the following objects with these initial values
  w["AR1"]    = 0;
  w["MA1"]    = 0;
  w["GM"]     = 0;
  w["ARMA"]   = 0;
  w["ARMA11"] = 0;
  w["DR"]     = 0;		
  w["RW"]     = 0;		
  w["QN"]     = 0;		
  w["WN"]     = 0;		
  
  for (unsigned int i = 0; i < desc.size(); i++) {		
    ++w[desc[i]];		
  }		
  
  return w;		
} 

