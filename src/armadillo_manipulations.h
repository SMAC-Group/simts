/* Copyright (C) 2014 - 2018  James Balamuta, Stephane Guerrier, Roberto Molinari
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

#ifndef ARMADILLO_MANIPULATIONS
#define ARMADILLO_MANIPULATIONS

arma::mat sort_mat(arma::mat x, unsigned int col);
  
arma::mat rev_col_subset(arma::mat x, unsigned int start, unsigned int end);

arma::mat rev_row_subset(arma::mat x, unsigned int start, unsigned int end);

arma::vec reverse_vec(arma::vec x);

arma::mat field_to_matrix(arma::field<arma::vec> x);

double sum_field_vec(const arma::field<arma::vec>& x);

#endif
