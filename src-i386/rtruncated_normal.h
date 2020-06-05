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

#ifndef RTRUNCATED_NORMAL_H
#define RTRUNCATED_NORMAL_H

// Multiple obs
arma::vec rtruncated_normal(unsigned int n, double mu, double sigma, double a, double b);

// One obs
double rtruncated_normal(double mu, double sigma, double a, double b);

// RNG Sampler
double sim_truncated_normal(double phi_a_cdf, double phi_b_cdf, double mu, double sigma);

#endif