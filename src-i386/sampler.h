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

#ifndef SAMPLE_H
#define SAMPLE_H

arma::vec rsample(const arma::vec &x, const int size, const bool replace, arma::vec prob_ );
void RSampleNoReplace(arma::vec &index, int nOrig, int size);
void RSampleReplace(arma::vec &index, int nOrig, int size);
void RProbSampleNoReplace(arma::vec &index, int nOrig, int size, arma::vec &prob);
void RProbSampleReplace(arma::vec &index, int nOrig, int size, arma::vec &prob);
void RWalkerProbSampleReplace(arma::vec &index, int nOrig, int size, arma::vec &prob);
void RFixProb(arma::vec &prob, const int size, const bool replace);

#endif