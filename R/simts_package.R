# Copyright (C) 2018 James Balamuta, Stephane Guerrier, Roberto Molinari, Justin Lee, Yuming Zhang
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

#' Simulation and Plotting Tools Time Series (simts) Package
#' @keywords internal
#' @details 
#' \tabular{ll}{
#' Package: \tab simts\cr
#' Type: \tab Package\cr
#' Version: \tab 1.0\cr
#' Date: \tab 2018-09-15\cr
#' License: \tab GPL\cr
#' }
#' 
#' @author
#' James Balamuta \email{balamut2@@illinois.edu},
#' Stephane Guerrier \email{stephane@@psu.edu},
#' Roberto Molinari \email{roberto.molinari@@hotmail.it},
#' Wenchao Yang \email{wyang40@@illinois.edu}
#' Justin Lee \email{munsheet93@@gmail.com}
#' Yuming Zhang \email{yfz5097@@psu.edu}
#'
#' @name simts-package
#' @docType package
#' @useDynLib simts
#' @importFrom graphics plot par grid polygon text lines box axis mtext abline rect segments
#' @importFrom grDevices hcl rgb 
#' @importFrom methods is
#' @importFrom utils install.packages tail head
#' @importFrom Rcpp evalCpp
#' @importFrom stats arima predict ts as.ts is.ts qnorm rnorm
#' @importFrom scales trans_breaks trans_format math_format
NULL