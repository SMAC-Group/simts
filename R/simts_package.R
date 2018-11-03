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