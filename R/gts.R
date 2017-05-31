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

#' @title Create a simts TS Object based on data
#' @description Setups a time series oriented object that works well with graphing and summary utilities
#' @param data      A one-column \code{matrix}, \code{data.frame}, or a numeric \code{vector}.
#' @param start     A \code{numeric} that provides the time of the first observation.
#' @param end       A \code{numeric} that provides the time of the last observation.
#' @param freq      A \code{numeric} that provides the rate of samples. Default value is 1.
#' @param unit_ts   A \code{string} that contains the unit expression of the time series. Default value is \code{NULL}.
#' @param unit_time A \code{string} that contains the unit expression of the time. Default value is \code{NULL}.
#' @param name_ts   A \code{string} that provides an identifier for the time series data. Default value is \code{NULL}.
#' @param name_time A \code{string} that provides an identifier for the time. Default value is \code{NULL}.
#' @return A \code{gts} object with the following attributes:
#' @author James Balamuta and Wenchao Yang
#' @examples
#' m = data.frame(rnorm(50))
#' x = gts(m, unit_time = 'sec', name_ts = 'example')
#' plot(x)
#' 
#' x = gen_gts(50, WN(sigma2 = 1))
#' x = gts(x, freq = 100, unit_time = 'sec')
#' plot(x)
gts = function(data, start = 0, end = NULL, freq = 1, unit_ts = NULL, unit_time = NULL, name_ts = NULL, name_time = NULL){
  
  # 1. requirement for 'data'
  # Force data.frame to matrix  
  if (is.data.frame(data)){ 
    data = data.matrix(data)
  }
  
  # Check if the data is in matrix form
  if (is.matrix(data)) {
    # Check ncol
    ncolumn = ncol(data)
    if(ncolumn != 1){
      stop("'data' must have one column.")
    }
    
  } else {
    data = data.matrix(data) # convert vector to matrix
  }
  
  ndata = nrow(data)
  colnames(data) = if(is.null(name_ts)) 'Observed' else name_ts
  
  if(ndata == 0) {
    stop("Not a valid data object! Please supply a data set with one column that is in either a data.frame, matrix, or numeric object.")
  }
  
  # 2. requirement for 'freq'
  if(!is(freq,"numeric") || length(freq) != 1){ stop("'freq' must be one numeric number.") }
  if(freq <= 0) { stop("'freq' must be larger than 0.") }
  
  # 3. requirements for 'start' and 'end'
  if( is.numeric(start)==F && is.numeric(end)==F){
    stop("'start' or 'end' must be specified.")}
  
  if(is.null(start)==F && is.null(end)==F && (end-start)!= ((ndata-1)/freq) ){
    stop("end-start == (ndata-1)/freq must be TRUE.")
  }
  
  if ( is.null(end) ){
    end = start + (ndata - 1)/freq} # freq conversion (unit conversion is handled in graphical function)
  else if ( is.null(start) ){
    start = end - (ndata - 1)/freq}
  
  # 4. requirement for 'unit_time'
  if(!is.null(unit_time)){
    if(!unit_time %in% c('ns', 'ms', 'sec', 'second', 'min', 'minute', 'hour', 'day', 'mon', 'month', 'year')){
      stop('The supported units are "ns", "ms", "sec", "min", "hour", "day", "month", "year". ')
    }
  }
  
  # x = 0:(ndata-1)
  # x = seq(from = 0, to = (ndata-1), length.out = ndata)
  # x = x/freq ###when generate the object, not deal with freq
  
  out = structure(data, 
                  start = start, 
                  end= end, # start and end will not be null now
                  freq = freq,
                  unit_ts = unit_ts, 
                  unit_time = unit_time,
                  name_ts = name_ts,
                  name_time = name_time,
                  class = c("gts","matrix"))
  
  out
}


#' @title Create a simts TS Object based on model
#' @description Create a \code{gts} object based on a supplied time series model.
#' @param n          An \code{interger} containing the amount of observations for the time series.
#' @param model      A \code{ts.model} or \code{simts} object containing one of the allowed models.
#' @param start      A \code{numeric} that provides the time of the first observation.
#' @param end        A \code{numeric} that provides the time of the last observation.
#' @param freq       A \code{numeric} that provides the rate of samples. Default value is 1.
#' @param unit_ts   A \code{string} that contains the unit expression of the time series. Default value is \code{NULL}.
#' @param unit_time A \code{string} that contains the unit expression of the time. Default value is \code{NULL}.
#' @param name_ts   A \code{string} that provides an identifier for the time series data. Default value is \code{NULL}.
#' @param name_time A \code{string} that provides an identifier for the time. Default value is \code{NULL}.
#' @return A \code{gts} object with the following attributes:
#' @author James Balamuta and Wenchao Yang
#' @details
#' This function accepts either a \code{ts.model} object (e.g. AR1(phi = .3, sigma2 =1) + WN(sigma2 = 1)) or a \code{simts} object.
#' @examples
#' # Set seed for reproducibility
#' set.seed(1336)
#' n = 1000
#' 
#' # AR1 + WN
#' model = AR1(phi = .5, sigma2 = .1) + WN(sigma2=1)
#' x = gen_gts(n, model)
#' plot(x)
#' 
#' # Reset seed
#' set.seed(1336)
#' 
#' # GM + WN
#' # Convert from AR1 to GM values
#' m = ar1_to_gm(c(.5,.1),10)
#' 
#' # Beta = 6.9314718, Sigma2_gm = 0.1333333
#' model = GM(beta = m[1], sigma2_gm = m[2]) + WN(sigma2=1)
#' x2 = gen_gts(n, model, freq = 10, unit_time = 'sec')
#' plot(x2)
#' 
#' # Same time series
#' all.equal(x, x2, check.attributes = FALSE)
gen_gts = function(n, model, start = 0, end = NULL, freq = 1, unit_ts = NULL, unit_time = NULL, name_ts = NULL, name_time = NULL){
  
  # 1. Do we have a valid model?
  if(!(is.ts.model(model))){
    stop("model must be created from a ts.model or simts object using a supported component (e.g. AR1(), ARMA(p,q), DR(), RW(), QN(), and WN(). ")
  }
  
  # 2. requirement for 'freq'
  if(!is(freq,"numeric") || length(freq) != 1){ stop("'freq' must be one numeric number.") }
  if(freq <= 0) { stop("'freq' must be larger than 0.") }
  
  # 3. requirements for 'start' and 'end'
  if( is.numeric(start)==F && is.numeric(end)==F){
    stop("'start' or 'end' must be specified.")}
  
  if(is.null(start)==F && is.null(end)==F && (end-start)!= ((n-1)/freq) ){
    stop("end-start == (N-1)/freq must be TRUE.")
  }
  
  if ( is.null(end) ){
    end = start + (n - 1)/freq} # freq conversion (unit conversion is handled in graphical function)
  else if ( is.null(start) ){
    start = end - (n - 1)/freq}
  
  # 4. 'unit_time'
  if(!is.null(unit_time)){
    if(!unit_time %in% c('ns', 'ms', 'sec', 'second', 'min', 'minute', 'hour', 'day', 'mon', 'month', 'year')){
      stop('The supported units are "ns", "ms", "sec", "min", "hour", "day", "month", "year". ')
    }
  }
  
  # Information Required by simts:
  desc = model$desc
  obj = model$obj.desc
  
  # Identifiability issues
  if(any( count_models(desc)[c("DR","QN","RW","WN")] >1)){
    stop("Two instances of either: DR, QN, RW, or WN have been detected. As a result, the model will have identifiability issues. Please submit a new model.")
  }
  
  if(!model$starting){
    
    theta = model$theta
    
    # Convert from AR1 to GM
    if(any(model$desc == "GM")){
      theta = conv.gm.to.ar1(theta, model$process.desc, freq)
    }
    
    out = gen_model(n, theta, desc, obj)
  }else{
    stop("Need to supply initial values within the ts.model object.")
  }
  
  colnames(out) = if(is.null(name_ts)) 'Observed' else name_ts 
  
  # reupdate desc for plotting
  desc = paste0(model$desc, "()", collapse = " + ")
  
  out = structure(.Data = out, 
                  start = start, 
                  end   = end, # start and end will not be null now
                  desc = desc, 
                  freq  = freq,
                  unit_ts = unit_ts, 
                  unit_time  = unit_time,
                  name_ts  = name_ts,
                  name_time = name_time, 
                  class = c("gts","matrix"))
  
  out
  
}

#' @title Convert Unit of Time Series Data
#' @description Manipulate the units of time to different ones
#' @keywords internal
#' @param x          A \code{vector} containing the values on x-axis.
#' @param from.unit  A \code{string} indicating the unit which the data is converted from.
#' @param to.unit    A \code{string} indicating the unit which the data is converted to.
#' @details
#' The supported units are "ns"(nanosecond), "ms"(millisecond), "sec", "min", "hour", "day", "month", and "year".
#' Make sure \code{from.unit} and \code{to.unit} are not \code{NULL} before it is passed to this function.
#' @return A \code{list} with the following structure:
#' \describe{
#'  \item{x}{Data}
#'  \item{converted}{A \code{boolean} indicating whether conversion is made}
#' }
#' @examples
#' x = seq(60, 3600, 60)
#' unitConversion(x, 'sec', 'min')
#' y = 1:10
#' unitConversion(y, 'hour', 'sec')
unitConversion = function(x, from.unit, to.unit){
  
  #ns, ms, second, min, hour, day, month, year
  unit = c(ns = 1, ms = 2,se = 3, mi = 4, ho = 5, da = 6, mo = 7, ye = 8)
  
  #assume 1 month = 30 days
  ratio = c(1E6, 1E3, 60, 60, 24, 30, 12)
  from.unit.1 = substr(from.unit, 1, 2)
  to.unit.1 = substr(to.unit, 1, 2)
  
  #check unit:
  no.convert = F
  if(from.unit.1 == to.unit.1){no.convert = T}
  if(is.na(unit[from.unit.1]) ) {
    message = paste('No such unit: ', from.unit, '. Supported units are "ns"(nanosecond), "ms"(millisecond), "sec", "min", "hour", "day", "month", and "year". Conversion is terminated.', sep = '')
    warning(message); no.convert = T}
  if(is.na(unit[to.unit.1]) ) {
    message = paste('No such unit: ', to.unit, '. Supported units are "ns"(nanosecond), "ms"(millisecond), "sec", "min", "hour", "day", "month", and "year". Conversion is terminated.', sep = '')
    warning(message); no.convert = T}
  
  if(!no.convert){
    #print out warning when day is convert to month, or month is converted to day.
    conversionRange = unit[from.unit.1] : unit[to.unit.1]
    if(6 %in% conversionRange && 7 %in% conversionRange){
      warning('Unit conversion might be wrong because this function simply assumes 1 month = 30 days.')
    }
  }
  
  if(!no.convert){
    if(unit[from.unit.1] > unit[to.unit.1]){
      temp = ratio[unit[to.unit.1]: (unit[from.unit.1]-1)]
      multiplier = prod(temp)
      x = x*multiplier
    }else{
      temp = ratio[unit[from.unit.1]: (unit[to.unit.1]-1) ]
      multiplier = prod(temp)
      x = x/multiplier
    }
  }
  obj = list(x = x, converted = !no.convert)  
  return(obj)
}


#' @title Plot Time Series Data
#' @description Plot Time Series Data generated by gts or gen_gts. 
#' @method plot gts
#' @export
#' @keywords internal
#' @param x               A \code{gts} object
#' @param xlab            A \code{string} that gives a title for the x axis.
#' @param ylab            A \code{string} that gives a title for the y axis.
#' @param main            A \code{string} that gives an overall title for the plot.
#' @param col             A \code{string} that gives a color for the line. 
#' @param ... other arguments passed to specific methods
#' @return A plot containing the graph of the gts time series.
#' @author Justin Lee
plot.gts = function(x, xlab = NULL, ylab = NULL, main = NULL, col = "blue4"){
  unit_ts = attr(x, 'unit_ts')
  name_ts = attr(x, 'name_ts')
  unit_time = attr(x, 'unit_time')
  name_time = attr(x, 'name_time')
  start =  attr(x, 'start')
  end = attr(x, 'end')
  freq = attr(x, 'freq')
  title_x = attr(x, 'desc')
  n_x = length(x)
  
  if (n_x == 0){stop('Time series is empty!')}
  
  if(!is(x,"gts")){stop('object must be a gts object. Use function gts() or gen_gts() to create it.')}
  
  # Labels
  if (!is.null(xlab)){
    name_time = xlab
  }
  
  if (!is.null(ylab)){
    name_ts = ylab
  }
  
  if (is.null(name_time)){
    name_time = "Time"
  }
  
  if (is.null(name_ts)){
    name_ts = "Observation"
  }
  
  if (!is.null(unit_time)){
    xlab = paste(name_time, " (", unit_time, ")", sep = "")
  }
  
  if (!is.null(unit_ts)){
    ylab = paste(name_ts, " (", unit_ts, ")", sep = "")
  }

  
  if (is.null(main)){
    main = title_x
  }
  
  # X Scales
  scales = seq(start, end, length = n_x)
  if (is.null(end)){
    scales = scales/freq
    end = scales[n_x]
  }
  
  # Main plot                     
  plot(NA, xlim = c(start, end), ylim = range(x), xlab = xlab, ylab = ylab, 
       xaxt = 'n', yaxt = 'n', bty = "n", ann = FALSE)
  win_dim = par("usr")
  
  par(new = TRUE)
  plot(NA, xlim = c(start, end), ylim = c(win_dim[3], win_dim[4] + 0.09*(win_dim[4] - win_dim[3])),
       xlab = xlab, ylab = ylab, xaxt = 'n', yaxt = 'n', bty = "n")
  win_dim = par("usr")
  
  # Add grid
  grid(NULL, NULL, lty = 1, col = "grey95")
  
  # Add title
  x_vec = c(win_dim[1], win_dim[2], win_dim[2], win_dim[1])
  y_vec = c(win_dim[4], win_dim[4],
               win_dim[4] - 0.09*(win_dim[4] - win_dim[3]), 
               win_dim[4] - 0.09*(win_dim[4] - win_dim[3]))
  polygon(x_vec, y_vec, col = "grey95", border = NA)
  text(x = mean(c(win_dim[1], win_dim[2])), y = (win_dim[4] - 0.09/2*(win_dim[4] - win_dim[3])), main)
  
  # Add axes and box
  lines(x_vec[1:2], rep((win_dim[4] - 0.09*(win_dim[4] - win_dim[3])),2), col = 1)
  box()
  axis(1, padj = 0.3)
  y_axis = axis(2, labels = FALSE, tick = FALSE)  
  y_axis = y_axis[y_axis < (win_dim[4] - 0.09*(win_dim[4] - win_dim[3]))]
  axis(2, padj = -0.2, at = y_axis)  
  
  # Add lines 
  lines(scales, x, type = "l", col = col, pch = 16)
}