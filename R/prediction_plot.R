# Copyright (C) 2018 Stephane Guerrier, Yuming Zhang, Roberto Molinari
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


######################
### Forecast Plot
######################
#' @title Plot Time Series Forecast Function
#' @description This function plots the time series output from a forecast method with approximate 68% and 95% confidence intervals.
#' @param x A \code{gts} object
#' @param model A \code{ts} model
#' @param n.ahead An \code{integer} indicating number of units of time ahead for which to make forecasts
#' @param level A \code{double} indicating confidence level of prediction interval. 
#' @param xlab A \code{string} for the title of x axis
#' @param ylab A \code{string} for the title of y axis
#' @param main A \code{string} for the over all title of the plot
#' @param ...      Additional parameters
#' @author Yuming Zhang
#' @export
#' @examples
#' # Example where Time is null
#' x = gts(as.vector(lynx), start = 1821, end = 1934, freq = 1, 
#' unit_ts = bquote(paste(10^8," ",m^3)), name_ts = "Numbers", 
#' unit_time = "year", data_name = "Annual Numbers of Lynx Trappings")
#' 
#' model = arima(x, c(2,0,0))
#' n.ahead = 20
#' plot_pred(x, model, n.ahead)
#' plot_pred(x, model, n.ahead, level = 0.90)
#' 
#' # Example where Time is not numeric
#' Time = c("2018-08-30", "2018-08-31", "2018-09-01", "2018-09-02", "2018-09-03", 
#' "2018-09-04", "2018-09-05", "2018-09-06", "2018-09-07", "2018-09-08")
#' x = gen_ar1(10, 0.3, 1)
#' x = gts(x, freq = 1, Time = Time)
#' model= arima(x, c(1,0,0))
#' n.ahead = 4
#' plot_pred(x, model, n.ahead)


plot_pred = function(x, model, n.ahead, level = 0.95, 
                     xlab = NULL, ylab = NULL, main = NULL, ...){
  
  # Extract values
  unit_ts = attr(x, 'unit_ts')
  name_ts = attr(x, 'name_ts')
  unit_time = attr(x, 'unit_time')
  name_time = attr(x, 'name_time')
  start =  attr(x, 'start')
  end = attr(x, 'end')
  freq = attr(x, 'freq')
  title_x = attr(x, 'print')
  simulated = attr(x, 'simulated')
  Time = attr(x, 'Time')
  data_name = attr(x, 'data_name')
  n_x = length(x)
  
  # Warning 
  if (n_x == 0){stop('Time series is empty!')}
  if(!is(x,"gts")){stop('Object must be a gts object. Use functions gts() or gen_gts() to create it.')}
  # if (length(time.pred) != n.ahead){stop('Number of required forecasts (n.ahead) do not correspond to given time points (time.pred)')}
  
  # Prediction 
  prediction = predict(model, n.ahead = n.ahead)
  pred = prediction$pred
  se = prediction$se
  ci.up = pred+qnorm(level)*se
  ci.low = pred-qnorm(level)*se
  
  # Labels
  if (!is.null(xlab)){ name_time = xlab }
  
  if (!is.null(ylab)){ name_ts = ylab }
  
  if (is.null(name_time)){ name_time = "Time" }
  
  if (is.null(name_ts)){ name_ts = "Observation and Prediction" }
  
  if (!is.null(unit_time)){
    if (class(unit_time) == "name" || class(unit_time) == "call"){
      name_time = comb(name_time, " (", unit_time, ")")
    }else{
      name_time = paste(name_time, " (", unit_time, ")", sep = "")
    }
  }
  
  if (!is.null(unit_ts)){
    if (class(unit_ts) == "name" || class(unit_ts) == "call"){
      name_ts = comb(name_ts, " (", unit_ts, ")")
    }else{
      name_ts = paste(name_ts, " (", unit_ts, ")", sep = "")
    }
  }
  
  if (is.null(main)){
    if (!is.null(simulated)){
      main = title_x
    }else{
      if (is.null(data_name)){
        main = "Time series"
      }else{
        main = data_name
      }
    }
  }
  
  
  # Plotting
  # X Scales
  scales = seq(start, end, length = n_x)
  if (is.null(end)){
    scales = scales/freq
    end = scales[n_x]
  }
  
  
  if (is.null(Time)){
    # time pred
    time.pred = seq(end+1, end + floor(n.ahead / freq))
    scale.pred = c(end, time.pred)
    
    # Make frame
    make_frame(x_range = range(c(scales, time.pred)), 
               y_range = range(c(x,pred, ci.up, ci.low), 
                               na.rm = TRUE), 
               xlab = name_time, ylab = name_ts, main = main) 
    
    # Add lines 
    couleur = "blue4"
    lines(scales, x, type = "l", col = couleur, lty = 1)
    
    lines(scale.pred, c(x[n_x], pred), type = "l", col = couleur, lty = 2)
    
    # Add CI
    polygon(x = c(scale.pred, rev(scale.pred)), 
            y = c(x[n_x], ci.low, rev(c(x[n_x], ci.up))), 
            col = rgb(0,0.1,1,0.1), border = NA)
  }
  
  else {
    if(!is.numeric(Time)){
      # time.pred
      Time_int = as.integer(as.Date(Time, origin="1970-01-01")) # 1970-01-01 is default origin
      start = Time_int[1]
      end = Time_int[length(Time)]
      time.pred = end + seq(1, floor(n.ahead / freq))

      # Make frame
      make_frame(x_range = range(c(Time_int, time.pred)), 
                 y_range = range(c(x,pred, ci.low, ci.up), na.rm = TRUE), 
                 add_axis_x = FALSE, 
                 xlab = name_time, ylab = name_ts, main = main)
      
      # Add x axis
      axis.Date(1, as.Date(c(Time_int, time.pred), , origin="1970-01-01") )
      
      # Add lines
      couleur = "blue4"
      lines(Time_int, x, type = "l", col = couleur, lty = 1)
      
      scales.pred = c(end, time.pred)
      lines(scales.pred, c(x[n_x], pred), type = "l", col = couleur, lty = 2)
      
      # Add CI
      polygon(x = c(scales.pred, rev(scales.pred)), 
              y = c(x[n_x], ci.low, rev(c(x[n_x], ci.up))), 
              col = rgb(0,0.1,1,0.1), border = NA)
    }
    else {
      # time.pred
      time.pred = end + seq(1, floor(n.ahead / freq))
      
      # Make frame
      make_frame(x_range = range(c(Time, time.pred)), 
                 y_range = range(c(x,pred, ci.low, ci.up), na.rm = TRUE), 
                 xlab = name_time, ylab = name_ts, main = main) 
      
      # Add lines
      couleur = "blue4"
      lines(Time, x, type = "l", col = couleur, lty = 1)
      
      scales.pred = c(end, time.pred)
      lines(scales.pred, c(x[n_x], pred), type = "l", col = couleur, lty = 2)
      
      # Add CI
      polygon(x = c(scales.pred, rev(scales.pred)), 
              y = c(x[n_x], ci.low, rev(c(x[n_x], ci.up))), 
              col = rgb(0,0.1,1,0.1), border = NA)
      
    }
    
  }
}
