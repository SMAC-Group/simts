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
### Prediction Plot
######################
#' @title Plot Time Series Prediction Function
#' @description This function plots the time series predictions.
#' @param x A \code{gts} object
#' @param model A \code{ts} model
#' @param n.ahead An \code{integer} indicating number of predictions needed
#' @param time.pred A \code{vector} indicating the time of predictions
#' @author Yuming Zhang
#' @export
#' @examples
#' x = gts(as.vector(lynx), start = 1821, end = 1934, freq = 1, 
#' unit_ts = bquote(paste(10^8," ",m^3)), name_ts = "Numbers", 
#' unit_time = "year", data_name = "Annual Numbers of Lynx Trappings")
#' 
#' model = arima(x, c(2,0,0))
#' n.ahead = 20
#' time.pred = 1935:1954
#' plot_pred(x, model, n.ahead, time.pred)

plot_pred = function(x, model, n.ahead, time.pred,
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
  
  # Prediction 
  prediction = predict(model, n.ahead = n.ahead)
  pred = prediction$pred
  se = prediction$se
  
  # ----- Labels
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
    name_ts = "Observation and Prediction"
  }
  
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
  
  
  # ----- Plotting
  # X Scales
  scales = seq(start, end, length = n_x)
  if (is.null(end)){
    scales = scales/freq
    end = scales[n_x]
  }
  
  if (is.null(Time)){
    # Make frame
    make_frame(x_range = range(c(scales, time.pred)), 
               y_range = range(c(x,pred), na.rm = TRUE), 
               xlab = name_time, ylab = name_ts, main = main) 
    
    # Add lines 
    couleur = "blue4"
    lines(scales, x, type = "l", col = couleur, lty = 1)
    
    scales_pred = c(end, time.pred)
    lines(scales_pred, c(x[n_x], pred), type = "l", col = couleur, lty = 2)
    
    # Add CI
    polygon(x = c(scales_pred, rev(scales_pred)), 
            y = c(x[n_x], pred-2*se, rev(c(x[n_x], pred+2*se))), 
            col = rgb(0,0.1,1,0.1), border = NA)
    polygon(x = c(scales_pred, rev(scales_pred)), 
            y = c(x[n_x], pred-se, rev(c(x[n_x], pred+se))), 
            col = rgb(0,0.1,1,0.1), border = NA)
  }
  
  else {
    if(!is.numeric(Time)){
      # Make frame
      make_frame(x_range = range(c(Time, time.pred)), 
                 y_range = range(c(x,pred), na.rm = TRUE), 
                 add_axis_x = FALSE, 
                 xlab = name_time, ylab = name_ts, main = main)
      
      # Add x axis
      axis.Date(1, c(Time, time.pred))
    }
    else {
      # Make frame
      make_frame(x_range = range(c(Time, time.pred)), 
                 y_range = range(c(x,pred), na.rm = TRUE), 
                 xlab = name_time, ylab = name_ts, main = main) 
      
    }
    
    # Add lines
    couleur = "blue4"
    lines(Time, x, type = "l", col = couleur, lty = 1)
    
    scales_pred = c(Time[n_x], time.pred)
    lines(scales_pred, c(x[n_x], pred), type = "l", col = couleur, lty = 2)
    
    # Add CI
    polygon(x = c(scales_pred, rev(scales_pred)), 
            y = c(x[n_x], pred-2*se, rev(c(x[n_x], pred+2*se))), 
            col = rgb(0,0.1,1,0.1), border = NA)
    polygon(x = c(scales_pred, rev(scales_pred)), 
            y = c(x[n_x], pred-se, rev(c(x[n_x], pred+se))), 
            col = rgb(0,0.1,1,0.1), border = NA)
    
  }
}
