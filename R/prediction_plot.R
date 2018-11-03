######################
### Forecast Plot
######################
#' @title Plot Time Series Forecast Function
#' @description This function plots the time series output from a forecast method with approximate 68% and 95% confidence intervals.
#' @param x A \code{gts} object
#' @param model A \code{ts} model
#' @param n.ahead An \code{integer} indicating number of units of time ahead for which to make forecasts
#' @param level A \code{double} or \code{vector} indicating confidence level of prediction interval.
#' By default, it uses the levels of 0.50 and 0.95.
#' @param xlab A \code{string} for the title of x axis
#' @param ylab A \code{string} for the title of y axis
#' @param main A \code{string} for the over all title of the plot
#' @param ...      Additional parameters
#' @author Yuming Zhang

plot_pred = function(x, model, n.ahead, level = NULL, 
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
  
  if(is.null(level)){
    level = c(0.50, 0.95)
  }
  n.level = length(level)
  out = list()   # stores all CI of all levels
  for (i in 1:n.level){
    ci.up = pred+qnorm(1- (1-level[i])/2)*se
    ci.low = pred-qnorm(1- (1-level[i])/2)*se
    CI = matrix(c(ci.low, ci.up), nrow = length(ci.low), ncol = 2)
    out[[i]] = CI
  }
  
  
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
    time.pred = end + (1:n.ahead)/freq
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
    for(i in 1:n.level){
      ci.low = out[[i]][,1]
      ci.up = out[[i]][,2]
      polygon(x = c(scale.pred, rev(scale.pred)), 
              y = c(x[n_x], ci.low, rev(c(x[n_x], ci.up))), 
              col = rgb(0,0.1,1,0.1), border = NA)
    }
    
  }
  
  else {
    if(!is.numeric(Time)){
      # time.pred
      Time_int = as.integer(as.Date(Time, origin="1970-01-01")) # 1970-01-01 is default origin
      start = Time_int[1]
      end = Time_int[length(Time)]
      time.pred = end + (1:n.ahead)/freq

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
      for(i in 1:n.level){
        ci.low = out[[i]][,1]
        ci.up = out[[i]][,2]
        polygon(x = c(scale.pred, rev(scale.pred)), 
                y = c(x[n_x], ci.low, rev(c(x[n_x], ci.up))), 
                col = rgb(0,0.1,1,0.1), border = NA)
      }
    }
    else {
      # time.pred
      time.pred = end + (1:n.ahead)/freq
      scales.pred = c(end, time.pred)
      
      # Make frame
      make_frame(x_range = range(c(Time, time.pred)), 
                 y_range = range(c(x,pred, ci.low, ci.up), na.rm = TRUE), 
                 xlab = name_time, ylab = name_ts, main = main) 
      
      # Add lines
      couleur = "blue4"
      lines(Time, x, type = "l", col = couleur, lty = 1)
      
      lines(scales.pred, c(x[n_x], pred), type = "l", col = couleur, lty = 2)
      
      # Add CI
      for(i in 1:n.level){
        ci.low = out[[i]][,1]
        ci.up = out[[i]][,2]
        polygon(x = c(scale.pred, rev(scale.pred)), 
                y = c(x[n_x], ci.low, rev(c(x[n_x], ci.up))), 
                col = rgb(0,0.1,1,0.1), border = NA)
      }
      
    }
    
  }
}


# ----- plot_pred for gmwm/rgmwm method
# here model is the 'fitsimts' object
# this is to be used in the predict.fitsimts function
plot_pred_gmwm = function(x, model, n.ahead, level = NULL, 
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
  if(model$demean==TRUE){
    a = predict(model$mod, model$Xt - model$sample_mean, n.ahead = n.ahead) 
    pred = a$pred+model$sample_mean
    se = a$se
  }else{
    a = predict(model$mod, model$Xt, n.ahead=10) 
    pred = a$pred
    se = a$se
  }
  
  if(is.null(level)){
    level = c(0.50, 0.95)
  }
  n.level = length(level)
  out = list()   # stores all CI of all levels
  for (i in 1:n.level){
    ci.up = pred+qnorm(1- (1-level[i])/2)*se
    ci.low = pred-qnorm(1- (1-level[i])/2)*se
    CI = matrix(c(ci.low, ci.up), nrow = length(ci.low), ncol = 2)
    out[[i]] = CI
  }
  
  
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
    time.pred = end + (1:n.ahead)/freq
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
    for(i in 1:n.level){
      ci.low = out[[i]][,1]
      ci.up = out[[i]][,2]
      polygon(x = c(scale.pred, rev(scale.pred)), 
              y = c(x[n_x], ci.low, rev(c(x[n_x], ci.up))), 
              col = rgb(0,0.1,1,0.1), border = NA)
    }
    
  }
  
  else {
    if(!is.numeric(Time)){
      # time.pred
      Time_int = as.integer(as.Date(Time, origin="1970-01-01")) # 1970-01-01 is default origin
      start = Time_int[1]
      end = Time_int[length(Time)]
      time.pred = end + (1:n.ahead)/freq
      
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
      for(i in 1:n.level){
        ci.low = out[[i]][,1]
        ci.up = out[[i]][,2]
        polygon(x = c(scale.pred, rev(scale.pred)), 
                y = c(x[n_x], ci.low, rev(c(x[n_x], ci.up))), 
                col = rgb(0,0.1,1,0.1), border = NA)
      }
    }
    else {
      # time.pred
      time.pred = end + (1:n.ahead)/freq
      scales.pred = c(end, time.pred)
      
      # Make frame
      make_frame(x_range = range(c(Time, time.pred)), 
                 y_range = range(c(x,pred, ci.low, ci.up), na.rm = TRUE), 
                 xlab = name_time, ylab = name_ts, main = main) 
      
      # Add lines
      couleur = "blue4"
      lines(Time, x, type = "l", col = couleur, lty = 1)
      
      lines(scales.pred, c(x[n_x], pred), type = "l", col = couleur, lty = 2)
      
      # Add CI
      for(i in 1:n.level){
        ci.low = out[[i]][,1]
        ci.up = out[[i]][,2]
        polygon(x = c(scale.pred, rev(scale.pred)), 
                y = c(x[n_x], ci.low, rev(c(x[n_x], ci.up))), 
                col = rgb(0,0.1,1,0.1), border = NA)
      }
      
    }
    
  }
}