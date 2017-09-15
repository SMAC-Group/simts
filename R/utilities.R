# Copyright (C) 2014 - 2017  
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

#' GM Conversion
#' 
#' Convert from AR1 to GM and vice-versa
#' @param theta        A \code{numeric vector} containing the theta values
#' @param process.desc A \code{character vector} containing the names of parameters.
#' @param freq         A \code{double} indicating the frequency of the data.
#' @keywords internal
#' @export
#' @author James Balamuta
#' @rdname gm_conv
conv.ar1.to.gm = function(theta, process.desc, freq){
  idx = process.desc %in% c("BETA","SIGMA2_GM")
  theta[idx] = ar1_to_gm(theta[idx],freq)
  
  theta
}

#' @rdname gm_conv
#' @export
conv.gm.to.ar1 = function(theta, process.desc, freq){
  idx = process.desc %in% c("BETA","SIGMA2_GM")
  theta[idx] = gm_to_ar1(theta[idx],freq)
  
  theta
}


#' @title Print simts Objects
#' @keywords internal
#' @method print imu
#' @description 
#' Pretty formatting for \code{gts}, \code{imu}, and \code{lts} objects.
#' @param x         A \code{gts}, \code{imu}, \code{lts} object.
#' @param obs       A \code{integer} the specifies how many from the beginning and end of the data set to show.
#' @param row.names A \code{boolean} that indicates whether row names should be displayed or surpressed.
#' @param ...       Further arguments passed to or from other methods.
#' @return 
#' A \code{logical} value that indicates whether the object is of that class (TRUE) or not (FALSE).
#' @author James Balamuta
#' @rdname print_data
#' @export
print.imu = function(x,
                     obs = 10L,
                     row.names = TRUE, ...)
{
  if(!is.null(attr(x,"name"))){
    cat("Data Name:",attr(x,"name"),"\n")
  }
  if(!is.null(attr(x,"stype"))){
    cat("Sensor:",attr(x,"stype"),"@",attr(x,"freq"),"Hz\n")
    cat("Obs:", nrow(x), " over ", round(nrow(x)/attr(x,"freq")/3600,2),"Hours \n")
  }else{
    cat("Freq:",attr(x,"freq"),"Hz\n")
  }
  outf(x, obs, row.names, ...)
}

#' @rdname print_data
#' @method print lts
#' @export
print.lts = function(x,
                     obs = 10L,
                     row.names = TRUE, ...)
{
  outf(x, obs, row.names, ...)
}

#' @rdname print_data
#' @method print gts
#' @export
print.gts = function(x,
                     obs = 10L,
                     row.names = TRUE, ...)
{
  outf(x, obs, row.names, ...)
}

#' @rdname print_data
outf = function(x, obs = 10L, row.names = TRUE){
  if(!is.numeric(obs)){ obs = 100L }
  if(!is.infinite(obs)){ obs = as.integer(obs) }
  
  if (obs*2 < nrow(x)) {
    print_lines = rbind(head(x,obs), tail(x, obs))
    rn = c(seq_len(obs), seq.int(to=nrow(x), length.out=obs))
    print_dashes = TRUE
  } else {
    print_lines = head(x,nrow(x))
    rn = seq_len(nrow(x))
    print_dashes = FALSE
  }
  
  if (isTRUE(row.names)){
    rownames(print_lines) = paste(format(rn,right=TRUE,scientific=FALSE),":",sep="")
  }else{
    rownames(print_lines) = rep("", nrow(print_lines))
  }
  if(is.null(colnames(x))){
    colnames(print_lines) = rep("NA", ncol(print_lines))
  }
  if(print_dashes) {
    print_lines = rbind(head(print_lines,obs),"---"="",tail(print_lines,obs))
    rownames(print_lines) = format(rownames(print_lines),justify="right")
  }
  
  print.default(print_lines,right=TRUE,quote=FALSE)
  return(invisible())
}

#' @title Is simts Object
#' @description 
#' Is the object a
#' \code{gts}, \code{imu}, or \code{lts} object?
#' @param x  A \code{gts}, \code{imu}, \code{lts} object.
#' @return 
#' A \code{logical} value that indicates whether the object is of that class (TRUE) or not (FALSE).
#' @details
#'  Uses \code{\link[base]{inherits}} over \code{\link[methods]{is}} for speed. 
#' @author James Balamuta
#' @rdname is_func
#' @export
is.gts = function(x){ inherits(x, "gts") }

#' @rdname is_func
#' @export
is.imu = function(x){ inherits(x, "imu") }

#' @rdname is_func
#' @export
is.lts = function(x){ inherits(x, "lts") }

#' @rdname is_func
#' @export
is.ts.model = function(x){ inherits(x, "ts.model") }


#' @title Obtain the value of an object's properties
#' @description 
#' Used to access different properties of the
#'  \code{gts}, \code{imu}, or \code{lts} object.
#' @param x      A \code{gts}, \code{imu}, or \code{lts} object.
#' @param type   A \code{string} indicating the field to be retrieved.
#' @return 
#' The method will return a single numeric or string result depending on the
#' slot being accessed.
#' @details 
#' To access information about \code{imu} properties use:
#' \describe{
#'  \item{\code{"accel"}}{Returns the number of accelerometers}
#'  \item{\code{"gyro"}}{Returns the number of gyroscopes}
#'  \item{\code{"sensors"}}{Returns total number of sensors}
#' }
#' @author James Balamuta
#' @export
value = function(x, type){
  UseMethod("value")
}

#' @describeIn value Access \code{imu} object properties
#' @export
value.imu = function(x, type){
  switch(type,
         accel   = attr(x, 'num.sensor')[1],
         gyro    = attr(x, 'num.sensor')[2],
         sensors = sum(attr(x, 'num.sensor')),
         stop("The `type` specified is not an available slot")
  ) 
}

#' @title Obtain the value of an object's properties
#' @keywords internal
#' @description 
#' Used to access different properties of the
#'  \code{gts}, \code{imu}, or \code{lts} object.
#' @param x      A \code{gts}, \code{imu}, or \code{lts} object.
#' @param type   A \code{string} indicating the field to be retrieved.
#' @return 
#' The method will return a single TRUE or FALSE response
#' @details 
#' To access information about \code{imu} properties use:
#' \describe{
#'  \item{\code{"accel"}}{Returns whether accelerometers have been specified}
#'  \item{\code{"gyro"}}{Returns whether accelerometers have been specified}
#'  \item{\code{"sensors"}}{Returns whether there exists both types of sensors}
#' }
#' @author James Balamuta
#' @export
has = function(x, type){
  UseMethod("has")
}

#' @describeIn has Access \code{imu} object properties
#' @export
has.imu = function(x, type){
  switch(type,
         accel   = attr(x, 'num.sensor')[1] > 0,
         gyro    = attr(x, 'num.sensor')[2] > 0,
         sensors = attr(x, 'num.sensor')[1] > 0 & attr(x, 'num.sensor')[2] > 0,
         stop("The `type` specified is not an available slot")
  ) 
}

#' Pulls the IMU time from the IMU object
#' 
#' Helper function for the IMU object to access \code{rownames()} with a numeric conversion.
#' @param x A \code{imu} object
#' @return A \code{vector} with numeric information.
imu_time = function(x){
  
  if(!is.imu(x)){ stop("`x` must be an `imu` object.")}
  
  # If the IMU object does not have a built in time stamp (not made w/ read.imu)
  if(!is.null(rownames(x))){
    1:nrow(x)
  }else{
    # Pull time information and cast as.numeric
    as.numeric(rownames(x))
  }
}

#' @title Default utility function for various plots titles
#' @description Adds title, grid, and required x- and y-axes. 
#' @param x_range           A \code{numeric} providing the range of values for the x-axis. 
#' @param y_range           A \code{numeric} providing the range of values for the y-axis. 
#' @param xlab              A \code{string} that gives a title for the x-axis.
#' @param ylab              A \code{string} that gives a title for the y-axis.
#' @param main              A \code{string} that gives an overall title for the plot. Default is an empty string. 
#' @param mar               A \code{vector} indicating overall margin values for the plot. 
#' @param add_axis_x        A \code{boolean} indicating whether a x-axis should be added.  
#' @param add_axis_y        A \code{boolean} indicating whether a y-axis should be added.
#' @param col_box           A \code{string} indicating the color for the title box. 
#' @param col_grid          A \code{string} indicating the color of the grid for the plot. 
#' @param col_band          A \code{string} indicating the color of the band. 
#' @param col_title         A \code{string} indicating the color of the plot title. 
#' @param add_band          A \code{boolean} indicating whether there should be a band. 
#' @param title_band_width  A \code{double} providing the value of the band width. Default is 0.09. 
#' @param grid_lty          A \code{integer} indicating the line type of the grid lines. 
#' @return Added title, grid, and axes. 
#' @export 
#' @author Stephane Guerrier and Justin Lee 
#' @examples 
#' make_frame(x_range = c(0, 1), y_range = c(0, 1), xlab = "my xlab", 
#'            ylab = "my ylab", main = "my title")
#'            
#' make_frame(x_range = c(0, 1), y_range = c(0, 1), xlab = "my xlab", 
#'            ylab = "my ylab", add_band = FALSE)
#'            
#' make_frame(x_range = c(0, 1), y_range = c(0, 1), xlab = "my xlab", 
#'            ylab = "my ylab", main = "my title", col_band = "blue3", 
#'            col_title = "white", col_grid = "lightblue", grid_lty = 3)
#'            
#' make_frame(x_range = c(0, 1), y_range = c(0, 1), xlab = "my xlab", 
#'            ylab = "my ylab", main = "my title", col_band = "blue3", 
#'            col_title = "white", col_grid = "lightblue", grid_lty = 3,
#'            title_band_width = 0.18)
make_frame = function(x_range = c(0, 1), y_range = c(0, 1), xlab = "", ylab = "",
                      transform_x = NULL, transform_y = NULL,
                      main = "", mar = c(5.1, 5.1, 1, 2.1), 
                      add_axis_x = TRUE, add_axis_y = TRUE, 
                      nb_ticks_x = NULL, nb_ticks_y = NULL,
                      unit_x = NULL, unit_y = NULL, 
                      col_box = "black", col_grid = "grey95", 
                      col_band = "grey95", col_title = "black", 
                      add_band = TRUE, title_band_width = 0.09, grid_lty = 1){  
  
  # Axes
  # if (is.null(nb_ticks_x)){
  #   nb_ticks_x = 6
  # }
  # 
  # if (is.null(nb_ticks_y)){
  #   nb_ticks_y = 5
  # }
  
  if (!add_band){
    title_band_width = 0
    main = ""
  }
  
  if (!is.null(transform_x)){
    if (identical(transform_x, log10)){
      x_low = floor(log10(x_range[1]))
      x_high = ceiling(log10(x_range[2]))
      transform_x = 10
    }else if(identical(transform_x, log2)){
      x_low = floor(log2(x_range[1]))
      x_high = ceiling(log2(x_range[2]))
      transform_x = 2
    }else{
      stop("You have entered an invalid transformation. Please choose between log10 or log2.") # Maybe change the way this is said...
    }
  }else{
    x_low = floor(x_range[1])
    x_high = ceiling(x_range[2])
  }  
  
  if (!is.null(transform_y)){
    if (identical(transform_y, log10)){
      y_low = floor(log10(y_range[1]))
      y_high = ceiling(log10(y_range[2]))
      transform_y = 10
    }else if(identical(transform_y, log2)){
      y_low = floor(log2(y_range[1]))
      y_high = ceiling(log2(y_range[2]))
      transform_y = 2
    }else{
      stop("You have entered an invalid transformation. Please choose between log10 or log2.") # Maybe change the way this is said...
    }
  }else{
    y_low = floor(y_range[1])
    y_high = ceiling(y_range[2])
  }  
  
  x_ticks = seq(x_low, x_high, by = 1)
  # if (length(x_ticks) > nb_ticks_x){
  #   x_ticks = x_low + ceiling((x_high - x_low)/(nb_ticks_x + 1))*(0:nb_ticks_x)
  # }
  y_ticks = seq(y_low, y_high, by = 1)
  # if (length(y_ticks) > nb_ticks_y){
  #   y_ticks = y_low + ceiling((y_high - y_low)/(nb_ticks_y + 1))*(0:nb_ticks_y)
  # }  

  if(!is.null(transform_x)){
    if(transform_x == 2){
      x_labels = sapply(x_ticks, function(i) as.expression(bquote(2^ .(i))))
      x_axis = 2^x_ticks
    }else if(transform_x == 10){
      x_labels = sapply(x_ticks, function(i) as.expression(bquote(10^ .(i))))
      x_axis = 10^x_ticks
    }
  }else{ # choose default labels as numbers 
    x_labels = x_ticks
  }
  
  if(!is.null(transform_y)){
    if(transform_y == 2){
      y_labels = sapply(y_ticks, function(i) as.expression(bquote(2^ .(i))))
      y_axis = 2^y_ticks
    }else if(transform_y == 10){
      y_labels = sapply(y_ticks, function(i) as.expression(bquote(10^ .(i))))
      y_axis = 10^y_ticks
    }
  }else{ # choose default labels as numbers 
    y_labels = y_ticks
  }
  
  # Concatenate x axis (unit) when needed 
  if (!is.null(unit_x)){
    if (class(unit_x) == "name" || class(unit_x) == "call"){
      xlab = comb(xlab, " (", unit_x, ")")
    }else{
      xlab = paste(xlab, " (", unit_x, ")", sep = "")
    }
  }
  
  # Concatenate y axis (unit) when needed
  if (!is.null(unit_y)){
    if (class(unit_y) == "name" || class(unit_y) == "call"){
      ylab = comb(ylab, " (", unit_y, ")")
    }else{
      ylab = paste(ylab, " (", unit_y, ")", sep = "")
    }
  }
  
  par(mar = mar)
  
  # # Main plot                     
  # plot(NA, xlim = x_range, ylim = y_range, xlab = xlab, ylab = ylab, 
  #      xaxt = 'n', yaxt = 'n', bty = "n", ann = FALSE)
  # 
  # win_dim = par("usr")
  # par(new = TRUE)
  # 
  # plot(NA, xlim = x_range, 
  #      ylim = c(win_dim[3], win_dim[4] + title_band_width*(win_dim[4] - win_dim[3])), 
  #      xlab = xlab, ylab = ylab, xaxt = 'n', yaxt = 'n', bty = "n")
  # win_dim = par("usr")
  
  ## NEW STUFF ADDED 

  if(!is.null(transform_x)){
    if(!is.null(transform_y)){
      # Main Plot                     
      plot(NA, xlim = x_range, ylim = y_range, xlab = xlab, ylab = ylab, 
           log = "xy", xaxt = 'n', yaxt = 'n', bty = "n", ann = FALSE)
    }else{
      # Main Plot                     
      plot(NA, xlim = x_range, ylim = y_range, xlab = xlab, ylab = ylab, 
           log = "x", xaxt = 'n', yaxt = 'n', bty = "n", ann = FALSE)
    }
  }else{
    if(!is.null(transform_y)){
      # Main Plot                     
      plot(NA, xlim = x_range, ylim = y_range, xlab = xlab, ylab = ylab, 
           log = "y", xaxt = 'n', yaxt = 'n', bty = "n", ann = FALSE)
    }else{
      # Main plot                     
      plot(NA, xlim = x_range, ylim = y_range, xlab = xlab, ylab = ylab, 
           xaxt = 'n', yaxt = 'n', bty = "n", ann = FALSE)
    }
  }
  win_dim = par("usr")
  par(new = TRUE)
  
  # Add grid
  if(transform_y == 2){
    plot(NA, xlim = x_range, ylim = 2^c(win_dim[3], win_dim[4] + 0.09*(win_dim[4] - win_dim[3])),
         xlab = xlab, ylab = ylab, log = "xy", xaxt = 'n', yaxt = 'n', bty = "n")
    abline(h = 2^y_ticks, lty = 1, col = "grey95")
  }else if(transform_y == 10){
    plot(NA, xlim = x_range, ylim = 10^c(win_dim[3], win_dim[4] + 0.09*(win_dim[4] - win_dim[3])),
         xlab = xlab, ylab = ylab, log = "xy", xaxt = 'n', yaxt = 'n', bty = "n")
    abline(h = 10^y_ticks, lty = 1, col = "grey95")
  }else{
    plot(NA, xlim = x_range, ylim = c(win_dim[3], win_dim[4] + 0.09*(win_dim[4] - win_dim[3])),
         xlab = xlab, ylab = ylab, log = "xy", xaxt = 'n', yaxt = 'n', bty = "n")
    abline(h = y_ticks, lty = 1, col = "grey95")
  }
  
  win_dim = par("usr")
  
  if(transform_x == 2){
    abline(h = 2^x_ticks, lty = 1, col = "grey95")
  }else if(transform_x == 10){
    abline(h = 10^x_ticks, lty = 1, col = "grey95")
  }else{
    abline(h = x_ticks, lty = 1, col = "grey95")
  }

# 
# grid(NULL, NULL, lty = grid_lty, col = col_grid)
# 

  # Add title
  x_vec = c(win_dim[1], win_dim[2], win_dim[2], win_dim[1])
  y_vec = c(win_dim[4], win_dim[4],
            win_dim[4] - title_band_width*(win_dim[4] - win_dim[3]), 
            win_dim[4] - title_band_width*(win_dim[4] - win_dim[3]))
  polygon(x_vec, y_vec, col = col_band, border = NA)
  text(x = mean(c(win_dim[1], win_dim[2])), 
       y = (win_dim[4] - title_band_width/2*(win_dim[4] - win_dim[3])), 
       main, col = col_title)
  
  # Add axes and box
  lines(x_vec[1:2], rep((win_dim[4] - title_band_width*(win_dim[4] - win_dim[3])),2), col = col_box)
  box(col = col_box)
  
  if(add_axis_x){
    axis(1, padj = 0.3)
  }
  
  if (add_axis_y){  
    new_y_axis = y_axis[y_axis < (win_dim[4] - title_band_width*(win_dim[4] - win_dim[3]))]
    axis(2, padj = -0.2, at = new_y_axis)  
  }
}


