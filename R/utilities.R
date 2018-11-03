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
make_frame = function(x_range, y_range, xlab, ylab, main = "", 
                      mar = c(5.1, 5.1, 1, 2.1), add_axis_x = TRUE,
                      add_axis_y = TRUE, col_box = "black", 
                      col_grid = "grey95", col_band = "grey95",
                      col_title = "black", add_band = TRUE,
                      title_band_width = 0.09, grid_lty = 1){  
  
  if (!add_band){
    title_band_width = 0
    main = ""
  }
  
  par(mar = mar)
  
  # Main plot                     
  plot(NA, xlim = x_range, ylim = y_range, xlab = xlab, ylab = ylab, 
       xaxt = 'n', yaxt = 'n', bty = "n", ann = FALSE)

  win_dim = par("usr")
  par(new = TRUE)
  
  plot(NA, xlim = x_range, 
       ylim = c(win_dim[3], win_dim[4] + title_band_width*(win_dim[4] - win_dim[3])),
       xlab = xlab, ylab = ylab, xaxt = 'n', yaxt = 'n', bty = "n")
  win_dim = par("usr")
  
  # Add grid
  grid(NULL, NULL, lty = grid_lty, col = col_grid)

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
    y_axis = axis(2, labels = FALSE, tick = FALSE)  
    y_axis = y_axis[y_axis < (win_dim[4] - title_band_width*(win_dim[4] - win_dim[3]))]
    axis(2, padj = -0.2, at = y_axis)  
  }
}

#' @title Custom legend function 
#' @description Legend placement function
#' @importFrom graphics legend
#' @keywords internal
custom_legend = function(x, usr = par("usr"), ...){
  lgd = legend(x = mean(c(usr[1],usr[2])), 
               y =  mean(c(usr[3],usr[4])),
               plot = F, ...)
  
  if(x == "topleft"){
    # Plot on top right
    legend(x = usr[1] + lgd$rect$w*1,
           y =  usr[4] - lgd$rect$h*0.8,
           plot = T, ...)
  }else if(x == "top"){
    # Plot on top right
    legend(x = (usr[1] + usr[2])/2 - lgd$rect$w/2,
           y =  usr[4] - lgd$rect$h,
           plot = T, ...)
  }else if(x == "topright"){
    # Plot on top right
    legend(x = usr[2] - lgd$rect$w*1.5,
           y =  usr[4] - lgd$rect$h*0.8,
           plot = T, ...)
  }else{
    legend(x = x,
           plot =T, ...)
  }
 
}

