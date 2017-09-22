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
                      loc_x = NULL, custom_label = NULL, 
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
  
  if(add_axis_x && is.null(custom_label) && is.null(x_loc)){
    axis(1, padj = 0.3)
  }else if(!is.null(custom_label) && !is.null(x_loc)){
    axis(1, labels = custom_label, at = loc_x, padj = 0.3)
  }else if(is.null(custom_label) && !is.null(x_loc)){
    axis(1, labels = loc_x, at = loc_x)
  }
  
  if (add_axis_y){
    y_axis = axis(2, labels = FALSE, tick = FALSE)  
    y_axis = y_axis[y_axis < (win_dim[4] - title_band_width*(win_dim[4] - win_dim[3]))]
    axis(2, padj = -0.2, at = y_axis)  
  }
}
