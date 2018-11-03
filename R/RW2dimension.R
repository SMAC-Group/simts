#' @title Function to Compute Direction Random Walk Moves
#' @description The RW2dimension function computes direction random walk moves.
#' @author Stéphane Guerrier
#' @param steps An \code{integer} that counts the number of steps of the random walk.
#' @param probs A \code{vector} of \code{double} that specifies the probabilities to choose each direction.
#' @importFrom stats runif
#' @export
#' @examples
#' RW2dimension(steps = 50, probs = c(0.2, 0.5, 0.6))

# Function computes direction random walk moves
RW2dimension = function(steps = 100, probs = c(0.25, 0.5, 0.75)){
  
  couleur = "blue4"
  xlab = "X-position"
  ylab = "Y-position"
  main = NULL
  pt_col = NULL
  pt_pch = 16
  pt.cex = 2
  leg_pos = NULL
  
  # Title
  if (is.null(main)){
    main = paste("Simulated 2D RW with", steps, "steps", sep = " ")
  }
  
  # Points colors
  if (is.null(pt_col)){
    hues = seq(15, 375, length = 3)
    pt_col = hcl(h = hues, l = 65, c = 100)[1:2]
  }
  
  # Initial matrix
  step_direction = matrix(0, steps+1, 2)

  # Start random walk
  for (i in seq(2, steps+1)){
    # Draw a random number from U(0,1)
    rn = runif(1)

    # Go right if rn \in [0,prob[1])
    if (rn < probs[1]) {step_direction[i,1] = 1}

    # Go left if rn \in [probs[1], probs[2])
    if (rn >= probs[1] && rn < probs[2]) {step_direction[i,1] = -1}

    # Go forward if rn \in [probs[2], probs[3])
    if (rn >= probs[2] && rn < probs[3]) {step_direction[i,2] = 1}

    # Go backward if rn \in [probs[3],1]
    if (rn >= probs[3]) {step_direction[i,2] = -1}
  }

  # Cumulative steps
  position = data.frame(x = cumsum(step_direction[, 1]), 
                        y = cumsum(step_direction[, 2]))
  
  par(mar = c(5.1, 5.1, 1, 2.1))

  # Main plot
  plot(NA, xlim = range(position[,1]), ylim = range(range(position[,2])), 
       xlab = xlab, ylab = ylab, xaxt = 'n', 
       yaxt = 'n', bty = "n", ann = FALSE)
  win_dim = par("usr")

  par(new = TRUE)
  plot(NA, xlim = range(position[,1]), ylim = c(win_dim[3], win_dim[4] + 0.09*(win_dim[4] - win_dim[3])),
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

  # Add trajectory
  lines(position, type = "l", col = couleur, pch = 16)
  
  # Start and end points
  points(c(0,position[steps+1,1]), c(0,position[steps+1,2]), cex = pt.cex,
         col = pt_col, pch = pt_pch)
  
  # Legend
  if (is.null(leg_pos)){
    leg_pos = c(min(position[,1]), max(position[,2]))
  }
  legend(leg_pos[1], leg_pos[2], c("Start","End"), 
         col = pt_col, pch = pt_pch, pt.cex = pt.cex, bty = "n")
}

