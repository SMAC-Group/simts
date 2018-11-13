##########################
# Some Help Functions
##########################

search_grid = function(p, d, q){
  o = expand.grid(p,d,q)
  colnames(o) = c("p","d","q")
  o
}

select_arima_ = function(xt, p = 0L, d = 0L, q = 0L,
                         include.mean = FALSE, class = NULL){
  
  ic = NULL
  
  # Compute sample size
  n = length(xt)
  
  # Make models
  a = search_grid(p,d,q)
  
  split(as.matrix(a), row(a)) %>% # Pop by row.
    map(~ arima(xt, order = ., include.mean = include.mean)) -> b
  
  b %>% map_df(broom::glance) -> model_stats
  
  b %>% map_dbl(AIC, k = 2*log(log(n))) -> HQ
  
  model_stats$HQ = HQ
  
  model_stats = cbind(a, model_stats)
  
  model_stats %>%
    mutate(models = b) %>%
    gather(ic, value, AIC:HQ) %>%
    group_by(ic) %>%
    mutate(minval = (min(value) == value)) -> model_select
  
  structure(model_select,
            n = n,
            class = c(class, "select_arima","data.frame"))
}



##########################
# Selection Functions
##########################

#' @title Run Model Selection Criteria on ARIMA Models
#' @description This function performs model fitting and calculates the model selection criteria to be plotted or used in \code{best_model} function.
#' @param xt           A \code{vector} of univariate time series. 
#' @param p.min        An \code{integer} indicating the lowest order of AR(p) process to search.
#' @param p.max        An \code{integer} indicating the highest order of AR(p) process to search.
#' @param d            An \code{integer} indicating the differencing order for the data.
#' @param q.min        An \code{integer} indicating the lowest order of MA(q) process to search.
#' @param q.max        An \code{integer} indicating the highest order of MA(q) process to search.
#' @param include.mean A \code{bool} indicating whether to fit ARIMA with the mean or not.
#' @param plot         A \code{logical}. If \code{TRUE} (the default) a plot should be produced.
#' @export
#' @examples 
#' xt = gen_arima(N=100, ar=0.3, d=1, ma=0.3)
#' x = select_arima(xt, d=1L)
#' 
#' xt = gen_ma1(100, 0.3, 1)
#' x = select_ma(xt, q.min=2L, q.max=5L)
#' best_model(x)
#' 
#' xt = gen_arma(10, c(.4,.5), c(.1), 1, 0)  
#' x = select_arma(xt, p.min = 1L, p.max = 4L,
#'                 q.min = 1L, q.max = 3L)
#' @rdname select_arima
#' @importFrom purrr map map_df map_dbl
#' @importFrom dplyr group_by mutate
#' @importFrom tidyr gather
#' @importFrom broom glance
#' @importFrom stats AIC
#' @importFrom magrittr %>%
select_arima = function(xt,
                        p.min = 0L, p.max = 3L,
                        d = 0L,
                        q.min = 0L, q.max = 3L,
                        include.mean = TRUE, plot = TRUE){
  
  out = select_arima_(xt,
                    p = p.min:p.max,
                    d = d,
                    q = q.min:q.max,
                    include.mean = include.mean,
                    class = "select_arma")
  
  if (plot == TRUE){
      plot_select_arma(x=out)
  }
  invisible(out)
}

#' @export
#' @rdname select_arima
select_arma = function(xt,
                       p.min = 0L, p.max = 3L,
                       q.min = 0L, q.max = 3L,
                       include.mean = TRUE,
                       plot = TRUE){
  
  out = select_arima_(xt,
                    p = p.min:p.max,
                    d = 0L,
                    q = q.min:q.max,
                    include.mean = include.mean,
                    class = "select_arma")
  if (plot == TRUE){
    plot_select_arma(x=out)
  }
  
  invisible(out)
}

print.select_arma = function(x) {
  
  cat("Use plot() to see the model criteria or best_model() to select which model you want")
  
}

#' @export
#' @rdname select_arima
select_ar = function(xt, p.min = 0L, p.max = 3L,
                     include.mean = TRUE, plot = TRUE){
  out = select_arima_(xt,
                p = p.min:p.max,
                d = 0L,
                q = 0L,
                include.mean = include.mean,
                class = "select_ar")
  
  if (plot == TRUE){
    plot_select_ar(x=out)
  }
  invisible(out)
}


#' @export
#' @rdname select_arima
select_ma = function(xt, q.min = 0L, q.max = 3L,
                     include.mean = TRUE, plot = TRUE){
  out = select_arima_(xt,
                p = 0L,
                d = 0L,
                q = q.min:q.max,
                include.mean = include.mean,
                class = "select_ma")
  
  if (plot == TRUE){
    plot_select_ma(x=out)
  }
  invisible(out)
}


#' @title Select the Best Model
#' @description This function retrieves the best model from a selection procedure.
#' @param x  An object of class
#'  \code{\link{select_arma}}, \code{\link{select_ar}} or \code{\link{select_ma}}.
#' @param ic A \code{string} indicating the type of criterion to use in selecting the best model. 
#' Supported criteria include "aic" (AIC), "bic" (BIC) and "hq" (HQ).
#'  
#' @export
#' @examples  
#' set.seed(18)
#' xt = gen_arima(N=100, ar=0.3, d=1, ma=0.3)
#' x = select_arima(xt, d=1L)
#' best_model(x, ic = "aic")
#' 
#' set.seed(19)
#' xt = gen_ma1(100, 0.3, 1)
#' x = select_ma(xt, q.min=2L, q.max=5L)
#' best_model(x, ic = "bic")
#' 
#' set.seed(20)
#' xt = gen_arma(100, c(.3,.5), c(.1), 1, 0)  
#' x = select_arma(xt, p.min = 1L, p.max = 4L,
#'                 q.min = 1L, q.max = 3L)
#' best_model(x, ic = "hq")
#' 
#' @importFrom dplyr filter_
best_model = function(x, ic = "aic"){
  
  criterion = switch(tolower(ic),
                     "aic" = "AIC",
                     "bic" = "BIC",
                     "hq" = "HQ",
                     stop("`criterion` not supported!"))
  
  crt = paste0("ic == '", criterion,"' & (minval == TRUE)")
  
  x %>%
    filter_(crt) -> o
  
  o$models[[1]]$call$order = eval(parse(text=paste0("c(",o$p,",",o$d,",",o$q,")")))
  
  o$models[[1]]
}


##########################
# Plotting Functions
##########################

# For select_ar

plot_select_ar = function(x){
  # Labs and title
  xlab = "Autoregressive Order (p)"
  ylab = "Criterion Values"
  main = "Model Selection for Autoregressive Process"
  
  # Store values to prepare for plotting
  p_max = max(x$p)
  p_min = min(x$p)
  p_length = p_max - p_min + 1
  aic_value = rep(NA, p_length)
  bic_value = rep(NA, p_length)
  hq_value = rep(NA, p_length)
  
  for (i in 1:p_length){
    aic_value[i] = x$value[i]
    bic_value[i] = x$value[i+p_length]
    hq_value[i] = x$value[i+2*p_length]
  }
  
  
  # Main plot
  plot(NA, xlim = c(p_min, p_max), ylim = range(x$value), 
       xlab = xlab, ylab = ylab, 
       xaxt = 'n', yaxt = 'n', bty = "n", ann = FALSE, cex.lab = 1.15)
  win_dim = par("usr")
  
  par(new = TRUE)
  plot(NA, xlim = c(p_min, p_max), ylim = c(win_dim[3], win_dim[4] + 0.09*(win_dim[4] - win_dim[3])),
       xlab = xlab, ylab = ylab, xaxt = 'n', yaxt = 'n', bty = "n", cex.lab = 1.15)
  win_dim = par("usr")
  
  # Add grid
  grid(NULL, NULL, lty = 1, col = "grey95")
  
  # Add title
  x_vec = c(win_dim[1], win_dim[2], win_dim[2], win_dim[1])
  y_vec = c(win_dim[4], win_dim[4],
            win_dim[4] - 0.09*(win_dim[4] - win_dim[3]),
            win_dim[4] - 0.09*(win_dim[4] - win_dim[3]))
  polygon(x_vec, y_vec, col = "grey95", border = NA)
  text(x = mean(c(win_dim[1], win_dim[2])),
       y = (win_dim[4] - 0.09/2*(win_dim[4] - win_dim[3])), 
       main)
  
  # Add axes and box
  lines(x_vec[1:2], rep((win_dim[4] - 0.09*(win_dim[4] - win_dim[3])),2), col = 1)
  box()
  axis(1, at = seq(p_min, p_max), padj = 0.3)
  y_axis = axis(2, labels = FALSE, tick = FALSE)
  y_axis = y_axis[y_axis < (win_dim[4] - 0.09*(win_dim[4] - win_dim[3]))]
  axis(2, padj = -0.2, at = y_axis)
  
  # Plotting
  col_aic = "#F8766DFF"
  col_bic = "#00BA38FF"
  col_hq = "#619CFFFF"
  lines(seq(p_min, p_max), aic_value, col = col_aic, lwd = 2)
  lines(seq(p_min, p_max), bic_value, col = col_bic, lwd = 2)
  lines(seq(p_min, p_max), hq_value, col = col_hq, lwd = 2)
  
  # Add best models
  points(x$p[which.min(aic_value)], aic_value[which.min(aic_value)], 
         col = col_aic, pch = 16, cex = 2)
  points(x$p[which.min(bic_value)], bic_value[which.min(bic_value)], 
         col = col_bic, pch = 16, cex = 2)
  points(x$p[which.min(hq_value)], hq_value[which.min(hq_value)], 
         col = col_hq, pch = 16, cex = 2)
  
  # # Add legend
  # usr = par("usr")
  # lgd = legend(x = mean(c(usr[1],usr[2])), 
  #              y =  mean(c(usr[3],usr[4])),
  #              plot = F,
  #              legend = c("AIC", "BIC", "HQ"))
  # 
  # #x = usr[1] + lgd$rect$w*0.08,
  # #     y =  usr[4] - lgd$rect$h*0.3,
  # legend("bottomleft",
  #        legend = c("AIC", "BIC", "HQ"), 
  #        text.col = rep("black", 3),
  #        lty = rep(1,3),
  #        pch = rep(16,3),
  #        pt.cex = 1.5,
  #        lwd = 1.5,
  #        col = c(col_aic, col_bic, col_hq),
  #        bty = "n", cex = 1.25)
  #   #,
  #   # x.intersp = 0.5,
  #   # y.intersp = 0.5)
  
  legend("topright",
         legend = c("AIC", "BIC", "HQ"),
         text.col = rep("black", 3),
         lty = rep(1,3),
         pch = rep(16,3),
         pt.cex = 1.5,
         lwd = 1.5,
         col = c(col_aic, col_bic, col_hq),
         bty = "n", cex = 1.25,
         inset = c(0, 0.05))
}



plot_select_ma = function(x){
  # Labs and title
  xlab = "Moving Average Order (q)"
  ylab = "Criterion Values"
  main = "Model Selection for Moving Average Process"
  
  # Store values to prepare for plotting
  q_max = max(x$q)
  q_min = min(x$q)
  q_length = q_max - q_min + 1
  aic_value = rep(NA, q_length)
  bic_value = rep(NA, q_length)
  hq_value = rep(NA, q_length)
  
  for (i in 1:q_length){
    aic_value[i] = x$value[i]
    bic_value[i] = x$value[i+q_length]
    hq_value[i] = x$value[i+2*q_length]
  }
  
  
  # Main plot
  plot(NA, xlim = c(q_min, q_max), ylim = range(x$value), 
       xlab = xlab, ylab = ylab, 
       xaxt = 'n', yaxt = 'n', bty = "n", ann = FALSE, cex.lab = 1.15)
  win_dim = par("usr")
  
  par(new = TRUE)
  plot(NA, xlim = c(q_min, q_max), ylim = c(win_dim[3], win_dim[4] + 0.09*(win_dim[4] - win_dim[3])),
       xlab = xlab, ylab = ylab, xaxt = 'n', yaxt = 'n', bty = "n", cex.lab = 1.15)
  win_dim = par("usr")
  
  # Add grid
  grid(NULL, NULL, lty = 1, col = "grey95")
  
  
  # Add title
  x_vec = c(win_dim[1], win_dim[2], win_dim[2], win_dim[1])
  y_vec = c(win_dim[4], win_dim[4],
            win_dim[4] - 0.09*(win_dim[4] - win_dim[3]),
            win_dim[4] - 0.09*(win_dim[4] - win_dim[3]))
  polygon(x_vec, y_vec, col = "grey95", border = NA)
  text(x = mean(c(win_dim[1], win_dim[2])),
       y = (win_dim[4] - 0.09/2*(win_dim[4] - win_dim[3])), 
       main)
  
  # Add axes and box
  lines(x_vec[1:2], rep((win_dim[4] - 0.09*(win_dim[4] - win_dim[3])),2), col = 1)
  box()
  axis(1, at = seq(q_min, q_max), padj = 0.3)
  y_axis = axis(2, labels = FALSE, tick = FALSE)
  y_axis = y_axis[y_axis < (win_dim[4] - 0.09*(win_dim[4] - win_dim[3]))]
  axis(2, padj = -0.2, at = y_axis)
  
  
  # Plotting
  col_aic = "#F8766DFF"
  col_bic = "#00BA38FF"
  col_hq = "#619CFFFF"
  lines(seq(q_min, q_max), aic_value, col = col_aic, lwd = 2)
  lines(seq(q_min, q_max), bic_value, col = col_bic, lwd = 2)
  lines(seq(q_min, q_max), hq_value, col = col_hq, lwd = 2)
  
  # Add best models
  points(x$q[which.min(aic_value)], aic_value[which.min(aic_value)], 
         col = col_aic, pch = 16, cex = 2)
  points(x$q[which.min(bic_value)], bic_value[which.min(bic_value)], 
         col = col_bic, pch = 16, cex = 2)
  points(x$q[which.min(hq_value)], hq_value[which.min(hq_value)], 
         col = col_hq, pch = 16, cex = 2)
  
  # # Add legend
  # usr = par("usr")
  # lgd = legend(x = mean(c(usr[1],usr[2])), 
  #              y =  mean(c(usr[3],usr[4])),
  #              plot = F,
  #              legend = c("AIC", "BIC", "HQ"))
  # legend("bottomleft",
  #        legend = c("AIC", "BIC", "HQ"), 
  #        text.col = rep("black", 3),
  #        lty = rep(1,3),
  #        pch = rep(16,3),
  #        pt.cex = 1.5,
  #        lwd = 1.5,
  #        col = c(col_aic, col_bic, col_hq),
  #        bty = "n", cex = 1.25)
  
  legend("topright",
         legend = c("AIC", "BIC", "HQ"),
         text.col = rep("black", 3),
         lty = rep(1,3),
         pch = rep(16,3),
         pt.cex = 1.5,
         lwd = 1.5,
         col = c(col_aic, col_bic, col_hq),
         bty = "n", cex = 1.25,
         inset = c(0, 0.05))
}


# ----- For select_arma
plot_select_arma = function(x){
  # Labs and title
  xlab = "Moving Average Order (q)"
  ylab = "Criterion Values"
  
  # Store values to prepare for plotting
  p_max = max(x$p); p_min = min(x$p); p_length = p_max - p_min + 1
  q_max = max(x$q); q_min = min(x$q); q_length = q_max - q_min + 1
  aic_value = x$value[1:(p_length*q_length)]
  bic_value = x$value[(p_length*q_length+1):(p_length*q_length*2)]
  hq_value = x$value[(p_length*q_length*2+1):(p_length*q_length*3)]
  col_aic = "#F8766DFF"; col_bic = "#00BA38FF"; col_hq = "#619CFFFF"
  
  # Store values of best model
  aic_region = 1:(p_length*q_length)
  bic_region = (p_length*q_length+1):(p_length*q_length*2)
  hq_region = (p_length*q_length*2+1):(p_length*q_length*3)
  
  best_index_aic = which.min(x$value[aic_region])
  best_value_aic = min(x$value[aic_region])
  best_p_aic = (x$p[aic_region])[best_index_aic]
  best_q_aic = (x$q[aic_region])[best_index_aic]
  
  best_index_bic = which.min(x$value[bic_region])
  best_value_bic = min(x$value[bic_region])
  best_p_bic = (x$p[bic_region])[best_index_bic]
  best_q_bic = (x$q[bic_region])[best_index_bic]
  
  best_index_hq = which.min(x$value[hq_region])
  best_value_hq = min(x$value[hq_region])
  best_p_hq = (x$p[hq_region])[best_index_hq]
  best_q_hq = (x$q[hq_region])[best_index_hq]
  
  # Main plot
  par(mfrow = c(ceiling(q_length / 2), 2))
  
  for (i in 1:q_length){
    # Main plot
    plot(NA, xlim = c(p_min, p_max), ylim = range(x$value), 
         xlab = xlab, ylab = ylab, 
         xaxt = 'n', yaxt = 'n', bty = "n", ann = FALSE)
    win_dim = par("usr")
    
    par(new = TRUE)
    plot(NA, xlim = c(p_min, p_max), ylim = c(win_dim[3], win_dim[4] + 0.09*(win_dim[4] - win_dim[3])),
         xlab = "Autoregressive Order (p)", ylab = NA, xaxt = 'n', yaxt = 'n', bty = "n")
    win_dim = par("usr")
    
    # Add grid
    grid(NULL, NULL, lty = 1, col = "grey95")
    
    # Add title
    x_vec = c(win_dim[1], win_dim[2], win_dim[2], win_dim[1])
    y_vec = c(win_dim[4], win_dim[4],
              win_dim[4] - 0.09*(win_dim[4] - win_dim[3]),
              win_dim[4] - 0.09*(win_dim[4] - win_dim[3]))
    polygon(x_vec, y_vec, col = "grey95", border = NA)
    text(x = mean(c(win_dim[1], win_dim[2])),
         y = (win_dim[4] - 0.09/2*(win_dim[4] - win_dim[3])), 
         paste("q = ",toString(seq(q_min, q_max)[i]), sep = ""))
    
    # Add axes and box
    lines(x_vec[1:2], rep((win_dim[4] - 0.09*(win_dim[4] - win_dim[3])),2), col = 1)
    box()
    axis(1, at = seq(p_min, p_max), padj = 0.3)
    y_axis = axis(2, labels = FALSE, tick = FALSE)
    y_axis = y_axis[y_axis < (win_dim[4] - 0.09*(win_dim[4] - win_dim[3]))]
    axis(2, padj = -0.2, at = y_axis)
    
    # Plotting
    lines(seq(p_min, p_max), aic_value[((i-1)*p_length+1): (i*p_length)],
          col = col_aic, lwd = 2)
    lines(seq(p_min, p_max), bic_value[((i-1)*p_length+1): (i*p_length)],
          col = col_bic, lwd = 2)
    lines(seq(p_min, p_max), hq_value[((i-1)*p_length+1): (i*p_length)],
          col = col_hq, lwd = 2)
    
    # Add best model
    if ((i-1) == best_q_aic){points(best_p_aic, best_value_aic, col = col_aic, pch = 16, cex = 2)}
    if ((i-1) == best_q_bic){points(best_p_bic, best_value_bic, col = col_bic, pch = 16, cex = 2)}
    if ((i-1) == best_q_hq){points(best_p_hq, best_value_hq, col = col_hq, pch = 16, cex = 2)}
    
    
    # Add legend
    if(i==1){
      # usr = par("usr")
      # lgd = legend(x = mean(c(usr[1],usr[2])), 
      #              y =  mean(c(usr[3],usr[4])),
      #              plot = F,
      #              legend = c("AIC", "BIC", "HQ"))
      # legend("bottomleft",
      #        legend = c("AIC", "BIC", "HQ"), 
      #        text.col = rep("black", 3),
      #        lty = rep(1,3),
      #        pch = rep(16,3),
      #        pt.cex = 1.5,
      #        lwd = 1.5,
      #        col = c(col_aic, col_bic, col_hq),
      #        bty = "n", cex = 1.25)
      
      # legend("topright",
      #        legend = c("AIC", "BIC", "HQ"),
      #        text.col = rep("black", 3),
      #        lty = rep(1,3),
      #        pch = rep(16,3),
      #        pt.cex = 1.5,
      #        lwd = 1.5,
      #        col = c(col_aic, col_bic, col_hq),
      #        bty = "n", cex = 1.25,
      #        inset = c(-0.15, -0.08),
      #        y.intersp = 0.5)
      
      legend("topright",
             legend = c("AIC", "BIC", "HQ"),
             text.col = rep("black", 3),
             lty = rep(1,3),
             pch = rep(16,3),
             pt.cex = 1.5,
             lwd = 1.5,
             col = c(col_aic, col_bic, col_hq),
             bty = "n", cex = 1.25,
             inset = c(0, 0.05))
      
    }
  }
  
  # Add overall x,y labels
  mtext(xlab, side = 1, outer = TRUE, line = -1)
  mtext(ylab, side = 2, outer = TRUE, line = -1)
  par(mfrow = c(1,1))
}
