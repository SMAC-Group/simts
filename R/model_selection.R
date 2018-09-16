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
            class = c(class,"select_arima","data.frame"))
}



##########################
# Selection Functions
##########################

#' @title Run Model Selection Criteria on ARIMA Models
#' @description This function performs model fitting and calculates the model selection criteria.
#' @param xt           A \code{vector} of univariate time series. 
#' @param p.min        An \code{integer} indicating the lowest order of AR(p) process to search.
#' @param p.max        An \code{integer} indicating the highest order of AR(p) process to search.
#' @param d.min        An \code{integer} indicating the lowest difference of data to take.
#' @param d.max        An \code{integer} indicating the highest difference of data to take.
#' @param q.min        An \code{integer} indicating the lowest order of MA(q) process to search.
#' @param q.max        An \code{integer} indicating the highest order of MA(q) process to search.
#' @param include.mean A \code{bool} indicating whether to fit ARIMA with the mean or not.
#' @export
#' @examples 
#' xt = gen_arima(N=100, ar=0.3, d=1, ma=0.3)
#' x = select_arima(xt)
#' 
#' xt = gen_ma1(100, 0.3, 1)
#' x = select_ma(xt, q.min=2L, q.max=5L)
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
                        p.min = 0L, p.max = 5L,
                        d.min = 0L, d.max = 2L,
                        q.min = 0L, q.max = 5L,
                        include.mean = TRUE){
  
  o = select_arima_(xt,
                    p = p.min:p.max,
                    d = d.min:d.max,
                    q = q.min:q.max,
                    include.mean = include.mean)
  
}

#' @export
#' @rdname select_arima
select_arma = function(xt,
                       p.min = 1L, p.max = 5L,
                       q.min = 1L, q.max = 5L,
                       include.mean = TRUE){
  
  o = select_arima_(xt,
                    p = p.min:p.max,
                    d = 0L,
                    q = q.min:q.max,
                    include.mean = include.mean,
                    class = "select_arma")
  
}


#' @export
#' @rdname select_arima
select_ar = function(xt, p.min = 1L, p.max = 5L,
                     include.mean = TRUE){
  select_arima_(xt,
                p = p.min:p.max,
                d = 0L,
                q = 0L,
                include.mean = include.mean,
                class = "select_ar")
  
}


#' @export
#' @rdname select_arima
select_ma = function(xt, q.min = 1L, q.max = 5L,
                     include.mean = TRUE){
  select_arima_(xt,
                p = 0L,
                d = 0L,
                q = q.min:q.max,
                include.mean = include.mean,
                class = "select_ma")
}


#' @title Select the Best Model
#'
#' @description This function retrieves the best model.
#' @param x  An object from either \code{\link{select_arima}},
#'  \code{\link{select_arma}}, \code{\link{select_ar}}, or \code{\link{select_ma}}.
#' @param ic A \code{string} indicating the type of criterion to use in selecting the best model. 
#' Supported criteria include "AIC", "BIC" and "HQ". 
#' @export
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
  
  # Figure this out in the future...
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
       xaxt = 'n', yaxt = 'n', bty = "n", ann = FALSE)
  win_dim = par("usr")
  
  par(new = TRUE)
  plot(NA, xlim = c(p_min, p_max), ylim = c(win_dim[3], win_dim[4] + 0.09*(win_dim[4] - win_dim[3])),
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
  
  # Add legend
  usr = par("usr")
  lgd = legend(x = mean(c(usr[1],usr[2])), 
               y =  mean(c(usr[3],usr[4])),
               plot = F,
               legend = c("AIC", "BIC", "HQ"))
  legend(x = usr[1] + lgd$rect$w*0.4,
         y =  usr[4] - lgd$rect$h*0.3,
         legend = c("AIC", "BIC", "HQ"), 
         text.col = rep("black", 3),
         lty = rep(1,3),
         pch = rep(16,3),
         col = c(col_aic, col_bic, col_hq),
         bty = "n")
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
       xaxt = 'n', yaxt = 'n', bty = "n", ann = FALSE)
  win_dim = par("usr")
  
  par(new = TRUE)
  plot(NA, xlim = c(q_min, q_max), ylim = c(win_dim[3], win_dim[4] + 0.09*(win_dim[4] - win_dim[3])),
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
  
  # Add legend
  usr = par("usr")
  lgd = legend(x = mean(c(usr[1],usr[2])), 
               y =  mean(c(usr[3],usr[4])),
               plot = F,
               legend = c("AIC", "BIC", "HQ"))
  legend(x = usr[1] + lgd$rect$w*0.4,
         y =  usr[4] - lgd$rect$h*0.3,
         legend = c("AIC", "BIC", "HQ"), 
         text.col = rep("black", 3),
         lty = rep(1,3),
         pch = rep(16,3),
         col = c(col_aic, col_bic, col_hq),
         bty = "n")
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
         toString(seq(q_min, q_max)[i]))
    
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
    if (i == best_q_aic){points(best_p_aic, best_value_aic,col = col_aic, pch = 16, cex = 2)}
    if (i == best_q_bic){points(best_p_bic, best_value_bic,col = col_bic, pch = 16, cex = 2)}
    if (i == best_q_hq){points(best_p_hq, best_value_hq,col = col_hq, pch = 16, cex = 2)}
    
    
    # Add legend
    if(i==1){
      usr = par("usr")
      lgd = legend(x = mean(c(usr[1],usr[2])), 
                   y =  mean(c(usr[3],usr[4])),
                   plot = F,
                   legend = c("AIC", "BIC", "HQ"))
      legend(x = usr[1] + lgd$rect$w*0.1,
             y =  usr[4] + lgd$rect$h*0,
             legend = c("AIC", "BIC", "HQ"), 
             text.col = rep("black", 3),
             lty = rep(1,3),
             pch = rep(16,3),
             col = c(col_aic, col_bic, col_hq),
             bty = "n",
             x.intersp = 0.3,
             y.intersp = 0.3)
    }
  }
  
  # Add overall x,y labels
  mtext(xlab, side = 1, outer = TRUE, line = -1)
  mtext(ylab, side = 2, outer = TRUE, line = -1)
  
}

#' @title Visualization of Model Selection 
#' @description This function visualize the model comparison based on different model selection criteria. 
#' @param x An object that is either of type \code{\link{select_ar}},
#' \code{\link{select_ma}} or \code{\link{select_arma}}.
#' @export
#' @author Yuming Zhang
#' @examples 
#' xt = gen_ar1(100, 0.3, 1)
#' x = select_ar(xt)
#' plot(x)
#' 
#' xt = gen_ma1(100, 0.3, 1)
#' x = select_ma(xt, q.min=2L, q.max=5L)
#' plot(x)
#' 
#' xt = gen_arma(10, c(.4,.5), c(.1), 1, 0)  
#' x = select_arma(xt, p.min = 1L, p.max = 4L,
#'                 q.min = 1L, q.max = 3L)
#' plot(x)
#' 
plot.select_arima = function(x){
  
  if (!"select_ar" %in% class(x) & !"select_ma" %in% class(x) & !"select_arma" %in% class(x)){
    stop("This function can only visualize either `select_ar`, `select_ma` or `select_arma`.")
  }
  
  # ----- for select_ar
  if ("select_ar" %in% class(x)){
    plot_select_ar(x=x)
  }
  
  # ----- for select_ma
  if ("select_ma" %in% class(x)){
    plot_select_ma(x=x)
  }
  
  if ("select_arma" %in% class(x)){
    plot_select_arma(x=x)
  }
  
}



