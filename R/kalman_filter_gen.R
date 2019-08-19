################################
### Kalman Filter estimation
################################

#hidden diagsum and sum_cov fcts

#define diagsum fct
diagsum = function(x){
  sum(diag(x))
}

#define sum cov fct
sum_cov = function(x){
  sum(x[lower.tri(x)])
}

#' @title Kalman Filter Estimation 
#' @description Compute the Kalman Filter estimation for the sum of any combinations of AR processes, RW, WN and DR
#' @param model a \code{ts.model} object with specified parameters values
#' @param y a \code{lts} object 
#' @param estimate_model a bolean indicating whether or not to estimate the parameters values of a provided model, default is \code{False}
#' @param model_to_estimate a \code{ts.model} object without parameters values specified if \code{estimate_model} is set to \code{True}
#' @param method specify the method of estimation if \code{estimate_model} is set to \code{True} and \code{model_to_estimate} is provided
#' @return a \code{KF} object with the structure:
#' \describe{
#' \item{X_h}{Estimated states for each time \code{t}}
#' \item{P_h}{Estimated variance-covariance matrix for each time \code{t}}
#' \item{y}{Observed time serie}
#' }
#' @details   
#' Compute a kalman filter on a state space model which
#' can be composed of the sum of AR(), RW(), DR() and WN() processes.
#' Note that there can be any ammount of AR() processes while RW(), DR() and WN()
#' processes are limited to 1. The function can both estimate the model parameter
#' from a given model and apply the kalman filter or directly apply the kalman 
#' filter with specified parameters. The function takes as input the
#' model which is the defined model and y which is the observed time serie.
#' If estimate_model is set to False (by default), the user need to provide a model
#' and its parameters (a ts.model class object). If estimate_model is set to True, the user just need to provide
#' the selected model to estimate and the estimation method (see function estimate)
#' @examples 
#' #Filter a 2*AR1 + DR + RW + WN process
#' set.seed(123)
#' model = AR(.3, 2) + AR(.5,3) + DR(.1) + RW(3) + WN(4) 
#' n = 250
#' y = gen_lts(n = n, model = model)
#' my_res = kalman_filter(model = model, y = y)
#' @importFrom dplyr right_join
#' @importFrom dplyr mutate
#' @importFrom dplyr arrange
#' @importFrom stats filter
#' @author Lionel Voirol
#' @export
kalman_filter = function(model, y, estimate_model = F, model_to_estimate = NULL, method = 'mle'){

  #if generated via gen_lts, take the last column, i.e. the sum of all processes
  y_decomp = y
  if (!is.null(dim(y))){
    y = y[, dim(y)[2]]
  }
  
  #return error if estimate_model == T and model to estimate is empty
  if(estimate_model == T & is.null(model_to_estimate)) stop("No defined model to estimate")
  
  #extract info from model and build matrices
  #define specific order
  my_order = data.frame('sel_order' = c(1,2,3,4,4), 'process' = c('WN','DR','RW','AR','SIGMA2'))
  
  #If the user provides the model form and parameters
  if(estimate_model == F){
    df = data.frame('process'= model$process.desc, 'val' = model$theta)
  }
  #if user want to estimate a given model
  if (estimate_model == T){
    #estimate model
    estimated_model = estimate(model = model_to_estimate, Xt = y , method = method)
    #store in dataframe
    df = data.frame('process.desc' = rownames(estimated_model$mod$estimate),
                    'theta' = estimated_model$mod$estimate)
    #rename colnames
    colnames(df) = c('process', 'val')
  }
  
  #join possible processes and model processes, had to do a little trick to avoid warnings
  combined = sort(union(levels(df$process), levels(my_order$process)))
  join_df <- right_join(mutate(my_order, process=factor(process, levels=combined)),
                        mutate(df, process=factor(process, levels=combined)), by = 'process')
  
  #isolate all processes and values
  
  #wn
  wn_process = join_df %>% arrange(sel_order) %>% filter(sel_order == 1)
  wn_val = wn_process$val
  measurment_error = wn_val
  
  #dr
  dr_process = join_df %>% arrange(sel_order) %>% filter(sel_order == 2)
  dr_process_val = dr_process$val
  
  #rw
  rw_process = join_df %>% arrange(sel_order) %>% filter(sel_order == 3)
  rw_process_val = rw_process$val
  
  #ar
  ar_processes = join_df %>% arrange(sel_order) %>% filter(sel_order == 4)
  dim_ar_processes = dim(ar_processes)[1]
  if(dim_ar_processes == 0){
    phi_vec = NULL
  }else{
    phi_vec_index = seq(1, dim_ar_processes, 2) #identify the phi parameters in the AR processes
    phi_vec = ar_processes[phi_vec_index,]$val  #extract the phi parameters in the AR processes
  }
  
  #define columns names for output
  ar_names = c()
  for(i in seq(dim(ar_processes)[1] %/% 2)){
    ar_names = c(ar_names, paste("AR1() -", i))
  }
  dr_name = rep('DR()', dim(dr_process)[1])
  rw_name = rep('RW()', dim(rw_process)[1])
  column_names = c(ar_names, rw_name, dr_name)
  
  #Define trans mat 
  dr_process_length = dim(dr_process)[1]
  dr_vec = rep(1, dr_process_length)
  rw_process_length = dim(rw_process)[1]
  rw_vec = rep(1, rw_process_length)
  if(length(c(phi_vec, rw_vec, dr_vec)) == 1) {
    trans_mat = c(phi_vec, rw_vec, dr_vec)
  } else{
    trans_mat = diag(x = c(phi_vec, rw_vec, dr_vec))
  }
  
  #state transition
  #trans_mat %* X_t + T * U where the T * U stands for the matrix multiplication for the DR() process
  
  #define T mat
  t_phi_rw = rep(0, length(c(phi_vec, rw_vec)))
  t_dr = rep(1, length(dr_vec))
  if(length(c(t_phi_rw, t_dr)) == 1) {
    T_mat = c(t_phi_rw, t_dr)
  } else{
    T_mat = diag(c(t_phi_rw, t_dr))
  }
  
  #define U vec
  U_vec = c(rep(0, length(c(phi_vec, rw_vec))), dr_process_val)
  
  #initial measurment
  x_0 = y[1]
  
  #number of obs
  n = length(y)
  
  #initial process_noise_cov_mat defined as the matrix multiplication of the process noise vector
  #only ar processes and rw processes have a wn terms added, dr will have a wn term of 0
  p_length = length(c(phi_vec, rw_vec, dr_vec))
  if(dim_ar_processes == 0){
    phi_wn_vec = NULL
  } else {
    phi_wn_vec_index = seq(2, dim_ar_processes, 2)
    phi_wn_vec = ar_processes[phi_wn_vec_index,]$val
  }
  dr_wn_vec = t_dr = rep(0, length(dr_vec))
  p_0 = diag(length(c(phi_vec, rw_vec, dr_vec))) * 1e-6
  if(length(c(phi_vec, rw_vec, dr_vec)) == 1){
    process_noise_cov_mat = c(phi_wn_vec, rw_process_val, dr_wn_vec)
  } else {
    process_noise_cov_mat = diag(c(phi_wn_vec, rw_process_val, dr_wn_vec))
  }
  
  #Define measurment error and if null set to 0
  if(length(measurment_error) == 0){
    measurment_error = 0
  } else {
    measurment_error = wn_val
  }
  #creation of empty list of matrices and 2 vectors 
  X_t = list() 
  X_h = list()
  P_t = list()
  P_h = list()
  K   = list() #Kalman gain
  
  #define the matrix H that transform State space to measurement space, here just the sum of all states
  #we only observe the final time serie which is the sum of all state for each time t
  total_states_length = length(c(phi_vec, rw_vec, dr_vec))
  H = matrix(rep(1, total_states_length), ncol = total_states_length)
  
  
  #Initialization
  #define initial X_t
  if(length(trans_mat) == 1){
    X_t_length = 1
  } else{
    X_t_length = dim(trans_mat)[1]
  }
  x0 = y[1]
  x00 = x0 / X_t_length
  X_t[[1]] = X_h[[1]] = matrix(c(rep(x00, X_t_length)), nrow = X_t_length)
  P_t[[1]] = P_h[[1]] = p_0
  
  for (k in seq(n-1)){
    # estimate of next state and error given observation untill t
    X_t[[k+1]] = trans_mat %*% X_h[[k]] + T_mat %*% U_vec
    P_t[[k+1]] = trans_mat %*% P_h[[k]] %*% t(trans_mat) + process_noise_cov_mat
    
    #update estimate given measurment t
    #Note that there is no measurment error in this model
    innovation = y[k+1] - H %*% X_t[[k+1]] #innovation
    innovation_cov = H %*% P_t[[k+1]] %*% t(H) + measurment_error #innovation covariance
    K[[k+1]]   = P_t[[k+1]] %*% t(H) %*% solve(innovation_cov)  #update optimal Kalman gain
    X_h[[k+1]] = X_t[[k+1]] + K[[k+1]] %*% innovation #update state prediction
    P_h[[k+1]] = (diag(X_t_length)-K[[k+1]] %*% H) %*% P_t[[k+1]] #update process_noise_cov_mat
  }
  
  #Structure output
  length(X_t)
  X_t_mat = matrix(unlist(X_t), nrow = length(X_t), byrow = T)
  colnames(X_t_mat) = column_names
  rownames(X_t_mat) = as.character(seq(dim(X_t_mat)[1]))
  X_h_mat = matrix(unlist(X_h), nrow = length(X_h), byrow = T)
  colnames(X_h_mat) = column_names
  rownames(X_h_mat) = as.character(seq(dim(X_h_mat)[1]))
  #Return output
  out = list("forecast" = X_t_mat, "forecast_cov_mat" = P_t, 
             "filter" = X_h_mat, "filter_cov_mat" = P_h,
             "y" = y, "y_d" = y_decomp, print = model$print)
  class(out) = "KF"
  invisible(out)
}



#' @title Plot Kalman filter estimate
#' @description The function plots the observed time serie, the sum of the estimated states and the confidence intervals of the estimate
#' @param x             A \code{KF} object resulting from the function \code{kalman_filter}
#' @param plot_state    A numeric value indicating which state to plot. Default is "all".
#' @param ...           Additional arguments affecting the plot produced.
#' @importFrom dplyr select
#' @importFrom stats filter
#' @author Lionel Voirol
#' @export
plot.KF = function(x, plot_state = "all", ...){
  obj = x
  alpha = 0.05
  n = dim(obj$filter)[1]
  estimated = rowSums(obj$filter)
  make_frame(x_range = c(0, dim(obj$filter)[1]), xlab = "Time", ylab = "Value", y_range = range(estimated), main = obj$print)
  lines(obj$y, col = 'blue4')
  lines(estimated, col = "#F8766DFF")
  var_vec = sapply(obj$filter_cov_mat, FUN = diagsum) + 2 * sapply(obj$filter_cov_mat, FUN = sum_cov)
  fit_sd = sqrt(var_vec)
  polygon(x = c(1:n, rev(1:n)),
          y = c(estimated + qnorm(1-alpha/2)*-fit_sd,
                rev(estimated + qnorm(1-alpha/2)*fit_sd)), border = NA, col = "#F8766D4D")
  legend("bottomleft", col = c("blue4", "#F8766DFF", "#F8766D4D"), 
         lwd = c(1,1,NA), pch = c(NA, NA, 15), pt.cex = c(NA, NA, 1.5), bty = 'n',
         legend = c("Observed time series", 'Estimated sum of the states', 'Sum of the states CI'))
  X_h = obj$filter
  y = as.data.frame(obj$y_d)
  ar_processes_val = dplyr::select(y, contains('SARIMA'))
  dr_process_val = dplyr::select(y, contains('DR'))
  rw_process_val = dplyr::select(y, contains('RW'))
  true_processes = cbind(ar_processes_val, rw_process_val, dr_process_val)
  P_h = obj$filter_cov_mat
  var_vec = lapply(P_h, diag)
  var_df = data.frame(matrix(unlist(var_vec), nrow=length(var_vec), byrow=T))
  if(plot_state == "all"){
    dim_p = dim(true_processes)[2]
    par(mfrow = c(dim_p, 1))
    for(i in seq(dim_p-1)){
      par(mai=c(0.02,0.5,0.02,0.02))
      make_frame(x_range = range(0, dim(true_processes)[1]), xlab = "Time",ylab = "Value", main = colnames(obj$filter)[i], y_range = range(true_processes[,i]))
      lines(true_processes[,i], type = 'l', col = 'blue4',xaxt='n')
      lines(X_h[,i], col = "#F8766DFF")
      fit_sd = var_df[,i]
      n=length(var_df[,1])
      alpha = .05
      polygon(x = c(1:n, rev(1:n)),
              y = c(X_h[,i] + qnorm(1-alpha/2)*-fit_sd,
                    rev(X_h[,i] + qnorm(1-alpha/2)*fit_sd)), border = NA, col = "#F8766D4D")
    }
    #last state add axis values
    i = i+1
    par(mai=c(0.55,0.5,0.02,0.02))
    make_frame(x_range = range(0, dim(true_processes)[1]), xlab = "Time", ylab = "Value", main = colnames(obj$filter)[i], y_range = range(true_processes[,i]))
    lines(true_processes[,i], type = 'l', col = 'blue4')
    lines(X_h[,i], col = "#F8766DFF")
    fit_sd = var_df[,i]
    n=length(var_df[,1])
    alpha = .05
    polygon(x = c(1:n, rev(1:n)),
            y = c(X_h[,i] + qnorm(1-alpha/2)*-fit_sd,
                  rev(X_h[,i] + qnorm(1-alpha/2)*fit_sd)), border = NA, col = "#F8766D4D")
    legend("bottomleft",cex= 1, col = c('#2E9AFE', "#F8766DFF", "#F8766D4D"), 
           lwd = c(1,1,NA), pch = c(NA, NA, 15), pt.cex = c(NA, NA, 1.5), bty = 'n',
           legend = c("Observed time series", 'Estimated sum of the states', 'Sum of the states CI'))
    par(mfrow=c(1,1))
  }else{
    make_frame(x_range = range(0, dim(true_processes)[1]), xlab = "Time", ylab = "Value", main = colnames(obj$filter)[plot_state], y_range = range(true_processes[,plot_state]))
    lines(true_processes[,plot_state], type = 'l', col = 'blue4')
    lines(X_h[,plot_state], col = "#F8766DFF")
    fit_sd = var_df[,plot_state]
    n=length(var_df[,1])
    alpha = .05
    polygon(x = c(1:n, rev(1:n)),
            y = c(X_h[,plot_state] + qnorm(1-alpha/2)*-fit_sd,
                  rev(X_h[,plot_state] + qnorm(1-alpha/2)*fit_sd)), border = NA, col = "#F8766D4D")
    legend("bottomleft",cex= 1, col = c('blue4', "#F8766DFF", "#F8766D4D"), 
           lwd = c(1,1,NA), pch = c(NA, NA, 15), pt.cex = c(NA, NA, 1.5), bty = 'n',
           legend = c("Observed time series", 'Estimated sum of the states', 'Sum of the states CI'))
    
    
  } 
  
}


#for devlopement purposes
# estimate_model = F
# model_to_estimate = NULL
# library(simts)
# set.seed(123)
# model = AR(.7,2) + AR(.65,1) + RW(.6) +DR(.02) + WN(6)
# n = 250
# y = gen_lts(n = n, model = model)
# plot(y)
# res = kalman_filter(y = y, model = model)
# plot(res)
# plot(res, plot_state = 3)

#Example
#Filter a 2*AR1 + DR + RW + WN process
#set.seed(123)
# model = AR(.3, 2) + AR(.5,3) + DR(.1) + RW(3) + WN(4) 
# n = 250
# y = gen_lts(n = n, model = model)
# my_res = kalman_filter(model = model, y = y)

