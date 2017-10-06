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

# #' Auto select
# #'
# #' @rdname select_arima
# #' @export
# select_auto = function(xt,
#                        p.min = 1L, p.max = 6L,
#                        d.min = 0L, d.max = 2L,
#                        q.min = 1L, q.max = 6L,
#                        include.mean = TRUE){
#
#   p = p.min:p.max
#   d = d.min:d.max
#   q = q.min:q.max
#
#   # model_info =
#   # if(length(p) != 0L && length(q) == 0L){
#   #   model_info = select_ar(xt, p.min:p.max = p.max, include.mean = TRUE)
#   # } else if(length(q) != 0L && length(p) == 0L){
#   #
#   # } else{
#   #    = select_arima_(xt,
#   #                    p = p.min:p.max,
#   #                    d = d.min:d.max,
#   #                    q = q.min:q.max,
#   #                    include.mean = include.mean)
#   # }
#
#   # Obtain a plot of the model selection criteria
#   model_info %>%
#     autoplot
#
#   # Obtain the best model
#   model_info %>%
#     best_model
#
# }


#' @title Run Model Selection Criteria on ARIMA Models
#' @description Performs model fitting and calculates model selection criteria
#' @param xt           A data set
#' @param p.min        Lowest Order AR(P) process to search
#' @param p.max        Highest Order AR(P) process to search
#' @param d.min        Lowest difference of data to take
#' @param d.max        Highest difference of data to take
#' @param q.min        Lowest Order MA(Q) process to search
#' @param q.max        Highest Order MA(Q) process to search
#' @param include.mean A \code{boolean} indicating whether to fit ARIMA with the mean or not
#' 
#' @author James Balamuta
#' @export
#' @rdname select_arima
#' @importFrom purrr map map_df map_dbl
#' @importFrom dplyr group_by mutate
#' @importFrom tidyr gather
#' @importFrom broom glance
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
                       p.min = 0L, p.max = 5L,
                       q.min = 0L, q.max = 5L,
                       include.mean = TRUE){
  
  o = select_arima_(xt,
                    p = p.min:p.max,
                    d = 0,
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
                class = c("select_comp_diag", "select_comp","select_ar"))
  
}


#' @export
#' @rdname select_arima
select_ma = function(xt, q.min = 1, q.max = 10,
                     include.mean = TRUE){
  select_arima_(xt,
                p = 0L,
                d = 0L,
                q = q.min:q.max,
                include.mean = include.mean,
                class = c("select_comp_diag", "select_comp", "select_ma"))
}



#' @title Select the best model
#' @description Retrieves the best model
#' @param x  An object from either \code{\link{select_arima}},
#'  \code{\link{select_arma}}, \code{\link{select_ar}}, or \code{\link{select_ma}}.
#' @param ic Type of criterion to use in selecting the best model.
#' 
#' @author James Balamuta
#' @export
#' @examples
#' sunspot.year %>%
#'  gts(start = 1700, freq = 1) %>%
#'  select_ar(p.min = 1, p.max = 8) %>%
#'  best_model(ic = "bic")
#' @importFrom dplyr filter_
best_model = function(x, ic = "aic"){
  
  criterion = switch(tolower(ic),
                     "aic" = "AIC",
                     "bic" = "BIC",
                     "hq" = "HQ",
                     stop("`criterion` not Supported!"))
  
  crt = paste0("ic == '", criterion,"' & (minval == TRUE)")
  
  x %>%
    filter_(crt) -> o
  
  # Figure this out in the future...
  o$models[[1]]$call$order = eval(parse(text=paste0("c(",o$p,",",o$d,",",o$q,")")))
  
  o$models[[1]]
}