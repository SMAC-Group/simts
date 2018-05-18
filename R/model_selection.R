search_grid = function(p, d, q){
  o = expand.grid(p,d,q)
  colnames(o) = c("p","d","q")
  o
}

#' @importFrom stats AIC
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


#' @title Run Model Selection Criteria on ARMA Models
#' @description Performs model fitting and calculates model selection criteria
#' @param xt           A data set
#' @param p.min        Lowest Order AR(P) process to search
#' @param p.max        Highest Order AR(P) process to search
#' @param q.min        Lowest Order MA(Q) process to search
#' @param q.max        Highest Order MA(Q) process to search
#' @param include.mean A \code{boolean} indicating whether to fit ARMA with the mean or not
#' 
#' @author James Balamuta and Stéphane Guerrier
#' @importFrom purrr map map_df map_dbl
#' @importFrom dplyr group_by mutate
#' @importFrom tidyr gather
#' @importFrom broom glance
#' @importFrom magrittr %>%
#' @export
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


#' @title Run Model Selection Criteria on AR Models
#' @description Performs model fitting and calculates model selection criteria
#' @param xt           A data set
#' @param p.min        Lowest Order AR(P) process to search
#' @param p.max        Highest Order AR(P) process to search
#' @param include.mean A \code{boolean} indicating whether to fit AR with the mean or not
#' 
#' @author James Balamuta and Stéphane Guerrier
#' @importFrom purrr map map_df map_dbl
#' @importFrom dplyr group_by mutate
#' @importFrom tidyr gather
#' @importFrom broom glance
#' @importFrom magrittr %>%
#' @export
select_ar = function(xt, p.min = 1L, p.max = 5L,
                     include.mean = TRUE){
  select_arima_(xt,
                p = p.min:p.max,
                d = 0L,
                q = 0L,
                include.mean = include.mean,
                class = c("select_comp_diag", "select_comp","select_ar"))
  
}


#' @title Run Model Selection Criteria on MA Models
#' @description Performs model fitting and calculates model selection criteria
#' @param xt           A data set
#' @param q.min        Lowest Order MA(Q) process to search
#' @param q.max        Highest Order MA(Q) process to search
#' @param include.mean A \code{boolean} indicating whether to fit MA with the mean or not
#' 
#' @author James Balamuta and Stéphane Guerrier
#' @importFrom purrr map map_df map_dbl
#' @importFrom dplyr group_by mutate
#' @importFrom tidyr gather
#' @importFrom broom glance
#' @importFrom magrittr %>%
#' @export
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
#' @author James Balamuta and Stéphane Guerrier
#' @export
#' @examples
#' sunspot = gts(sunspot.year, start = 1700, freq = 1)
#' sunspot_select = select_ar(sunspot, p.min = 1, p.max = 12)
#' sunspot_bm = best_model(sunspot_select, ic = "bic")
#' sunspot_bm
#' 
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