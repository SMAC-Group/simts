# Copyright (C) 2014 - 2017  James Balamuta, Stephane Guerrier, Roberto Molinari
#
# This file is part of simts R Methods Package
#
# The `simts` R package is free software: you can redistribute it and/or modify it
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

#' @title Order the Model
#' @description Orders the model and changes it to the correct format
#' @param models A vector of \code{string} that specifies the models
#' @details If the \code{models} are c("AR1", "WN", "AR1", "WN", "AR1+WN+AR1+WN"), it will be converted to 
#' c("AR1-1", "WN-1", "AR1-2", "WN-2", "AR1+WN+AR1+WN").
#' 
#' This function is used in \code{gen.lts()}
#' @keywords internal
#' @export
#' @examples 
#' models = c("AR1", "WN", "AR1", "WN", "AR1+WN+AR1+WN")
#' new.models = orderModel(models)
#' new.models
#' 
#' models = c('AR1', 'QN', 'WN', 'AR1+QN+WN')
#' new.models = orderModel(models)
#' new.models
orderModel = function(models){
  count = table(models)
  if( any(count>1)){
    multi.models = names( count[count>1] )
    
    for(model in multi.models){
      num = count[model]
      models[models == model] = paste( rep(model, num), rep('-', num), 1:num, sep = '' )
    }
    
    return(models)
  }else{
    return(models)
  }
}
