#' @title Create an IMU Object
#' @description Builds an IMU object that provides the program with gyroscope, accelerometer, and axis information per column in the dataset.
#' @param data A \code{vector} which contains data, or a \code{matrix} or \code{data.frame} which contains the data in each column.
#' @param gyros A \code{vector} that contains the index of columns where gyroscope data (such as Gyro. X, Gyro. Y and Gyro. Z) is placed.
#' @param accels A \code{vector} that contains the index of columns where accelerometer data (such as Accel. X, Accel. Y and Accel. Z) is placed.
#' @param axis A \code{vector} that indicates the axises, such as 'X', 'Y', 'Z'. Please supply the axises for gyroscope data before that for accelerometer data, if gyroscope data exists.
#' @param freq An \code{integer} that provides the frequency for the data.
#' @param unit A \code{string} that contains the unit expression of the frequency. Default value is \code{NULL}.
#' @param name A \code{string} that provides an identifier to the data. Default value is \code{NULL}.
#' @return An \code{imu} object in the following attributes:
#' \describe{
#'   \item{sensor}{A \code{vector} that indicates whether data contains gyroscope sensor, accelerometer sensor, or both.}
#'   \item{num.sensor}{A \code{vector} that indicates how many columns of data are for gyroscope sensor and accelerometer sensor.}
#'   \item{axis}{Axis value such as 'X', 'Y', 'Z'.}
#'   \item{freq}{Observations per second.}
#'   \item{unit}{String representation of the unit.}
#'   \item{name}{Name of the dataset.}
#' }
#' @details 
#' \code{data} can be a numeric vector, matrix or data frame.
#' 
#' \code{gyros} and \code{accels} cannot be \code{NULL} at the same time, but it will be fine if one of them is \code{NULL}.
#' In the new implementation, the length of \code{gyros} and \code{accels} do not need to be equal.
#' 
#' In \code{axis}, duplicate elements are not alowed for each sensor. In the new implementation, please specify the axis for each column of data.
#' \code{axis} will be automatically generated if there are less than or equal to 3 axises for each sensor.
#' 
#' @author James Balamuta and Wenchao Yang
#' @export
#' @examples
#' \dontrun{
#' if(!require("imudata")){
#'    install_imudata()
#'    library("imudata")
#' }
#' 
#' data(imu6)
#' 
#' # Example 1 - Only gyros
#' test1 = imu(imu6, gyros = 1:3, axis = c('X', 'Y', 'Z'), freq = 100)
#' df1 = wvar.imu(test1)
#' plot(df1)
#' 
#' # Example 2 - One gyro and one accelerometer
#' test2 = imu(imu6, gyros = 1, accels = 4, freq = 100)
#' df2 = wvar.imu(test2)
#' plot(df2)
#' 
#' # Example 3 - 3 gyros and 3 accelerometers
#' test3 = imu(imu6, gyros = 1:3, accels = 4:6, axis = 
#'                        c('X', 'Y', 'Z', 'X', 'Y', 'Z'), freq = 100)
#' df3 = wvar.imu(test3)
#' plot(df3)
#' 
#' # Example 4 - Custom axis
#' test4 = imu(imu6, gyros = 1:2, accels = 4:6, axis = 
#'                        c('X', 'Y', 'X', 'Y', 'Z'), freq = 100)
#' df4 = wvar.imu(test4)
#' plot(df4)
#' }
imu = function(data, gyros = NULL, accels = NULL, axis = NULL, freq = NULL, unit = NULL, name = NULL){
  
  # 1. Check object
  if(is.null(data) || !(is.numeric(data)||is.data.frame(data)||is.matrix(data)) ) {
    stop('Data must a numeric vector, data frame, or matrix.')
  }
  
  if(is.numeric(data)){
    data = as.matrix(data)
  }
  
  if(is.data.frame(data)){
    data = as.matrix(data)
  }
  colnames(data) = NULL
  
  # 2. Check gyro and acce
  gyro = gyros
  acce = accels
  
  ngyros = length(gyro)
  nacces = length(acce)
  
  if(is.null(gyro) && is.null(acce)){
    stop("At lease one of parameters ('gyros' or 'accels') must be not NULL.") 
  }
  
  # Merge indices
  index = c(gyro, acce)
  
  if(!is.whole(index)){
    stop("Paramater 'gyros' and 'accels' must be vectors of integers.")
  }
  
  if(any(gyro > ncol(data)) || any(gyro < 1)){
    stop('Index for gyroscope is out of bound.')
  }
  if(any(acce > ncol(data)) || any(acce < 1)){
    stop('Index for accelerometer is out of bound.')
  }
  
  # 3. Check 'axis': if the user supplies the axis, check input to make sure it is 'good'.
  if(!is.null(axis)){
    
    if(length(axis)==((ngyros + nacces)/2) && ngyros!=0 && nacces!=0){
      axis = rep(axis, times = 2)
    }else if (length(axis) != (ngyros + nacces)){
      stop('Please specify the axis for each column of data.')
    }
    
    if (ngyros == 0||nacces == 0){
      if( anyDuplicated(axis) ){
        stop('`axis` cannot have duplicated elements.')
      }
    }else if (anyDuplicated(axis[1:ngyros]) || anyDuplicated(axis[(ngyros+1):length(axis)])){
      stop('For each sensor, `axis` cannot have duplicated elements.')
    }
    
  }else{
    # if the user doesn't supply the axis, guess number of sensors
    if(ngyros > 0 && nacces > 0){
      naxis = if(ngyros == nacces) ngyros else 0
    }else{
      naxis = if(ngyros != 0) ngyros else nacces
    }
    
    axis = switch(as.character(naxis),
                  '1' = 'X',
                  '2' = c('X','Y'),
                  '3' = c('X','Y','Z'),
                  stop('axis cannot be automatically generated. Please supply it by specifying "axis = ...".')
    )
    
    if(ngyros == nacces){
      axis = rep(axis, times = 2)
    }
    
  }
  
  # 4. Check freq
  if(is.null(freq)){
    freq = 100
    warning("`freq` has not been specified. Setting `imu` data's frequency to 100. \n Please recreate the object if the frequency is incorrect.")
  }
  if(!is(freq,"numeric") || length(freq) != 1){ stop("'freq' must be one numeric number.") }
  if(freq <= 0) { stop("'freq' must be larger than 0.") }
  
  # 5. do not need 'start' and 'end'
  
  # 6. unit = NULL
  if(!is.null(unit)){
    if(!unit %in% c('ns', 'ms', 'sec', 'second', 'min', 'minute', 'hour', 'day', 'mon', 'month', 'year')){
      stop('The supported units are "ns", "ms", "sec", "min", "hour", "day", "month", "year". ')
    }
  }
  
  create_imu(data[,index, drop = F], ngyros, nacces, axis, freq, unit = unit, name = name)
}

#' @title Internal IMU Object Construction
#' @description Internal quick build for imu object.
#' @param data A \code{matrix} with dimensions N x length(index)
#' @param ngyros An \code{integer} containing the number of gyroscopes
#' @param nacces An \code{integer} containing the number of accelerometers
#' @param axis A \code{vector} unique representation of elements e.g. x,y,z or x,y or x.
#' @param freq An \code{integer} that provides the frequency for the data.
#' @param unit A \code{string} that contains the unit expression of the frequency. Default value is \code{NULL}.
#' @param name A \code{string} that provides an identifier to the data. Default value is \code{NULL}.
#' @param stype A \code{string} that describes the sensor type. Default value is \code{NULL}.
#' @return An \code{imu} object class.
#' @author James Balamuta
#' @export
#' @keywords internal
create_imu = function(data, ngyros, nacces, axis, freq, unit = NULL, name = NULL, stype = NULL){
  
  if(!is.null(ncol(data))){
    if(ngyros>0 && nacces>0){
      colnames(data) = paste( c(rep('Gyro.', times = ngyros), rep('Accel.', times = nacces)), axis)
    }else if (ngyros > 0){
      colnames(data) = c(paste(rep('Gyro.', times = ngyros), axis))
    }else{
      colnames(data) = c(paste(rep('Accel.', times = nacces), axis))
    }
  }
  
  out = structure(data, 
                  sensor = c(rep("Gyroscope",ngyros), rep("Accelerometer",nacces)),
                  num.sensor = c(ngyros, nacces),
                  axis = axis,
                  freq = freq,
                  unit = unit,
                  name = name,
                  stype = stype,
                  class = c("imu","matrix"))
  
}

#' Subset an IMU Object
#' 
#' Enables the IMU object to be subsettable. That is, you can load all the data in and then select certain properties.
#' @export
#' @param x    A \code{imu} object
#' @param i    A \code{integer vector} that specifies the rows to subset. If blank, all rows are selected.
#' @param j    A \code{integer vector} that specifies the columns to subset. Special rules apply see details.
#' @param drop A \code{boolean} indicating whether the structure should be preserved or simplified.
#' @return An \code{imu} object class.
#' @details 
#' When using the subset operator, note that all the Gyroscopes are placed at the front of object 
#' and, then, the Accelerometers are placed.
#' 
#' @examples 
#' \dontrun{
#' if(!require("imudata")){
#' install_imudata()
#' library("imudata")
#' }
#' 
#' data(imu6)
#' 
#' # Create an IMU Object that is full. 
#' ex = imu(imu6, gyros = 1:3, accels = 4:6, axis = c('X', 'Y', 'Z', 'X', 'Y', 'Z'), freq = 100)
#' 
#' # Create an IMU object that has only gyros. 
#' ex.gyro = ex[,1:3]
#' ex.gyro2 = ex[,c("Gyro. X","Gyro. Y","Gyro. Z")]
#' 
#' # Create an IMU object that has only accels. 
#' ex.accel = ex[,4:6]
#' ex.accel2 = ex[,c("Accel. X","Accel. Y","Accel. Z")]
#' 
#' # Create an IMU object with both gyros and accels on axis X and Y
#' ex.b = ex[,c(1,2,4,5)]
#' ex.b2 = ex[,c("Gyro. X","Gyro. Y","Accel. X","Accel. Y")]
#' 
#' }
#' 
`[.imu` = function(x, i, j, drop = FALSE){
  
  axis = attr(x,"axis")
  sensor = attr(x,"sensor")
  num.sensor = attr(x,"num.sensor")
  
  # If j is missing, then it is a subset by row (not column!)
  if(!missing(j)){
    
    # Select column names picked by user
    if(is(j, "character")){
      nc = j
    }else{
      # Otherwise, use j as a numeric.
      nc = colnames(x)[j]
    }
    
    # TO DO:
    # Rewrite the selection using indices now that
    # we are no longer bound by naming schemes.
    
    # Remove structure to get Gyros/Accels
    g = gsub("\\..*","",nc)
    ng = table(g)
    
    # Remove structure to get at X,Y,Z axis.
    g2 = gsub(".* ","",nc)
    axis = g2
    
    num.sensor = c({if(!is.na(ng["Gyro"])) ng["Gyro"] else 0}, {if(!is.na(ng["Accel"])) ng["Accel"] else 0})
  }
  
  create_imu(NextMethod("[", drop = drop),
             num.sensor[1], num.sensor[2], axis, attr(x,"freq"), attr(x,"unit"), attr(x,"name"), attr(x,"stype"))
  
}

#' @title Read an IMU Binary File into R
#' 
#' @description 
#' Process binary files within the 
#' 
#' @param file A \code{string} containing file names or paths.
#' @param type A \code{string} that contains a supported IMU type given below.
#' @param unit A \code{string} that contains the unit expression of the frequency. Default value is \code{NULL}.
#' @param name A \code{string} that provides an identifier to the data. Default value is \code{NULL}.
#' @details
#' Currently supports the following IMUs:
#' \itemize{
#' \item IMAR
#' \item LN200
#' \item LN200IG
#' \item IXSEA
#' \item NAVCHIP_INT
#' \item NAVCHIP_FLT
#' }
#' @author James Balamuta
#' We hope to soon be able to support delimited files.
#' @return An \code{imu} object that contains 3 gyroscopes and 3 accelerometers in that order.
#' @references
#' Thanks goes to Philipp Clausen of Labo TOPO, EPFL, Switzerland, topo.epfl.ch, Tel:+41(0)21 693 27 55
#' for providing a matlab function that reads in IMUs.
#' This function is a heavily modified port of MATLAB code into Armadillo/C++.
#' @examples
#' \dontrun{
#' # Relative
#' setwd("F:/")
#' 
#' a = read.imu(file = "Documents/James/short_test_data.imu", type = "IXSEA")
#' 
#' # Fixed path
#' b = read.imu(file = "F:/Desktop/short_test_data.imu", type = "IXSEA")
#' }
read.imu = function(file, type, unit = NULL, name = NULL){
  d = read_imu(file_path = file, imu_type = type)
  
  obj = create_imu(d[[1]][,-1], 3, 3, c('X','Y','Z','X','Y','Z'), d[[2]][1], unit = unit, name = name, stype = type)
  
  rownames(obj) = d[[1]][,1]
  
  obj
}