data <- readxl::read_excel('C:\\Users\\John\\Documents\\CGM Research\\Data\\ABPM\\70417-1 ABPM.xlsx') # Delete
data <- data[, -8]
data <- data[, c(5,3,8,6,7)]
data <- as.matrix(data)

#' Data Pre-processor
#'
#' @param data (Required) User-supplied data set. Must be either matrix or data.frame
#' @param bp_date (optional) Column index of Date or Date/Time values
#' @param sbp (Required) Column index of Systolic Blood Pressure (SBP) values
#' @param dbp (Required) Column index of Diastolic Blood Pressure (DBP) values
#' @param hr (Optional) Column index of Heart Rate (HR) values
#' @param pp (optional) Column index of Pulse Pressure (PP) values
#' @param map (optional) Column index of Mean Arterial Pressure (MAP) values
#' @param rpp (optional) Column index of Rate Pressure Product (RPP) values - Note that RPP can be directly calculated from HR and SBP values if not originally supplied
#' @param override (optional) Indicates whether or not to override automatic detection of column indexes. Defaults to zero indicating that the function will automatically detect.
#'
#' @return
#' @export
#'
#' @examples
#' process_data(data, sbp = "syst", dbp = "diast", bp_datetime = "date&time", id = "id", wake = "wake", visit = "visit", hr = "hr", pp = "pp", map = "map",rpp = "rpp")
process_data <- function(data, bp_datetime = NULL, id = NULL, wake = NULL, visit = NULL, sbp = NULL, dbp = NULL, hr = NULL, pp = NULL, map = NULL, rpp = NULL){


  # Ensure that data is either data.frame or matrix
  if(is.data.frame(data) == FALSE){

    if(is.matrix(data) == FALSE){

      stop('Invalid data type. Please use either data.frame or matrix')

      }else{

        warning('Converted matrix data type to data.frame')

        data <- as.data.frame(data)

        }
    }


  # Convert all column names to upper case for consistency
  if(is.data.frame(data)){
    colnames(data) <- toupper(colnames(data))
  }else{
    stop('Error: did not convert to data frame')
  }

  # Throw error if SBP and DBP columns aren't specified
  if(is.null(sbp) | is.null(dbp)){

      stop('Both "SBP" and "DBP" column names must be specified.')

    }




  # Systolic BP (SBP)
  if(is.character(sbp)){

    if(toupper(sbp) %in% colnames(data) == FALSE){

      warning('Could not find user-defined SBP argument name in dataset. \ni.e. for example, if user improperly defines sbp = "syst" but there is no column name in the dataset, then there will be no matches for "syst". Check spelling of SBP argument.')

      if(length(grep(paste("\\bSBP\\b", sep = ""), names(data))) == 1){

        stop('Fix user-defined argument name for SBP. \nNote: A column in the dataset DOES match the name "SBP": if this is the correct column, indicate as such in function argument. \ni.e. sbp = "SBP" \n ')

      }

    }else{

      col_idx <- grep(paste("\\b",toupper(sbp),"\\b", sep = ""), names(data))
      data <- data[, c(col_idx, (1:ncol(data))[-col_idx])]

      if(colnames(data)[1] != "SBP"){

        colnames(data)[1] <- "SBP"
        data$SBP <- as.numeric(data$SBP)
      }
    }
  } else {
    stop('User-defined SBP name must be character.')
  }# if working with numeric below, remove this bracket
  {
  # }else{
  #
  #   if(sbp > ncol(data)){
  #
  #     stop('Invalid index for SBP. Index greater than number of available columns')
  #   }
  #
  #   if(sbp < 0){
  #
  #     stop('Invalid index for SBP. Cannot have negative index')
  #   }
  #
  #   if( (ncol(data) == 2) & (sbp == 2) ){
  #
  #     data <- data[ , c(2 , 1)]
  #
  #   }else if(sbp != 1){
  #
  #     data <- data[, c(sbp, (1:ncol(data))[-sbp])]
  #     #data[ , sbp] <- as.numeric(data[ , sbp])
  #
  #   }
  # }
}




  # Diastolic BP (DBP)
  if(is.character(dbp)){

    if(toupper(dbp) %in% colnames(data) == FALSE){

      warning('User-defined DBP name does not match column name of supplied dataset. \ni.e. for example, if user improperly defines dbp = "diast" but there is no column name in the dataset, then there will be no matches for "diast". Check spelling of DBP argument.')

      if(length(grep(paste("\\bDBP\\b", sep = ""), names(data))) == 1){

        stop('Fix user-defined argument name for DBP. \nNote: A column in the dataset DOES match the name "DBP": if this is the correct column, indicate as such in function argument. \ni.e. sbp = "DBP" \n ')

      }
    }else{

      col_idx <- grep(paste("\\b",toupper(dbp),"\\b", sep = ""), names(data))
      data <- data[, c(1, col_idx, (2:ncol(data))[-col_idx+1])]

      if(colnames(data)[2] != "DBP"){

        colnames(data)[2] <- "DBP"
        data$DBP <- as.numeric(data$DBP)
      }
    }
  } else {
    stop('User-defined DBP name must be character.')
    }# if working with numeric below, remove this bracket
  {
  # }else{
  #
  #   if(dbp > ncol(data)){
  #
  #     stop('Invalid index for DBP. Index greater than number of available columns')
  #   }
  #
  #   if(dbp < 0){
  #
  #     stop('Invalid index for DBP. Cannot have negative index')
  #   }
  #
  #   if(sbp == 1){
  #
  #     data <- data[, c(1, dbp, (2:ncol(data))[-dbp+1])]
  #
  #   }else if(dbp != 2){
  #
  #     dbp <- dbp + 1
  #     data <- data[, c(1, dbp, (2:ncol(data))[-dbp+1])]
  #     #data[ , 2] <- as.numeric(data[ , 2])
  #
  #     }
  #   }
}




  # Pulse Pressure
  if(is.null(pp)){

    if(length(grep(paste("\\bPP\\b", sep = ""), names(data))) == 0){

      data$PP <- data$SBP - data$DBP

    }

    col_idx <- grep(paste("\\bPP\\b", sep = ""), names(data))
    data <- data[ , c(1:2, col_idx, (3:(ncol(data)))[-col_idx + 2])]

    data$PP <- as.numeric(data$PP)

  }else if(is.character(pp)){ # if character (i.e. by name)

    if(toupper(pp) %in% colnames(data) == FALSE){ # is pp argument found in data colnames

      stop('User-defined PP name does not match column name of supplied dataset')

    }else{

      col_idx <- grep(paste("\\b",toupper(pp),"\\b", sep = ""), names(data))
      data <- data[, c(1:2, col_idx, (3:ncol(data))[-col_idx+2])]

      data$PP <- as.numeric(data$PP)
      }
  } else {

      stop('User-defined PP name must be character.')
  }

  {
  # }else{ # if numeric
  #
  #   if(pp > ncol(data)){
  #
  #     stop('Invalid index for PP. Index greater than number of available columns')
  #   }
  #
  #   if(pp < 0){
  #
  #     stop('Invalid index for PP. Cannot have negative index')
  #   }
  #
  #   data <- data[, c(1:2, pp, (3:ncol(data))[-pp + 2])]
  #
  #   idx_list$hr + 1
  #   idx_list$map + 1
  #   idx_list$rpp + 1
  # }
}




  # Heart Rate
  if(is.null(hr)){

    if(length(grep(paste("\\bHR\\b", sep = ""), names(data))) == 1){

      warning('HR column found in data. \nIf this column corresponds to Heart Rate, use hr = "HR" in the function argument.')

    }

  } else if(is.character(hr)){

      if(toupper(hr) %in% colnames(data) == FALSE){

        stop('User-defined HR name does not match column name of supplied dataset')

      }else{

        col_idx <- grep(paste("\\b",toupper(hr),"\\b", sep = ""), names(data))
        data <- data[, c(1:3, col_idx, (4:ncol(data))[-col_idx+3])]
        data$HR <- as.numeric(data$HR)
      }
    } else {
      stop('User-defined HR name must be character.')
  }

    {
  #   }else{
  #
  #     if(hr > ncol(data)){
  #
  #       stop('Invalid index for HR. Index greater than number of available columns')
  #     }
  #
  #     if(hr < 0){
  #
  #       stop('Invalid index for HR. Cannot have negative index')
  #     }
  #
  #
  #     # Check this, not entirely sure if this logic is correct... will this work if multiple changes? tested with a few examples, but
  #     # not sure if my tests were thorough enough. I feel like I'm missing something here
  #     if(sbp != 1 | dbp != 2 | is.null(pp) | if(!is.null(pp)){pp != 3}){
  #       hr <- hr + 1
  #     }
  #     # same as above... check for more tests
  #     if(is.null(pp)){
  #       hr_inc <- 4
  #     }else{
  #       hr_inc <- 3
  #     }
  #
  #     data <- data[, c(1:3, hr, (4:ncol(data))[-hr + hr_inc])]
  #   }
  # }
}




  # Rate Pulse Product
  if(is.null(rpp)){

    # Try to find "RPP" column in data: if found, length is 1 (a number), if not found, length is 0 (i.e. logical(0) )
    if(length(grep(paste("\\bRPP\\b", sep = ""), names(data))) == 1){

      warning('"RPP" argument not specified in function, but "RPP" column found in data. \n Ensure that the "RPP" column in the data is not the desired column.')

    } else if( (length(grep(paste("\\bRPP\\b", sep = ""), names(data))) == 0) & (length(grep(paste("\\bHR\\b", sep = ""), names(data))) == 1)){

      data$RPP <- data$SBP * data$HR
      data$RPP <- as.numeric(data$RPP)

      hr_idx <- grep(paste("\\bHR\\b", sep = ""), names(data))
      rpp_idx <- grep(paste("\\bRPP\\b", sep = ""), names(data))
      data <- data[, c(1:hr_idx, rpp_idx, ((hr_idx+1):ncol(data))[-rpp_idx+hr_idx])]

      warning('No RPP column found. Automatically generated from SBP and HR columns.')
    }

  }else if( (toupper(rpp) %in% colnames(data)) == FALSE){

    stop('User-defined RPP name does not match column name of supplied dataset')

  }else if( (length(grep(paste("\\bHR\\b", sep = ""), names(data))) == 1) & (toupper(rpp) %in% colnames(data)) ){ # HR column is present and in position 4

    col_idx <- grep(paste("\\b",toupper(rpp),"\\b", sep = ""), names(data))
    data <- data[, c(1:4, col_idx, (5:ncol(data))[-col_idx+4])]

  }else if( (length(grep(paste("\\bHR\\b", sep = ""), names(data))) == 0) & (toupper(rpp) %in% colnames(data)) ){ # HR column is NOT present

    col_idx <- grep(paste("\\b",toupper(rpp),"\\b", sep = ""), names(data))
    data <- data[, c(1:3, col_idx, (4:ncol(data))[-col_idx+3])]

  }

    {
  #   }else{
  #
  #     if(rpp > ncol(data)){
  #
  #       stop('Invalid index for RPP. Index greater than number of available columns')
  #
  #       }
  #
  #     if(rpp < 0){
  #
  #       stop('Invalid index for RPP. Cannot have negative index')
  #
  #       }
  #
  #     # This needs to be fixed:
  #     if(rpp != ncol(data)){
  #
  #       rpp <- rpp + 2
  #       data <- data[, c(1:4, rpp, (5:ncol(data))[-(rpp)])]
  #
  #     }else{
  #
  #       data <- data[, c(1:4, rpp)]
  #
  #     }
  #   }

  # # Add RPP column if none exist
  # if(is.null(idx_list$rpp) & (!is.null(idx_list$sbp) & !is.null(idx_list$hr))){
  #   data$RPP <- data[, idx_list$sbp] * data[, idx_list$hr]
  #   idx_list$rpp <- ncol(data)
  #   message("No RPP column found. Automatically added using SBP and HR.")
  # }
    }




  # Mean Arterial Pressure
  if(is.null(map)){

    if(length(grep(paste("\\bMAP\\b", sep = ""), names(data))) == 1){

      warning('MAP column found in data. \nIf this column corresponds to Mean Arterial Pressure, use map = "MAP" in the function argument.')

    }
  } else if(toupper(map) %in% colnames(data) == FALSE){

      stop('User-defined MAP name does not match column name of supplied dataset')

  } else {

    col_idx <- grep(paste("\\b",toupper(map),"\\b", sep = ""), names(data))
    data <- data[, c(1:2, col_idx, (3:ncol(data))[-col_idx+2])]

  }





  # Wake (1: Awake | 2: Asleep)
  if(!is.null(wake)){

    if(toupper(wake) %in% colnames(data) == FALSE){

      stop('User-defined ID name does not match column name of supplied dataset')

    } else {

      col_idx <- grep(paste("\\b",toupper(wake),"\\b", sep = ""), names(data))
      data <- data[, c(col_idx, (1:ncol(data))[-col_idx])]

    }
  }






  # Visit
  if(!is.null(visit)){

    if(toupper(visit) %in% colnames(data) == FALSE){

      stop('User-defined VISIT name does not match column name of supplied dataset')

    } else {

      col_idx <- grep(paste("\\b",toupper(visit),"\\b", sep = ""), names(data))
      data <- data[, c(col_idx, (1:ncol(data))[-col_idx])]

    }
  }





  # Date & Time (DateTime object)
  if(!is.null(bp_datetime)){

      if(toupper(bp_datetime) %in% colnames(data) == FALSE){

      stop('User-defined bp_datetime name does not match column name of supplied dataset')

    } else {

      col_idx <- grep(paste("\\b",toupper(bp_datetime),"\\b", sep = ""), names(data))
      data <- data[, c(col_idx, (1:ncol(data))[-col_idx])]

    }
  }




  # ID
  if(!is.null(id)){

    if(toupper(id) %in% colnames(data) == FALSE){

      stop('User-defined ID name does not match column name of supplied dataset')

    } else {

      col_idx <- grep(paste("\\b",toupper(id),"\\b", sep = ""), names(data))
      data <- data[, c(col_idx, (1:ncol(data))[-col_idx])]

    }
  }



  return(data)
}



