data <- readxl::read_excel('C:\\Users\\John\\Documents\\CGM Research\\Data\\ABPM\\70417-1 ABPM.xlsx') # Delete
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
process_data <- function(data, override = 0, bp_date = NULL, sbp = NULL, dbp = NULL, hr = NULL, pp = NULL, map = NULL, rpp = NULL){

  #idx_list <- list(bp_date = bp_date, sbp = sbp, dbp = dbp, hr = hr, pp = pp, map = map, rpp = rpp)
  #idx <- as.numeric(unlist(idx_list))


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

    stop('Both "SBP" & "DBP" column indices must be specified.')

    }



  # Systolic BP (SBP)
  if(is.character(sbp)){

    if(toupper(sbp) %in% colnames(data) == FALSE){

      stop('User-defined SBP name does not match column name of supplied dataset')

    }else{

      col_idx <- grep(paste("\\b",toupper(sbp),"\\b", sep = ""), names(data))
      data <- data[, c(col_idx, (1:ncol(data))[-col_idx])]
      #data[ , 1] <- as.numeric(data[ , 1])
    }

  }else{

    if(sbp > ncol(data)){

      stop('Invalid index for SBP. Index greater than number of available columns')
    }

    if(sbp < 0){

      stop('Invalid index for SBP. Cannot have negative index')
    }

    if( (ncol(data) == 2) & (sbp == 2) ){

      data <- data[ , c(2 , 1)]

    }else if(sbp != 1){

      data <- data[, c(sbp, (1:ncol(data))[-sbp])]
      #data[ , sbp] <- as.numeric(data[ , sbp])

    }
  }




  # Diastolic BP (DBP)
  if(is.character(dbp)){

    if(toupper(dbp) %in% colnames(data) == FALSE){

      stop('User-defined DBP name does not match column name of supplied dataset')

    }else{

      col_idx <- grep(paste("\\b",toupper(dbp),"\\b", sep = ""), names(data))
      data <- data[, c(1, col_idx, (2:ncol(data))[-col_idx+1])]
      #data[ , 2] <- as.numeric(data[ , 2])
    }

  }else{

    if(dbp > ncol(data)){

      stop('Invalid index for DBP. Index greater than number of available columns')
    }

    if(dbp < 0){

      stop('Invalid index for DBP. Cannot have negative index')
    }

    if(sbp == 1){

      data <- data[, c(1, dbp, (2:ncol(data))[-dbp+1])]

    }else if(dbp != 2){

      dbp <- dbp + 1
      data <- data[, c(1, dbp, (2:ncol(data))[-dbp+1])]
      #data[ , 2] <- as.numeric(data[ , 2])

      }
    }




  # Pulse Pressure
  if(is.null(pp)){

    if("PP" %in% names(data) == F){

      data$PP <- data[ , 1] - data[ , 2]

    }

    col_idx <- grep(paste("\\bPP\\b", sep = ""), names(data))
    data <- data[ , c(1:2, col_idx, (3:(ncol(data)))[-col_idx + 2])]


  }else if(is.character(pp)){ # if character (i.e. by name)

    if(toupper(pp) %in% colnames(data) == FALSE){

      stop('User-defined PP name does not match column name of supplied dataset')

    }else{

      col_idx <- grep(paste("\\b",toupper(pp),"\\b", sep = ""), names(data))
      data <- data[, c(1:2, col_idx, (3:ncol(data))[-col_idx+2])]

      }

  }else{ # if numeric

    if(pp > ncol(data)){

      stop('Invalid index for PP. Index greater than number of available columns')
    }

    if(pp < 0){

      stop('Invalid index for PP. Cannot have negative index')
    }

    data <- data[, c(1:2, pp, (3:ncol(data))[-pp + 2])]

    idx_list$hr + 1
    idx_list$map + 1
    idx_list$rpp + 1
  }




  # Heart Rate
  if(!is.null(hr)){

    if(is.character(hr)){

      if(toupper(hr) %in% colnames(data) == FALSE){

        stop('User-defined HR name does not match column name of supplied dataset')

      }else{

        col_idx <- grep(paste("\\b",toupper(hr),"\\b", sep = ""), names(data))
        data <- data[, c(1:3, col_idx, (4:ncol(data))[-col_idx+3])]
      }
    }else{

      if(hr > ncol(data)){

        stop('Invalid index for HR. Index greater than number of available columns')
      }

      if(hr < 0){

        stop('Invalid index for HR. Cannot have negative index')
      }


      # Check this, not entirely sure if this logic is correct... will this work if multiple changes? tested with a few examples, but
      # not sure if my tests were thorough enough. I feel like I'm missing something here
      if(sbp != 1 | dbp != 2 | is.null(pp) | if(!is.null(pp)){pp != 3}){
        hr <- hr + 1
      }
      # same as above... check for more tests
      if(is.null(pp)){
        hr_inc <- 4
      }else{
        hr_inc <- 3
      }

      data <- data[, c(1:3, hr, (4:ncol(data))[-hr + hr_inc])]
    }
  }




  # # Rate Pulse Product
  # if(is.null(rpp) & !is.null(hr)){
  #
  #   data$RPP <- data[ , 1] * data[ , 4]
  #
  # }else if(is.character(rpp)){
  #
  #   if(toupper(rpp) %in% colnames(data) == FALSE){
  #
  #       stop('User-defined RPP name does not match column name of supplied dataset')
  #
  #   }else{
  #
  #     col_idx <- grep(paste("\\b",toupper(rpp),"\\b", sep = ""), names(data))
  #     data <- data[, c(1:2, col_idx, (3:ncol(data))[-col_idx+4])]
  #   }
  #
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

  return(data)
}










#
#   # Automatically detect columns unless explicitly specified by the user via override = 1
#   if(override == 0){
#
#     # Create copy for automatic detection back-up
#     idx_list_orig <- list(bp_date = bp_date, sbp = sbp, dbp = dbp, hr = hr, pp = pp, map = map, rpp = rpp)
#
#     if(any( names(data) %in% toupper(names(idx_list)) ) ){
#       warning('Matching column names automatically detected. To manually correct column indices, use override = 1')
#       idx_list[which(toupper(names(idx_list)) %in% names(data) == T)] <- match(toupper(names(idx_list)), names(data))[!is.na(match(toupper(names(idx_list)), names(data)) )]
#     }
#       # if column automatically detected, ensure that the column index doesn't already exist
#     if(any( table(unlist(idx_list)) > 1) ){
#       warning('Column index automatically detected, but duplicate of user-specified index. Reverting back to user-specified selection from function argument given by user.')
#       idx_list <- idx_list_orig
#     }
#   }
#
#   # Add RPP column if none exist
#   if(is.null(idx_list$rpp) & (!is.null(idx_list$sbp) & !is.null(idx_list$hr))){
#     data$RPP <- data[, idx_list$sbp] * data[, idx_list$hr]
#     idx_list$rpp <- ncol(data)
#     message("No RPP column found. Automatically added using SBP and HR.")
#   }
#
#   data <- data[, c(idx_list$bp_date,
#                    idx_list$sbp,
#                    idx_list$dbp,
#                    idx_list$hr,
#                    idx_list$pp,
#                    idx_list$map,
#                    idx_list$rpp)]
#
#   return(data)
# }


process_data(data, sbp = 3, dbp = 5) # HYPNOS
process_data(data2, sbp = 10, dbp = 12)










## Arrange variable positions of data set by user-specified index
### Code ideas borrowed from stackexchange question: https://stackoverflow.com/a/37009127/14189332
arrange_data <- function(data, sbp = NULL, dbp = NULL, hr = NULL, pp = NULL, map = NULL, rpp = NULL){#vars){

  vars <- c(sbp = sbp, dbp = dbp, hr = hr, pp = pp, map = map, rpp = rpp)

  if(is.null(vars)){
    sbp = 1
    vars <- c(sbp = sbp, dbp = dbp, hr = hr, pp = pp, map = map, rpp = rpp)
    warning('No variable index arguments specified in arrange_vars. Forced "SBP" as first column.')
  }

## data frame compatibility check was here

  ## Uppercase all names for consistency
  data_names <- toupper(names(data))
  var_names <- toupper(names(vars))
  var_rows <- length(data_names)

  # Ensure that indices are numeric
  if(is.character(vars)){
    vars <- as.numeric(vars)
    warning('Changed index values to numeric')
  }
  # Create vector of index position for convenience
  var_pos <- vars


  ### Miscellaneous Compatibility checks ###

  # Duplicated Column indices / values
  if(any(duplicated(vars))){
    stop('Duplicated column indices.')
  }

  # Make sure that all specified function arguments match columns in dataset
  if(all(var_names %in% data_names) == F){
    stop('Specified function arguments in arrange_vars do not match data set names. Make sure to only specify the argument variables that are present in your data. i.e. specifying that hr = 1 but there is not hr column in your data.')
  }

  if( any(var_pos < 0)){
    stop('Variable positions must be positive')
  }

  #
  if( all(vars <= var_rows) == F ){
    stop('Indices of specified function arguments cannot exceed the number of columns in the data set. i.e. You cannot specify hr = 5 if there are only 4 columns in the data.')
  }

  ### Indices for re-arranged output
  out_vec <- character(var_rows)
  out_vec[var_pos] <- var_names
  out_vec[-var_pos] <- data_names[ !(data_names %in% var_names) ]
  if( length(out_vec) != var_rows ){
    stop('Length of re-arranged vector differs from the number of variables in the original data')
  }

  ### Final re-arranged data
  data <- data[ , out_vec]

  return(data)
}



