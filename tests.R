arv <- function(data, sys = 1, dia = 2, hr = 3){
  if(data_process(data) == data){}
}


test_fun <- function(data){

  if(is.data.frame(data) == F){
    if(is.matrix(data) == F){
      stop('Invalid data type. Please use either data.frame or matrix')
    }else{
      warning('Converted matrix data type to data.frame')
      data <- as.data.frame(data)
    }
  }

  return(data)
}


testfun2 <- function(data, sbp = NULL, dbp = NULL, hr = NULL, pp = NULL, map = NULL, rpp = NULL){

  #out <- list(sbp = NULL, dbp = NULL, hr=NULL, pp=NULL, map=NULL, rpp=NULL)
  out <- formals(testfun2)
  # for(i in 1:length(out)){
    # if(!is.null(sbp)){
    #
    #   out[1] <- sbp
    # }
  # }

  # out$sbp <- sbp
  # out$dbp <- dbp
  # out$hr <- hr
  # out$map <- map
  # out$rpp <- rpp
  # out$pp <- pp

  # out <- c(0,0,0,0,0,0)
  # bp_args <- c(sbp, dbp, hr, pp, map, rpp)
  # if(is.null(sbp)){
  #   next
  # }else{
  #   out[1] <- sbp
  # }


  if(all(is.null(out))){
    stop('No columns selected. Must specify at least SBP and DBP column indices.')
  }

  result <- all(is.null(out))
  return(formals(testfun2))
}
testfun2(hr=1)


testfun3 <- function(sbp = NULL, dbp = NULL, hr = NULL, pp = NULL, map = NULL, rpp = NULL){
  data <- c(sbp, dbp, hr, pp, map, rpp)
  out <- list(sbp = sbp, dbp = dbp, hr = hr, pp = pp, map = map, rpp = rpp)
  return(out)
}
testfun3(hr = 1)

formals(testfun3)






















library(readxl);
data <- read_excel('C:\\Users\\John\\Downloads\\70417-1 ABPM.xlsx')
data2 <- read.csv('C:\\Users\\John\\Documents\\Final Data (Bike Trip)\\Blood Pressure\\Blood_Pressure.csv')
data2 <- data2[, c(1:15,31:34)]

test_1 <- data.frame(c(146,140,139,144,128), c(88,86,81,82,80))
names(test_1) <- c("SBP", "DBP")
test_1
"SBP" %in% names(test_1)
#test_1 <- as.matrix(test_1)
#test_1 <- as.list(test_1)
test_1$HR <- c(77,64,75,73,72)
#test_1$RPP <- test_1$SBP * test_1$HR
data <- test_1

vars <- c(sbp = NULL, dbp = "1", hr = NULL, pp = NULL, map = 2, rpp = NULL)

arrange_vars(data, vars)



















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

#
# process_data(data, sbp = 3, dbp = 5) # HYPNOS
# process_data(data2, sbp = 10, dbp = 12)










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

