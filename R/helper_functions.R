#' Compatibility Check for User-Supplied Subject Subset Vector
#'
#' @param data Input data within original function
#' @param subj A vector corresponding to a selected subset of subjects to
#' refine the supplied data with
#'
#' @return Logical indicating whether the subj vector passes the checks (\code{TRUE}),
#' or not (\code{FALSE})
subject_subset_check <- function(data, subj = NULL){

  subj <- as.character(subj)
  data$ID <- as.character(data$ID)

  if(!is.vector(subj)){
    stop('subj argument must be a vector corresponding to the selected individuals to analyze')
  }

  if(typeof(subj) != typeof(data$ID)){
    stop('subj and ID column must be of the same type (vector)')
  }

  if(all(subj %in% data$ID) == FALSE){
    cat("ERROR: The following subj argument elements are not present in the ID column of the supplied dataset:\n", subj[which(!subj %in% data$ID)],"\n")
    stop('One or more of the supplied subject IDs are not present in the supplied data')
  }

  # Ensure that there are actually multiple subjects to subset
  if(length(unique(data$ID)) == 1){

    warning('Only one unique ID found in data set. Ignoring subj subset argument.')

  }

  return(TRUE)
}








#' Compatibility Checks for User-Supplied SBP/DBP Stages Vector
#'
#' Used in \code{process_data} function
#'
#' @param sbp_stages Checks whether the user supplied \code{sbp_stages_alt} function argument is valid or not
#' @param dbp_stages Checks whether the user supplied \code{dbp_stages_alt} function argument is valid or not
#'
#' @return A list containing two vectors that each correspond to the SBP/DBP stages for use in further processing.
#' If the function returns no error, the supplied vectors are valid. Function arguments are coerced to integer.
stage_check <- function(sbp_stages, dbp_stages){


  # Compatibility checks for SBP
  if(is.null(sbp_stages)){

    sbp_stages <- c(80,100,120,130,140,180,200)

  }else{

    sbp_stages <- as.integer(as.vector(sbp_stages))

    if(length(sbp_stages) != 7){
      stop('Invalid sbp_stages vector supplied. \nEnsure there are 7 values corresponding to the high / low thresholds of each stage.')
    }

    if(all(sbp_stages == cummax(sbp_stages)) == FALSE){
      stop('sbp_stages must be an increasing vector of integers.')
    }

    if(max(sbp_stages) > 300){
      warning('Highest systolic blood pressure threshold value exceeds 300. Ensure that this is intentional.')
    }

    if(min(sbp_stages) < 60){
      warning('Lowest systolic blood pressure threshold value is less than 60. Ensure that this is intentional.')
    }

  }


  # Compatibility checks for DBP
  if(is.null(dbp_stages)){

    dbp_stages <- c(25,60,80,85,90,120,140)

  }else{

    dbp_stages <- as.integer(as.vector(dbp_stages))

    if(length(dbp_stages) != 7){
      stop('Invalid dbp_stages vector supplied. \nEnsure there are 7 values corresponding to the low / high thresholds of each stage.')
    }

    if(all(dbp_stages == cummax(dbp_stages)) == FALSE){
      stop('dbp_stages vector must be monotonically increasing.')
    }

    if(max(dbp_stages) > 200){
      warning('Highest diastolic blood pressure threshold value exceeds 200. Ensure that this is intentional.')
    }

    if(min(dbp_stages) < 25){
      warning('Lowest diastolic blood pressure threshold value is less than 25. Ensure that this is intentional.')
    }

  }



  return(list(sbp_stages, dbp_stages))

}






#' Create Groups for Dplyr
#'
#' Used in the following functions:
#' \code{arv}, \code{bp_mag}, \code{bp_range}, \code{cv}, \code{sv}, \code{bp_center}, \code{bp_stats}
#'
#' @param data Supplied data from function
#' @param inc_date TRUE/FALSE indicator from function argument for whether or not to
#' include the date in grouping
#' @param add_groups Character vector from function argument input corresponding
#' to which other variables other than "ID", "WAKE", and "VISIT" to include from
#' the supplied data's column names. If "DATE" is supplied in add_groups, and
#' inc_date = TRUE, the duplicate will be omitted.
#'
#' @return A vector of string values corresponding to the column names that
#' will subset / group the data in dplyr functions
create_grps <- function(data, inc_date, add_groups){

  # Verify that add_groups is valid
  if(!is.null(add_groups)){

    if(!is.character(add_groups)){
      stop('add_groups must only contain character values corresponding to column names present in the supplied dataset.')
    }

    if( !all( toupper(add_groups) %in% toupper(colnames(data)) ) ){
      stop('add_groups argument not found in column names of supplied dataset.')
    }

  }


  # Determine how granular to calculate based on which columns are available
  if(inc_date == TRUE){

      if( !"DATE" %in% toupper(colnames(data)) ){
        warning('inc_date = TRUE but no DATE column found in supplied data set. Ignored inc_date argument.')
      }

    grps = c("ID", "VISIT", "WAKE", "DATE", add_groups)

  }else{

    grps = c("ID", "VISIT", "WAKE", add_groups)

    }

  # Remove any duplicates
  if( any(duplicated(grps)) ){
    grps <- grps[ - which(duplicated(grps) == TRUE) ]
  }

  # Subset based on which are available in data
  grps = grps[which(toupper(grps) %in% toupper(colnames(data)) == TRUE)]

  return(grps)
}






#' Compatibility Check for \code{path} Argument
#'
#' @param path A user-supplied string corresponding to the path location
#' where the export is to be saved
#'
#' @return A formatted string corresponding to the proper binding of
#' directory and base naming conventions. If user's path is invalid,
#' \code{path_check} will throw an error.
#'
#' @examples
#' \dontrun{
#' path = "~dir/abpm/"
#' path_check(path) # drops the trailing slash
#' }

path_check <- function(path){

  path <- path.expand(path)

  path <- file.path(dirname(path), basename(path))

  if( utils::file_test("-d", path) == FALSE){

    stop('Invalid path argument. Directory does not exist.')

  }

  if( !dir.exists(path) ){

    stop('Invalid path argument. Directory does not exist.')

  }

  return(path)

}








