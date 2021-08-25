#' Compatibility Check for User-Supplied Subject Subset Vector
#'
#' @param data Input data within original function
#' @param subj A vector corresponding to a selected subset of subjects to
#' refine the supplied data with
#'
#' @return Logical indicating whether the subj vector passes the checks (\code{TRUE}),
#' or not (\code{FALSE})
subject_subset_check <- function(data, subj = NULL){

  if(!"ID" %in% toupper(colnames(data))){
    stop('No ID column found. \nEnsure that process_data is run first to identify corresponding column.')
  }

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
stage_check <- function(sbp_stages = NULL, dbp_stages = NULL){


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
#'
#' @param inc_date TRUE/FALSE indicator from function argument for whether or not to
#' include the date in grouping
#'
#' @param add_groups Character vector from function argument input corresponding
#' to which other variables other than "ID", "WAKE", and "VISIT" to include from
#' the supplied data's column names. If "DATE" is supplied in add_groups, and
#' inc_date = TRUE, the duplicate will be omitted.
#'
#' @param inc_wake Optional argument corresponding to whether or not to include \code{WAKE}
#' in the grouping of the final output (if \code{WAKE} column is available). By default,
#' \code{inc_wake = TRUE} which will include the \code{WAKE} column in the groups by which
#' to calculate the respective metrics.
#'
#' @return A vector of string values corresponding to the column names that
#' will subset / group the data in dplyr functions
create_grps <- function(data, inc_date, add_groups, inc_wake){

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

    if(inc_wake == TRUE){
        grps = c("ID", "VISIT", "WAKE", "DATE", add_groups)
    }else{
        grps = c("ID", "VISIT", "DATE", add_groups)
    }


  }else{

    if(inc_wake == TRUE){
        grps = c("ID", "VISIT", "WAKE", add_groups)
    }else{
        grps = c("ID", "VISIT", add_groups)
    }


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

  if(!is.null(path)){

      path <- path.expand(path)

      path <- file.path(dirname(path), basename(path))

      if( utils::file_test("-d", path) == FALSE){

        stop('Invalid path argument. Directory does not exist.')

      }

      if( !dir.exists(path) ){

        stop('Invalid path argument. Directory does not exist.')

      }

  }else{

    path <- getwd()

  }

  return(path)

}








#' Sleep Interval Manual Override
#'
#' Adjusts WAKE column to reflect start and end of user-defined sleep period. If sleep_start_end
#' is NULL and WAKE column is included in input data, this function will not alter data.
#'
#' @param data
#' Supplied data from function
#'
#' @param sleep_start_end
#' User-supplied sleep interval to indicate start and end time of
#' the sleep interval of interest. Must only contain 2 values and must be 24-hour denoted integers
#'
#' @param adj Logical indicating whether or not to perform necessary adjustments / additions to data
#' for WAKE column (i.e. set the default sleep time from 11PM - 6 AM if no interval specified and
#' a DATE_TIME column is available). Default set to TRUE. adj == FALSE essentially acts as a pass statement
#'
#' @return
#' Dataframe with added / adjusted WAKE column moved after DATE_TIME column containing binary 1 (awake)
#' and 0 (asleep)
#'
sleep_int <- function(data, sleep_start_end = NULL, adj = TRUE){


  WAKE = DATE_TIME = . = NULL
  rm(list = c("WAKE", "DATE_TIME", "."))


  # sleep_start_end argument specified
    if(!is.null(sleep_start_end)){

        # Compatibility checks for sleep_start_end

        if(!is.vector(sleep_start_end)){ # Not vector

          stop('sleep_start_end must be a vector specifying the sleep interval. \nFor example: sleep_start_end = c(22,5) implies a sleep period from 22:00 to 05:00\n')

        }else if( any( !is.numeric(sleep_start_end) | is.na(sleep_start_end) ) == TRUE){ # Yes vector, No numeric

          stop('One or more of the elements of the supplied sleep_start_end vector are not numeric and/or NA\n')

        }

        # Coerce decimal values to integers
        if( all(sleep_start_end == floor(sleep_start_end)) == FALSE ){

          warning('Coerced floating point values within sleep_start_end vector to integer.\n')
          sleep_start_end <- as.integer(sleep_start_end)

        }

        # Ensure that there are only two number specified in the vector
        if(length(sleep_start_end) != 2){
          stop('sleep_start_end must be a 2 x 1 vector with a start and end time. \nFor example: sleep_start_end = c(22,5) implies a sleep period from 22:00 until 05:00\n')
        }

        # Ensure that repeated intervals such as sleep_start_end = c(2,2) don't occur
        if(sleep_start_end[1] == sleep_start_end[2]){

          stop('sleep_start_end time interval values cannot be equal.\n')

        }

        # Ensure that numbers in sleep_start_end interval are valid (i.e. between 0 and 24)
        if( any(sleep_start_end[1] > 23 | sleep_start_end[2] > 23 | sleep_start_end[1] < 0 | sleep_start_end[2] < 0)  ){

          stop('Invalid entries for sleep_start_end interval. Values must be between 0 and 23.\n')

        }


    }



  # Process WAKE column

  # No WAKE (Sleep / Awake) indicator column provided in input data
    if(is.null(data$WAKE)){

      # Check to see if there is a user-defined sleep interval

      # No sleep_start_end present
      if(is.null(sleep_start_end)){

        # Check to see if DATE_TIME column is present

        # No WAKE, No sleep_start_end, No DATE_TIME --> error
        if(is.null(data$DATE_TIME)){

          stop('Unable to calculate BP dip. No DATE_TIME or WAKE columns present and no user-supplied sleep_start_end.
               \nMake sure to process all data using the process_data() function.\n')

        } else {


          # if adj == FALSE indicates then no adjustments will be made to data (assuming sleep_start_end == NULL)
          # the argument adj == FALSE is essentially a pass statement

            if(adj == TRUE){

              # No WAKE, No sleep_start_end, Yes DATE_TIME --> WAKE = default interval of 11PM - 6AM
              sleep_int_seq = c(23, 0, 1, 2, 3, 4, 5, 6)
              awake_int_seq <- (seq(1:24)-1)[!(seq(1:24)-1) %in% sleep_int_seq]

              data$WAKE <- ifelse(lubridate::hour(data$DATE_TIME) %in% awake_int_seq == TRUE, 1, 0)

              data <- data %>% dplyr::relocate(WAKE, .after = DATE_TIME)

            }


        }

        # sleep_start_end present
      } else {

        # Check to see if DATE_TIME column is present

        # No WAKE, Yes sleep_start_end, No DATE_TIME --> error
        if(is.null(data$DATE_TIME)){

          stop('Unable to calculate BP dip. No DATE_TIME column present to match user-supplied sleep_start_end
                           and no WAKE column found in dataframe.\n')

        }
        # No WAKE, Yes sleep_start_end, Yes DATE_TIME --> WAKE = sleep_start_end
        else {

          # Add WAKE column if not already present
          data <- data %>%
            dplyr::mutate(WAKE = dplyr::case_when(

              # start of interval (fall asleep) at midnight
              sleep_start_end[1] == 0 ~ ifelse( lubridate::hour(data$DATE_TIME) <= sleep_start_end[2], 0, 1),

              # end of interval (wake up) at midnight
              sleep_start_end[2] == 23 ~ ifelse( lubridate::hour(data$DATE_TIME) >= sleep_start_end[1], 0, 1),

              # second number of sleep interval greater than first number ( i.e. c(2, 9) ) --> morning bed time
              sleep_start_end[1] < sleep_start_end[2] ~ ifelse( lubridate::hour(data$DATE_TIME) >= sleep_start_end[1] &
                                                                  lubridate::hour(data$DATE_TIME) <= sleep_start_end[2], 0, 1),

              # second number of sleep interval less than first number ( i.e. c(22, 3) )
              sleep_start_end[1] > sleep_start_end[2] ~ ifelse( lubridate::hour(data$DATE_TIME) <= sleep_start_end[2] |
                                                                  lubridate::hour(data$DATE_TIME) >= sleep_start_end[1], 0, 1),

              # if sleep and awake are same hour, assume they are awake (i.e 1)
              sleep_start_end[1] == sleep_start_end[2] ~ 1,

              # 9999 indicates error
              TRUE ~ 9999

            )) %>%
            dplyr::relocate(WAKE, .after = DATE_TIME)


        }

      }


  # WAKE column present:
    }else{

      # Check to see if there is a user-defined sleep interval

      # Yes WAKE, Yes sleep_start_end
      if(!is.null(sleep_start_end)){

        # Check to see if DATE_TIME column is present

        # Yes WAKE, Yes sleep_start_end, No DATE_TIME --> do nothing, throw warning
        if(is.null(data$DATE_TIME)){

          warning('No DATE_TIME column found to match user-defined sleep_start_end with.
                  \nKeeping WAKE column found in user-supplied dataframe.\n')

        }
        # Yes WAKE, Yes sleep_start_end, Yes DATE_TIME
        else{

          # Override WAKE column by specifying times according to sleep_start_end
          # Throw warning

          message('WAKE column found in user-supplied data. Sleep interval overwritten using sleep_start_end argument.
                  \nIf this is unintended, do not specify sleep_start_end in function argument.\n')


          # Override WAKE column using sleep_start_end interval from function argument
          data <- data %>%
            dplyr::mutate(WAKE = dplyr::case_when(

              # start of interval (fall asleep) at midnight
              sleep_start_end[1] == 0 ~ ifelse( lubridate::hour(data$DATE_TIME) <= sleep_start_end[2], 0, 1),

              # end of interval (wake up) at midnight
              sleep_start_end[2] == 23 ~ ifelse( lubridate::hour(data$DATE_TIME) >= sleep_start_end[1], 0, 1),

              # second number of sleep interval greater than first number ( i.e. c(2, 9) ) --> morning bed time
              sleep_start_end[1] < sleep_start_end[2] ~ ifelse( lubridate::hour(data$DATE_TIME) >= sleep_start_end[1] &
                                                                  lubridate::hour(data$DATE_TIME) <= sleep_start_end[2], 0, 1),

              # second number of sleep interval less than first number ( i.e. c(22, 3) )
              sleep_start_end[1] > sleep_start_end[2] ~ ifelse( lubridate::hour(data$DATE_TIME) <= sleep_start_end[2] |
                                                                  lubridate::hour(data$DATE_TIME) >= sleep_start_end[1], 0, 1),

              # if sleep and awake are same hour, assume they are awake (i.e 1)
              sleep_start_end[1] == sleep_start_end[2] ~ 1,

              # 9999 indicates error
              TRUE ~ 9999

            )) %>%
            dplyr::relocate(WAKE, .after = DATE_TIME)



        }

      }

    }

  return(data)

}










# helper function for bp_scatter visual for inserting low / crisis values into xlim / ylim breaks vector
# source: https://stackoverflow.com/a/18951302/14189332
# credit: Ferdinand.kraft
insert.at <- function(a, pos, ...){
  dots <- list(...)
  stopifnot(length(dots)==length(pos))
  result <- vector("list",2*length(pos)+1)
  result[c(TRUE,FALSE)] <- split(a, cumsum(seq_along(a) %in% (pos+1)))
  result[c(FALSE,TRUE)] <- dots
  unlist(result)
}





