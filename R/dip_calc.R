
#' Nocturnal Blood Pressure Dipping Calculation
#'
#' @description
#' Calculate the percent and average drop (or potentially reverse) in nocturnal blood pressure.
#' This function is typically used with ABPM data, or at minimum, data with a corresponding a
#' \code{WAKE} column available to indicate awake vs asleep.
#'
#' @param data
#' User-supplied data set that must contain \code{SBP}, \code{DBP}, and either \code{DATE_TIME} or \code{WAKE}
#' columns in order to distinguish between sleep and awake
#'
#' In the event of non-ABPM data (i.e. a data set without a corresponding \code{WAKE} column), then a
#' \code{DATE_TIME} column \strong{must} be present in order to denote which times correspond to sleep and which
#' times correspond to awake.
#'
#' @param sleep_int
#' (optional) User-supplied sleep interval to indicate start and end time of
#' the sleep interval of interest. Must only contain 2 values and must be 24-hour denoted integers
#'
#' Example: \code{sleep_int = c(22,5)} indicates a sleep period from 10pm - 5am.
#'
#' \strong{NOTE:} If the \code{sleep_int} function argument is specified, and no \code{WAKE} column
#' exists, then awake/asleep indicators will be assigned according the the \code{DATE_TIME}
#' column (which must exist). Otherwise, if \code{sleep_int} is not supplied, then the
#' WAKE column will default to a sleep period between 11PM - 6AM as specified in the
#' literature (see reference).
#'
#' Furthermore, the \code{sleep_int} argument
#' will override the \code{WAKE} column, which may cause unintended consequences in the event that the
#' data set already contains a \code{WAKE} column.
#'
#' @param dip_thresh
#' Default dipping threshold set to 0.10 (i.e. 10\%)
#'
#' @param inc_date
#' Default to FALSE. Indicates whether or not to include the date in the grouping of
#' the final output
#'
#' @references
#' Holt-Lunstad, J., Jones, B.Q., and Birmingham, W. (2009). The influence of close relationships
#' on nocturnal blood pressure dipping,
#' \emph{International Journal of Psychophysiology} \strong{71}, 211-217,
#' \doi{10.1016/j.ijpsycho.2008.09.008}.
#'
#'
#' @return A list containing 2 tibble objects. One for the grouped average values for SBP and DBP and
#' another for the dip % and classification. If inc_date = TRUE these two tibbles will be broken down
#' further by date
#'
#' @export
#'
#' @examples
#' ## Load hypnos_data
#' data(hypnos_data)
#'
#' ## Process hypnos_data
#' data <- process_data(hypnos_data,
#'                      sbp = 'syst',
#'                      dbp = 'diast',
#'                      bp_datetime = 'date.time',
#'                      hr = 'hr',
#'                      pp = 'PP',
#'                      map = 'MaP',
#'                      rpp = 'Rpp',
#'                      id = 'id',
#'                      visit = 'Visit',
#'                      wake = 'wake')
#'
#' dip_calc(data)
dip_calc <- function(data, sleep_int = NULL, dip_thresh = .10, inc_date = FALSE){



  # Features:
  #        X If Awake indicator column is specified in dataset, calculate percentages according to indicator.
  #           If not, default to Awake: 6am - 11pm | Asleep: 11pm - 6am |
  #        X Give user ability to choose time frame interval if no awake indicator column provided
  #        X User-defined dipping threshold, default to 10%
  #        - Screening criteria  for {SBP > 250 | SBP < 70} and {DBP < 45 | DBP > 150} and {HR < 40 | HR > 200} according to Holt-Lunstad, Jones, and Birmingham (2009) paper
  #        X Calculate the percent difference between two successive groups. In this case: Awake vs Asleep


  avg_SBP = SBP = DBP = . = NULL
  rm(list = c('avg_SBP','SBP', 'DBP', '.'))


  if(is.null(data$WAKE)){ # No Sleep / Awake indicator column provided (WAKE column)

    # Check to see if there is a user-defined sleep interval

    # No sleep_int present
    if(is.null(sleep_int)){

      # Check to see if DATE_TIME column is present

      # No WAKE, No sleep_int, No DATE_TIME --> error
      if(is.null(data$DATE_TIME)){

        stop('Unable to calculate BP dip. No DATE_TIME or WAKE columns present and no user-supplied sleep_int. Make sure to process all data using the process_data() function.')

      } else {

        # No WAKE, No sleep_int, Yes DATE_TIME --> WAKE = default interval of 11PM - 6AM
        sleep_int_seq = c(23, 0, 1, 2, 3, 4, 5, 6)
        awake_int_seq <- (seq(1:24)-1)[!(seq(1:24)-1) %in% sleep_int]

        data$WAKE <- ifelse(lubridate::hour(data$DATE_TIME) %in% awake_int_seq == TRUE, 1, 0)

      }

      # sleep_int present
    } else {

      # Check to see if DATE_TIME column is present

      # No WAKE, Yes sleep_int, No DATE_TIME --> error
      if(is.null(data$DATE_TIME)){

        stop('Unable to calculate BP dip. No DATE_TIME column present to match user-supplied sleep_int
                         and no WAKE column found in dataframe.')

      }
      # No WAKE, Yes sleep_int, Yes DATE_TIME --> WAKE = sleep_int
      else {

        # Compatibility checks for sleep_int

        if(!is.vector(sleep_int)){ # Not vector

          stop('sleep_int must be a vector specifying the sleep interval. For example: sleep_int = c(22,5) implies a sleep period from 22:00 to 05:00')

        }else if( any( !is.numeric(sleep_int) | is.na(sleep_int) ) == TRUE){ # Yes vector, No numeric

          stop('One or more of the elements of the supplied sleep_int vector are not numeric and/or NA')

        }

        # Coerce decimal values to integers
        if( all(sleep_int == floor(sleep_int)) == FALSE ){

          warning('Coerced floating point values within sleep_int vector to integer')
          sleep_int <- as.integer(sleep_int)

        }

        # Ensure that there are only two number specified in the vector
        if(length(sleep_int) != 2){
          stop('sleep_int must be a 2 x 1 vector with a start and end time. For example: sleep_int = c(22,5) implies a sleep period from 22:00 until 05:00')
        }

        # Ensure that repeated intervals such as sleep_int = c(2,2) don't occur
        if(sleep_int[1] == sleep_int[2]){

          stop('sleep_int time interval values cannot be equal')

        }

        # Obtain the proper sequence of numbers and account for numbers passing into the next day
        if(sleep_int[1] < sleep_int[2]){

          # Create sequence from starting time assuming first number is smaller than second
          sleep_int_seq <- seq( sleep_int[1], sleep_int[2] )

        }else if(sleep_int[1] > sleep_int[2]){

          # Create sequence when first number is larger than second (passes midnight)
          sleep_int_seq <- c( sleep_int[1] : 23, 0 : sleep_int[2])
        }

        # Complement of sleep interval sequence
        awake_int_seq <- (seq(1 : 24) - 1)[!(seq(1 : 24) - 1) %in% sleep_int_seq]

        if(length(awake_int_seq) + length(sleep_int_seq) != 24){
          stop('sleep and awake intervals do not add to 24')
        }

        data$WAKE <- ifelse(lubridate::hour(data$DATE_TIME) %in% awake_int_seq == TRUE, 1, 0)

      }

    }


    # WAKE column present:
  }else{

    # Check to see if there is a user-defined sleep interval

    # sleep_int present
    if(!is.null(sleep_int)){

      # Check to see if DATE_TIME column is present

      # Yes WAKE, Yes sleep_int, No DATE_TIME --> do nothing, throw warning
      if(is.null(data$DATE_TIME)){

        warning('No DATE_TIME column found to match user-defined sleep_int with.
                  Defaulting to WAKE column found in dataframe for calculation.')

      }
      # Yes WAKE, Yes sleep_int, Yes DATE_TIME
      else{

        # Override WAKE column by specifying times according to sleep_int
        # Throw warning

        warning('Disregarded WAKE column for BP dip calculation, instead used sleep_int specified by user.
                      If this is unintended, do not specify sleep_int in function argument.')


        # Compatibility checks for sleep_int

        if(!is.vector(sleep_int)){ # Not vector

          stop('sleep_int must be a vector specifying the sleep interval. For example: sleep_int = c(22,5)
                     implies a sleep period from 22:00 to 05:00')

        }else if( any( !is.numeric(sleep_int) | is.na(sleep_int) ) == TRUE){ # Yes vector, No numeric

          stop('One or more of the elements of the supplied sleep_int vector are not numeric and/or NA')

        }

        # Coerce decimal values to integers
        if( all(sleep_int == floor(sleep_int)) == FALSE ){

          warning('Coerced floating point values within sleep_int vector to integer')
          sleep_int <- as.integer(sleep_int)

        }

        # Ensure that there are only two number specified in the vector
        if(length(sleep_int) != 2){
          stop('sleep_int must be a 2 x 1 vector with a start and end time. For example: sleep_int = c(22,5)
                     implies a sleep period from 22:00 until 05:00')
        }

        # Ensure that repeated intervals such as sleep_int = c(2,2) don't occur
        if(sleep_int[1] == sleep_int[2]){

          stop('sleep_int time interval values cannot be equal')

        }

        # Obtain the proper sequence of numbers and account for numbers passing into the next day
        if(sleep_int[1] < sleep_int[2]){

          # Create sequence from starting time assuming first number is smaller than second
          sleep_int_seq <- seq( sleep_int[1], sleep_int[2] )

        }else if(sleep_int[1] > sleep_int[2]){

          # Create sequence when first number is larger than second (passes midnight)
          sleep_int_seq <- c( sleep_int[1] : 23, 0 : sleep_int[2])
        }

        # Complement of sleep interval sequence
        awake_int_seq <- (seq(1 : 24) - 1)[!(seq(1 : 24) - 1) %in% sleep_int_seq]

        if(length(awake_int_seq) + length(sleep_int_seq) != 24){
          stop('sleep and awake intervals do not add to 24')
        }

        data$WAKE <- ifelse(lubridate::hour(data$DATE_TIME) %in% awake_int_seq == TRUE, 1, 0)

      }

    }

  }




  # Determine how granular to calculate based on which columns are available
  if(inc_date == TRUE){
    grps = c("ID", "VISIT", "WAKE", "DATE")
  }else{
    grps = c("ID", "VISIT", "WAKE")
  }

  grps = grps[which(grps %in% colnames(data) == TRUE)]


  # Begin dipping calculation

  # helper function to be used in dip_pct
  pct = function(X_vec) {
    return(X_vec[1]/X_vec[2])
  }

  # Group data by whichever of the three above variables are present in the data
  dip <- data %>%
    dplyr::group_by_at(dplyr::vars(grps) ) %>%
    dplyr::summarise(avg_SBP = mean(SBP),
                     avg_DBP = mean(DBP),
                     N = dplyr::n() )

  dip_pct <- dip %>%
    dplyr::summarise( dip = -(1 - pct(avg_SBP)) ) %>%
    dplyr::mutate(classification = ifelse(dip <= -dip_thresh, "dipper",
                                   ifelse(dip > 0, "reverse", "non-dipper")) )

  return( list(dip, dip_pct) )


}



