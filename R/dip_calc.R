
#' Nocturnal Blood Pressure Dipping Calculation
#'
#' @description
#' Calculate the percent and average decline (or potentially reverse) in nocturnal blood pressure.
#' This function is typically used with ABPM data, or at minimum, data with a corresponding a
#' \code{WAKE} column available to indicate awake vs asleep.
#'
#' Nocturnal blood pressure decline is an indicator of an individual's natural circadian rhythm. Studies
#' have shown that individuals with diminished circadian rhythms are more likely to exhibit target organ damage.
#' There is a "U-shaped" relationship that exists among the magnitude of nocturnal blood pressure decline; the
#' extreme dippers and the non-dippers (including reverse dippers) are both more prone to mortality risk than normal dippers.
#'
#' @param data
#' User-supplied data set that must contain \code{SBP}, \code{DBP}, and either \code{DATE_TIME} or \code{WAKE}
#' columns in order to distinguish between sleep and awake
#'
#' In the event of non-ABPM data (i.e. a data set without a corresponding \code{WAKE} column), then a
#' \code{DATE_TIME} column \strong{must} be present in order to denote which times correspond to sleep and which
#' times correspond to awake.
#'
#' @param sleep_start_end
#' (optional) User-supplied manual override to adjust sleep interval indicating indicate start and end time
#' corresponding to the sleep interval of interest. Must only contain 2 values and must be 24-hour denoted integers
#'
#' Example: \code{sleep_start_end = c(22,5)} indicates a sleep period from 10pm - 5am.
#'
#' \strong{NOTE:} If the \code{sleep_start_end} function argument is specified, and no \code{WAKE} column
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
#' Default threshold for normal "Dipping" set to 0.10 (i.e. 10\%)
#'
#' @param extreme_thresh
#' Default threshold for "Extreme Dipping" set to 0.20 (i.e. 20\%) NOTE: dip_thresh cannot exceed extreme_thresh
#'
#' @param inc_date
#' Default to FALSE. Indicates whether or not to include the date in the grouping of
#' the final output
#'
#' @param subj Optional argument. Allows the user to specify and subset specific subjects
#' from the \code{ID} column of the supplied data set. The \code{subj} argument can be a single
#' value or a vector of elements. The input type should be character, but the function will
#' comply with integers so long as they are all present in the \code{ID} column of the data.
#'
#' @references
#' Okhubo, T., Imai, Y., Tsuji, K., Nagai, K., Watanabe, N., Minami, J., Kato, J., Kikuchi, N., Nishiyama, A.,
#' Aihara, A., Sekino, M., Satoh, H., and Hisamichi, S. (1997). Relation Between Nocturnal Decline in Blood
#' Pressure and Mortality: The Ohasama Study,
#' \emph{American Journal of Hypertension} \strong{10(11)}, 1201--1207,
#' \doi{10.1016/S0895-7061(97)00274-4}.
#'
#' @return A list containing 2 tibble objects. The first tibble object lists grouped average values for SBP and DBP
#' for awake and asleep periods. The second \code{dip_pct} tibble object lists the dipping percentage and
#' classification according to the results from the first \code{dip} tibble. If inc_date = TRUE these two
#' tibbles will be broken down further by date. There are 4 classifications a subject can have (assuming a
#' default dipping threshold of 10\% and extreme dipping threshold of 20\% according to the original source):
#'
#' \itemize{
#'    \item{Reverse Dipper - no nocturnal decline (greater or equal to 0\%)}
#'    \item{Non-Dipper - a nocturnal decline between 0 - 10\%}
#'    \item{Dipper - a nocturnal decline between 10\% and the extreme dipping \% (20\%)}
#'    \item{Extreme Dipper - a nocturnal decline exceeding 20\%}
#' }
#'
#' @export
#'
#' @examples
#' ## Load bp_hypnos
#' data(bp_hypnos)
#'
#' ## Process bp_hypnos
#' hypnos_proc <- process_data(bp_hypnos,
#'                      sbp = 'syst',
#'                      dbp = 'diast',
#'                      date_time = 'date.time',
#'                      hr = 'hr',
#'                      pp = 'PP',
#'                      map = 'MaP',
#'                      rpp = 'Rpp',
#'                      id = 'id',
#'                      visit = 'Visit',
#'                      wake = 'wake')
#'
#' dip_calc(hypnos_proc)
dip_calc <- function(data, sleep_start_end = NULL, dip_thresh = 0.10, extreme_thresh = 0.20, inc_date = FALSE, subj = NULL){


  # Features:
  #        X If Awake indicator column is specified in dataset, calculate percentages according to interval.
  #           If not, default to Awake: 6am - 11pm | Asleep: 11pm - 6am |
  #        X Give user ability to choose time frame interval if no awake indicator column provided
  #        X User-defined dipping threshold, default to 10%
  #        - (data_process) Screening criteria  for {SBP > 250 | SBP < 70} and {DBP < 45 | DBP > 150} and {HR < 40 | HR > 200} according to Holt-Lunstad, Jones, and Birmingham (2009) paper
  #        X Calculate the percent difference between two successive groups. In this case: Awake vs Asleep


  avg_SBP = avg_DBP = dip_sys = dip_dias = class_sys = class_dias = SBP = DBP = ID = . = NULL
  rm(list = c('avg_SBP','avg_DBP','dip_sys','dip_dias','class_sys','class_dias','SBP', 'DBP', 'ID', '.'))


  # If user supplies a vector corresponding to a subset of multiple subjects (multi-subject only)
  if(!is.null(subj)){

    # check to ensure that supplied subject vector is compatible
    subject_subset_check(data, subj)

    if(length(unique(data$ID)) > 1){

      # Filter data based on subset of subjects
      data <- data %>%
        dplyr::filter(ID == subj)

    }

  }


  # Check for user-supplied manual override of sleep start/end interval and adjust data accordingly
  data <- sleep_int(data = data, sleep_start_end = sleep_start_end)


  # Check compatibility of dip_thresh and extreme_thresh if not default (user-specified)
  if(dip_thresh != 0.10 | extreme_thresh != 0.20){
    if(dip_thresh == extreme_thresh){
      warning('Threshold for "Dipper" status and "Extreme Dipper" status are identical')
    }
    if(dip_thresh > extreme_thresh){
      stop('"dipping" threshold cannot exceed "extreme dipping" threshold')
    }
  }


  # Determine how granular to calculate based on which columns are available
  if(inc_date == TRUE){
    grps = c("ID", "VISIT", "DATE", "WAKE")
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
                     N = dplyr::n() ) %>%
    dplyr::arrange(dplyr::vars(grps))

  dip_pct <- dip %>%

    dplyr::summarise( dip_sys = -(1 - pct(avg_SBP)),
                      dip_dias = -(1 - pct(avg_DBP)) ) %>%

    dplyr::mutate(class_sys = ifelse(  dip_sys >= 0, "reverse",
                                       ifelse(  dip_sys > -dip_thresh, "non-dipper",
                                                ifelse( (dip_sys <= -dip_thresh) & (dip_sys >= -extreme_thresh), "dipper","extreme"))),
                  class_dias = ifelse(  dip_dias >= 0, "reverse",
                                        ifelse(  dip_dias > -dip_thresh, "non-dipper",
                                                 ifelse( (dip_dias <= -dip_thresh) & (dip_dias >= -extreme_thresh), "dipper","extreme")))) %>%

    dplyr::relocate(class_sys, .after = dip_sys)

  return( list(dip, dip_pct) )


}





