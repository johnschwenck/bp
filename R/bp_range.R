#' Blood Pressure Range
#'
#' Calculates the range (max - min) of both SBP and DBP values in addition to max and
#' min values for reference with the option to specify date as an additional level
#' of granularity
#'
#' @param data Required dataframe with SBP and DBP columns corresponding to
#' Systolic and Diastolic BP. This dataframe should come from \code{data_process}
#'
#' @param inc_date Optional argument. Default is FALSE. As ABPM data typically
#' overlaps due to falling asleep on one date and waking up on another, the \code{inc_date}
#' argument is typically kept as FALSE, but the function will work regardless. Setting
#' \code{inc_date = TRUE} will include these dates as a grouping level.
#'
#' @param subj Optional argument. Allows the user to specify and subset specific subjects
#' from the \code{ID} column of the supplied data set. The \code{subj} argument can be a single
#' value or a vector of elements. The input type should be character, but the function will
#' comply with integers so long as they are all present in the \code{ID} column of the data.
#'
#' @param add_groups Optional argument. Allows the user to aggregate the data by an
#' additional "group" to further refine the output. The supplied input must be a
#' character vector with the strings corresponding to existing column names of the
#' processed \code{data} input supplied. Capitalization of \code{add_groups} does not matter.
#' Ex: \code{add_groups = c("Time_of_Day")}
#'
#' @param inc_wake Optional argument corresponding to whether or not to include \code{WAKE}
#' in the grouping of the final output (if \code{WAKE} column is available). By default,
#' \code{inc_wake = TRUE} which will include the \code{WAKE} column in the groups by which
#' to calculate the respective metrics.
#'
#'
#' @return A tibble with SBP_max, SBP_min, SBP_range, DBP_max, DBP_min, DBP_range
#' and any additional optional columns included in data such as \code{ID}, \code{VISIT},
#' \code{WAKE}, and \code{DATE}. If inc_date = TRUE, each row will correspond to a date.
#' The resulting tibble consists of:
#' \itemize{
#'
#'    \item \code{ID}: The unique identifier of the subject. For single-subject datasets, ID = 1
#'    \item \code{VISIT}: (If applicable) Corresponds to the visit # of the subject, if more than 1
#'    \item \code{WAKE}: (If applicable) Corresponds to the awake status of the subject (0 = asleep |
#'    1 = awake)
#'    \item \code{SBP_max} / \code{DBP_max}: Finds the maximum value for the given grouping granularity
#'    \item \code{SBP_min} / \code{DBP_min}: Finds the minimum value for the given grouping granularity
#'    \item \code{SBP_range} / \code{DBP_range}: Calculates the range between the max and min values
#'    \item \code{N}: The number of observations for that particular grouping. If \code{inc_date = TRUE},
#'    \code{N} corresponds to the number of observations for that date. If \code{inc_date = FALSE},
#'    \code{N} corresponds to the number of observations for the most granular grouping available
#'    (i.e. a combination of \code{ID}, \code{VISIT}, and \code{WAKE})
#'    \item Any add_groups variables supplied to function argument will be present as a column in the
#'    resulting tibble.
#'
#' }
#'
#' @export
#'
#' @examples
#' # Load bp_hypnos
#' data(bp_hypnos)
#' data(bp_jhs)
#'
#' # Process bp_hypnos
#' hypnos_proc <- process_data(bp_hypnos, sbp = "SYST", dbp = "DIAST", date_time = "date.time",
#' id = "id", wake = "wake", visit = "visit", hr = "hr", pp ="pp", map = "map", rpp = "rpp")
#' # Process bp_jhs data
#' jhs_proc <- process_data(bp_jhs, sbp = "Sys.mmHg.", dbp = "Dias.mmHg.", date_time = "DateTime",
#' hr = "Pulse.bpm.")
#'
#' # Calculate BP range
#' bp_range(hypnos_proc)
#' bp_range(jhs_proc, inc_date = TRUE, add_groups = c("meal_time"))
#' # Notice that meal_time is not a column from process_data, but it still works
bp_range <- function(data, inc_date = FALSE, subj = NULL, add_groups = NULL, inc_wake = TRUE){

  SBP = DBP = ID = NULL
  rm(list = c('SBP', 'DBP', 'ID'))


  # If user supplies a vector corresponding to a subset of multiple subjects (multi-subject only)
  if(!is.null(subj)){

    # check to ensure that supplied subject vector is compatible
    subject_subset_check(data, subj)

    if(length(unique(data$ID)) > 1){

      # Filter data based on subset of subjects
      data <- data %>%
        dplyr::filter(ID %in% subj)

    }

  }


  # Verify that add_groups is valid and create grps variable for dplyr
  grps <- create_grps(data = data, inc_date = inc_date, add_groups = add_groups, inc_wake = inc_wake)

  if(length(grps) == 0){

    message('NOTE: No columns specified for ID, VISIT, or WAKE. All ARV values aggregated.')

  }

  # Avoid issues with capitalization by the user
  colnames(data) <- toupper( colnames(data) )
  grps <- toupper(grps)


  # Summary Output
  out <- data %>%
    dplyr::group_by_at( dplyr::vars(grps) ) %>%
    dplyr::add_count() %>%
    dplyr::summarise(SBP_max = max(SBP),
              SBP_min = min(SBP),
              DBP_max = max(DBP),
              DBP_min = min(DBP),
              SBP_range = ifelse(dplyr::n() == 1, NA, max(SBP) - min(SBP)),
              DBP_range = ifelse(dplyr::n() == 1, NA, max(DBP) - min(DBP)),
              N = dplyr::n())

  return(out)
}
