#' Blood Pressure Range
#'
#' Calculates the range (max - min) of both SBP and DBP values in addition to max and
#' min values for reference with the option to specify date as an additional level
#' of granularity
#'
#' @param data Required dataframe with SBP and DBP columns corresponding to
#' Systolic and Diastolic BP. This dataframe should come from \code{data_process}
#' @param inc_date Optional argument that specifies whether or not to break down
#' range by date. Default value is \code{inc_date = 0} indicating that date is
#' omitted from grouping. This is typically omitted for ABPM data as dates tend
#' to overlap due to recordings during sleep.
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
#'
#' }
#'
#' @export
#'
#' @examples
#' # Load hypnos_data
#' data(hypnos_data)
#' data(bp_jhs)
#'
#' # Process hypnos_data
#' hypnos_proc <- process_data(hypnos_data, sbp = "SYST", dbp = "DIAST", bp_datetime = "date.time",
#' id = "id", wake = "wake", visit = "visit", hr = "hr", pp ="pp", map = "map", rpp = "rpp")
#' # Process bp_jhs data
#' jhs_proc <- process_data(bp_jhs, sbp = "Sys.mmHg.", dbp = "Dias.mmHg.", bp_datetime = "DateTime",
#' hr = "Pulse.bpm.")
#'
#' # Calculate BP range
#' bp_range(hypnos_proc)
#' bp_range(jhs_proc, inc_date = 1)
bp_range <- function(data, inc_date = 0){

  SBP = DBP = NULL
  rm(list = c('SBP', 'DBP'))

  # Determine how granular to calculate based on which columns are available
  if(inc_date != 0){
    grps = c("ID", "VISIT", "WAKE", "DATE")
  }else{
    grps = c("ID", "VISIT", "WAKE")
  }

  grps = grps[which(grps %in% colnames(data) == TRUE)]

  if(length(grps) == 0){

    warning('No columns specified for ID, VISIT, or WAKE. All ARV values aggregated.')

  }

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
