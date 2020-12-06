#' Blood Pressure Range
#'
#' Calculates the range (max - min) of both SBP and DBP values with the option
#' to specify date as an additional level of granularity
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
#' \code{WAKE}, and \code{DATE}
#'
#' @export
#'
#' @examples
#' # Load hypnos_data
#' data(hypnos_data)
#' data(bp_jhs)
#'
#' # Process hypnos_data
#' data1 <- process_data(hypnos_data, sbp = "SYST", dbp = "DIAST", bp_datetime = "date.time",
#' id = "id", wake = "wake", visit = "visit", hr = "hr", pp ="pp", map = "map", rpp = "rpp")
#' # Process bp_jhs data
#' data2 <- process_data(bp_jhs, sbp = "Sys.mmHg.", dbp = "Dias.mmHg.", bp_datetime = "DateTime",
#' hr = "Pulse.bpm.")
#'
#' # Calculate BP range
#' bp_range(data1)
#' bp_range(data2, inc_date = 1)
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
              SBP_range = ifelse(dplyr::n() == 1, NA, max(SBP) - min(SBP)),
              DBP_max = max(DBP),
              DBP_min = min(DBP),
              DBP_range = ifelse(dplyr::n() == 1, NA, max(DBP) - min(DBP)),
              SD = sd(SBP),
              N = dplyr::n())

  return(out)
}
