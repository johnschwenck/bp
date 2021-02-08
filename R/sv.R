
#' Successive Variation (SV)
#'
#' Calculate the successive variation (SV) at various levels of granularity
#' based on what is supplied (ID, VISIT, WAKE, and / or DATE)for either SBP,
#' DBP, or both. SV is a measure of dispersion that takes into account the
#' temporal structure of the data and relies on the sum of squared differences
#' in successive observations, unlike the average real variability (ARV)
#' which relies on the sum of absolute differences.
#' $$SV = sqrt(sum(x_{i+1} - x_i)^2/n-1)$$
#'
#' \strong{NOTE:} The canonical standard deviation, independent of the temporal
#' structure using the sample average, is added for comparison:
#' $$SD = sqrt(sum(x_{i+1} - xbar)^2/n-1)$$
#'
#' @param data Required argument. Pre-processed dataframe with SBP and DBP columns
#' with optional ID, VISIT, WAKE, and DATE columns if available.
#' Use \code{process_data} to properly format data.
#' @param inc_date Optional argument. Default is FALSE. inc_date = TRUE will include
#' Date as a grouping level. For ABPM data this is typically omitted as sleep
#' is recorded over multiple days. This argument is useful for self-monitoring
#' as with the bp_jhs data
#' @param bp_type Optional argument. Determines whether to calculate ARV for SBP
#' values or DBP values. Default is 0 corresponding to output for both SBP & DBP.
#' For \strong{both} SBP and DBP ARV values use bp_type = 0, for \strong{SBP-only}
#' use bp_type = 1, and for \strong{DBP-only} use bp_type = 2
#'
#' @return A tibble object with a row corresponding to each subject, or alternatively
#' a row corresponding to each date if inc_date = TRUE. The resulting tibble consists of:
#' \itemize{
#'
#'    \item \code{ID}: The unique identifier of the subject. For single-subject datasets, ID = 1
#'    \item \code{VISIT}: (If applicable) Corresponds to the visit # of the subject, if more than 1
#'    \item \code{WAKE}: (If applicable) Corresponds to the awake status of the subject (0 = asleep |
#'    1 = awake)
#'    \item \code{SV_SBP} / \code{SV_DBP}: Calculates the square root of the average squared differences
#'    between successive measurements. The resulting value averages across the granularity
#'    grouping for however many observations are present.
#'    \item N: The number of observations for that particular grouping. If \code{inc_date = TRUE},
#'    \code{N} corresponds to the number of observations for that date. If \code{inc_date = FALSE}, N
#'    corresponds to the number of observations for the most granular grouping available (i.e.
#'    a combination of \code{ID}, \code{VISIT}, and \code{WAKE})
#'
#' }
#'
#' @export
#'
#'
#' @examples
#' # Load data
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
#' # SV Calculation
#' sv(hypnos_proc)
#' sv(jhs_proc)
sv <- function(data, inc_date = FALSE, bp_type = 0){

  SBP = DBP = . = NULL
  rm(list = c('SBP', 'DBP', '.'))

  if(bp_type == 0 | bp_type == 1){

    # SBP
    # check for missing values
    if(nrow(data) != length(which(stats::complete.cases(data$SBP) == TRUE))){
      warning('Missing SBP values found in data set. Removing for calculation')
      data <- data[which(!is.na(data$SBP)),]
    }
  }else if(bp_type == 0 | bp_type == 2){

    # DBP
    # check for missing values
    if(nrow(data) != length(which(stats::complete.cases(data$DBP) == TRUE))){
      warning('Missing DBP values found in data set. Removing for calculation')
      data <- data[which(!is.na(data$DBP)),]
    }
  }

  # Determine how granular to calculate based on which columns are available
  if(inc_date == TRUE){
    grps = c("ID", "VISIT", "WAKE", "DATE")
  }else{
    grps = c("ID", "VISIT", "WAKE")
  }

  grps = grps[which(grps %in% colnames(data) == TRUE)]

  if(length(grps) == 0){

    warning('No columns specified for ID, VISIT, or WAKE. All SV values aggregated.')

  }

  out <- data %>%
    dplyr::group_by_at( dplyr::vars(grps) ) %>%

    # SV Calculation
    { if (bp_type == 1) dplyr::summarise(., SV = sqrt( sum(( (SBP - dplyr::lag(SBP))[2:length(SBP - dplyr::lag(SBP))] )^2 ) / (dplyr::n() - 1)), N = dplyr::n()) else . } %>% # SBP only
    { if (bp_type == 2) dplyr::summarise(., SV = sqrt( sum(( (DBP - dplyr::lag(DBP))[2:length(DBP - dplyr::lag(DBP))] )^2 ) / (dplyr::n() - 1)), N = dplyr::n()) else . } %>% # DBP only
    { if (bp_type == 0) dplyr::summarise(., SV_SBP = sqrt( sum(( (SBP - dplyr::lag(SBP))[2:length(SBP - dplyr::lag(SBP))] )^2 ) / (dplyr::n() - 1)),
                                            SV_DBP = sqrt( sum(( (DBP - dplyr::lag(DBP))[2:length(DBP - dplyr::lag(DBP))] )^2 ) / (dplyr::n() - 1)),
                                            N = dplyr::n()) else . } # both SBP and DBP


  return(out)
}
