#' Average Real Variability (ARV)
#'
#' Calculate the Average Real Variability (ARV) at various levels of granularity
#' based on what is supplied (ID, VISIT, WAKE, and / or DATE). ARV is a measure
#' of dispersion that takes into account the temporal structure of the data and relies
#' on the sum of absolute differences in successive observations, unlike the
#' successive variation (SV) which relies on the sum of squared differences.
#' \deqn{SV = (\sum|x_{i+1} - x_i|/(n-1))^{1/2}}
#'
#' @param data Required argument. Pre-processed dataframe containing SBP and
#' DBP with optional ID, VISIT, WAKE, and DATE columns if available.
#' Use \code{process_data} to properly format data.
#' @param inc_date Optional argument. Default is FALSE. inc_date = TRUE will include
#' Date as a grouping level. For ABPM data this is typically omitted as sleep
#' is recorded over multiple days. This argument is useful for self-monitoring
#' as with the bp_jhs data
#' @param bp_type Optional argument. Determines whether to calculate ARV for SBP
#' values or DBP values. Default is 0 corresponding to output for both SBP & DBP.
#' For both SBP and DBP ARV values use bp_type = 0, for SBP-only use bp_type = 1,
#' and for DBP-only use bp_type = 2
#'
#' @return A tibble object with a row corresponding to each subject, or alternatively
#' a row corresponding to each date, if inc_date = TRUE
#'
#' @references
#' Mena et al. (2005) A reliable index for the prognostic significance of
#' blood pressure variability
#' \emph{Journal of Hypertension} \strong{23(5)}.505-11,
#' \doi{10.1097/01.hjh.0000160205.81652.5a}.
#'
#' @import stats
#'
#' @examples
#' # Load data
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
#' # ARV Calculation
#' arv(data1)
#' arv(data2)
#' @export
arv <- function(data, inc_date = 0, bp_type = 0){

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
  if(inc_date != 0){
    grps = c("ID", "VISIT", "WAKE", "DATE")
  }else{
    grps = c("ID", "VISIT", "WAKE")
  }

  grps = grps[which(grps %in% colnames(data) == TRUE)]

  if(length(grps) == 0){

    warning('No columns specified for ID, VISIT, or WAKE. All ARV values aggregated.')

  }

  # Group data by whichever of the three above variables are present in the data
  out <- data %>%
    dplyr::group_by_at(dplyr::vars(grps) ) %>%

    #ARV Calculation
    { if (bp_type == 1) dplyr::summarise(., ARV = sum( abs( (SBP - dplyr::lag(SBP))[2:length(SBP - dplyr::lag(SBP))] ) ) / (dplyr::n() - 1)) else . } %>% # SBP only
    { if (bp_type == 2) dplyr::summarise(., ARV = sum( abs( (DBP - dplyr::lag(DBP))[2:length(DBP - dplyr::lag(DBP))] ) ) / (dplyr::n() - 1)) else . } %>% # DBP only
    { if (bp_type == 0) dplyr::summarise(., ARV_SBP = sum( abs( (SBP - dplyr::lag(SBP))[2:length(SBP - dplyr::lag(SBP))] ) ) / (dplyr::n() - 1),
                                            ARV_DBP = sum( abs( (DBP - dplyr::lag(DBP))[2:length(DBP - dplyr::lag(DBP))] ) ) / (dplyr::n() - 1)) else . } # both SBP and DBP


  return(out)
}




