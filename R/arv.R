#' Average Real Variability (ARV)
#'
#' Calculate the Average Real Variability (ARV) for various levels of granularity
#' based on what is supplied (ID, VISIT, WAKE, and / or DATE)
#'
#' @param data Required argument. Pre-processed dataframe containing SBP and
#' DBP with optional ID, VISIT, WAKE, and DATE columns
#' @param inc_date Optional argument. Default is 0. inc_date = 1 will include
#' Date as a grouping level. For ABPM data this is typically omitted as sleep
#' is recorded over multiple days. This argument is useful for self-monitoring
#' as with the bp_jhs data
#' @param bp_type Optional argument. Determines whether to calculate ARV for SBP
#' values or DBP values. Default is 0 corresponding to output for both SBP & DBP.
#' For both SBP and DBP ARV values use bp_type = 0, for SBP-only use bp_type = 1,
#' and for DBP-only use bp_type = 2
#'
#' @return \code{arv} returns ....
#'
#' @import stats
#'
#' @examples
#' # Load data
#' data(hypnos_data)
#'
#' # Process data
#' data <- process_data(hypnos_data, sbp = "SYST", dbp = "DIAST", bp_datetime = "date.time",
#' id = "id", wake = "wake", visit = "visit", hr = "hr", pp ="pp", map = "map", rpp = "rpp")
#'
#' # ARV Calculation
#' arv(data)
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
    #filter(ID == 70417, VISIT == 1, WAKE == 0) %>% # subset for manual calculation test

    #ARV Calculation
    { if (bp_type == 1) dplyr::summarise(., ARV = sum( abs( (SBP - dplyr::lag(SBP))[2:length(SBP - dplyr::lag(SBP))] ) ) / (dplyr::n() - 1)) else . } %>% # SBP only
    { if (bp_type == 2) dplyr::summarise(., ARV = sum( abs( (DBP - dplyr::lag(DBP))[2:length(DBP - dplyr::lag(DBP))] ) ) / (dplyr::n() - 1)) else . } %>% # DBP only
    { if (bp_type == 0) dplyr::summarise(., ARV_SBP = sum( abs( (SBP - dplyr::lag(SBP))[2:length(SBP - dplyr::lag(SBP))] ) ) / (dplyr::n() - 1),
                                  ARV_DBP =  sum( abs( (DBP - dplyr::lag(DBP))[2:length(DBP - dplyr::lag(DBP))] ) ) / (dplyr::n() - 1)) else . } # both SBP and DBP


  return(out)
}




