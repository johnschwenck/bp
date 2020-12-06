#' Coefficient of Variation (CV)
#'
#' Calculate the coefficient of variation at various levels of granularity
#' based on what is supplied (ID, VISIT, WAKE, and / or DATE) for either SBP,
#' DBP, or both. CV is a measure of dispersion
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
#' @return explanation
#' @export
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
#' # CV Calculation
#' cv(data1)
#' cv(data2)
cv <- function(data, inc_date = FALSE, bp_type = 0){

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

    # CV Calculation
    { if (bp_type == 1) dplyr::summarise(., CV = sd(SBP, na.rm = TRUE) / mean(SBP, na.rm = TRUE) * 100, SD = sd(SBP),N = dplyr::n()) else . } %>% # SBP only
    { if (bp_type == 2) dplyr::summarise(., CV = sd(DBP, na.rm = TRUE) / mean(DBP, na.rm = TRUE) * 100, SD = sd(DBP),N = dplyr::n()) else . } %>% # DBP only
    { if (bp_type == 0) dplyr::summarise(., CV_SBP = sd(SBP, na.rm = TRUE) / mean(SBP, na.rm = TRUE) * 100,
                                            CV_DBP = sd(DBP, na.rm = TRUE) / mean(DBP, na.rm = TRUE) * 100,
                                            SD = sd(SBP),
                                            SD = sd(DBP),
                                            N = dplyr::n()) else . } # both SBP and DBP

  return(out)
}
