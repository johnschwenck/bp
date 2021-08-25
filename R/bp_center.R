#' Measures of Center (bp_center)
#'
#' Calculate the mean and median at various levels of granularity
#' based on what is supplied (ID, VISIT, WAKE, and / or DATE) for either SBP,
#' DBP, or both.
#'
#' @param data Required argument. Pre-processed dataframe with SBP and DBP columns
#' with optional ID, VISIT, WAKE, and DATE columns if available.
#' Use \code{process_data} to properly format data.
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
#' @param bp_type Optional argument. Determines whether to calculate ARV for SBP
#' values or DBP values. Default is 0 corresponding to output for both SBP & DBP.
#' For \strong{both} SBP and DBP ARV values use bp_type = 0, for \strong{SBP-only}
#' use bp_type = 1, and for \strong{DBP-only} use bp_type = 2
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
#' @return A tibble object with a row corresponding to each subject, or alternatively
#' a row corresponding to each date if inc_date = TRUE. The resulting tibble consists of:
#' \itemize{
#'
#'    \item \code{ID}: The unique identifier of the subject. For single-subject datasets, ID = 1
#'    \item \code{VISIT}: (If applicable) Corresponds to the visit # of the subject, if more than 1
#'    \item \code{WAKE}: (If applicable) Corresponds to the awake status of the subject (0 = asleep |
#'    1 = awake)
#'    \item \code{SBP_mean} / \code{DBP_mean}: Calculates the mean of systolic blood pressure readings
#'    for the specified time granularity.
#'    \item \code{SBP_med} / \code{DBP_med}: Calculates the median of systolic blood pressure readings
#'    for the specified time granularity.
#'    \item \code{N}: The number of observations for that particular grouping. If \code{inc_date = TRUE},
#'    \code{N} corresponds to the number of observations for that date. If \code{inc_date = FALSE}, N
#'    corresponds to the number of observations for the most granular grouping available (i.e.
#'    a combination of \code{ID}, \code{VISIT}, and \code{WAKE})
#'    \item Any add_groups variables supplied to function argument will be present as a column in the
#'    resulting tibble.
#'
#' }
#'
#' @export
#'
#' @examples
#' # Load data
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
#' # BP Center Calculation
#' bp_center(hypnos_proc, subj = c(70417, 70435))
bp_center <- function(data, inc_date = FALSE, subj = NULL, bp_type = 0, add_groups = NULL, inc_wake = TRUE){

  SBP = DBP = ID = grps = . = NULL
  rm(list = c('SBP', 'DBP', 'ID', 'grps', '.'))


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



  # Create groups for summarizing data with dplyr
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

    # CV Calculation
    { if (bp_type == 1) dplyr::summarise(., SBP_mean = mean(SBP, na.rm = TRUE), SBP_med = median(SBP, na.rm = TRUE), N = dplyr::n()) else . } %>% # SBP only
    { if (bp_type == 2) dplyr::summarise(., DBP_mean = mean(DBP, na.rm = TRUE), DBP_med = median(DBP, na.rm = TRUE), N = dplyr::n()) else . } %>% # DBP only
    { if (bp_type == 0) dplyr::summarise(., SBP_mean = mean(SBP, na.rm = TRUE),
                                            DBP_mean = mean(DBP, na.rm = TRUE),
                                            SBP_med = median(SBP, na.rm = TRUE),
                                            DBP_med = median(DBP, na.rm = TRUE),
                                            N = dplyr::n()) else . } # both SBP and DBP

  return(out)
}
