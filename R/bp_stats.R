#' Aggregated BP Summary Statistics
#'
#' @description
#' Combines the output from the following functions:
#' \itemize{
#'
#'    \item \code{bp_center}
#'    \item \code{bp_cv}
#'    \item \code{bp_arv}
#'    \item \code{bp_sv}
#'    \item \code{bp_mag}
#'    \item \code{bp_range}
#'
#' }
#'
#' @param data Required argument. Pre-processed dataframe containing SBP and
#' DBP with optional ID, VISIT, WAKE, and DATE columns if available.
#' Use \code{process_data} to properly format data.
#'
#' @param inc_date Optional argument. Default is FALSE. As ABPM data typically
#' overlaps due to falling asleep on one date and waking up on another, the \code{inc_date}
#' argument is typically kept as FALSE, but the function will work regardless. Setting
#' \code{inc_date = TRUE} will include these dates as a grouping level.
#'
#' @param bp_type Optional argument. Determines whether to calculate ARV for SBP
#' values or DBP values. Default is 0 corresponding to output for both SBP & DBP.
#' For both SBP and DBP ARV values use bp_type = 0, for SBP-only use bp_type = 1,
#' and for DBP-only use bp_type = 2
#'
#' @param add_groups Optional argument. Allows the user to aggregate the data by an
#' additional "group" to further refine the output. The supplied input must be a
#' character vector with the strings corresponding to existing column names of the
#' processed \code{data} input supplied. Capitalization of \code{add_groups} does not matter.
#' Ex: \code{add_groups = c("Time_of_Day")}
#'
#' @param subj Optional argument. Allows the user to specify and subset specific subjects
#' from the \code{ID} column of the supplied data set. The \code{subj} argument can be a single
#' value or a vector of elements. The input type should be character, but the function will
#' comply with integers so long as they are all present in the \code{ID} column of the data.
#'
#' @param inc_wake Optional argument corresponding to whether or not to include \code{WAKE}
#' in the grouping of the final output (if \code{WAKE} column is available). By default,
#' \code{inc_wake = TRUE} which will include the \code{WAKE} column in the groups by which
#' to calculate the respective metrics.
#'
#'
#' @return A tibble object with a row corresponding to each subject, or alternatively
#' a row corresponding to each date, if inc_date = TRUE. The resulting tibble consists of:
#' \itemize{
#'
#'    \item \code{ID}: The unique identifier of the subject. For single-subject datasets, ID = 1
#'    \item \code{N}: The number of observations for that particular grouping. If \code{inc_date = TRUE},
#'    \code{N} corresponds to the number of observations for that date. If \code{inc_date = FALSE}, N
#'    corresponds to the number of observations for the most granular grouping available (i.e.
#'    a combination of \code{ID}, \code{VISIT}, and \code{WAKE})
#'    \item \code{VISIT}: (If applicable) Corresponds to the visit # of the subject, if more than 1
#'    \item \code{WAKE}: (If applicable) Corresponds to the awake status of the subject (0 = asleep |
#'    1 = awake)
#'    \item Output from the following functions: \code{bp_center}, \code{bp_cv}, \code{bp_arv}, \code{bp_sv},
#'    \code{bp_mag}, \code{bp_range}
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
#' # BP Stats Aggregated Table
#' bp_stats(hypnos_proc, subj = c(70417, 70435), add_groups = c("SBP_Category"))
#' bp_stats(jhs_proc, add_groups = c("SBP_Category"))
bp_stats <- function(data,
                     inc_date = FALSE,
                     subj = NULL,
                     bp_type = 0,
                     add_groups = NULL,
                     inc_wake = TRUE){

  ID = N = DATE = SBP_mean = DBP_mean = SBP_med = DBP_med = SD = SD_SBP = SD_DBP = CV_SBP = CV_DBP = SBP_max = DBP_max = SBP_min = DBP_min = SBP_range = DBP_range = . = NULL
  rm(list = c(ID, N, DATE, SBP_mean, DBP_mean, SBP_med, DBP_med, SD, SD_SBP, SD_DBP, CV_SBP, CV_DBP, SBP_range, DBP_range, SBP_max, SBP_min, DBP_max, DBP_min, .))

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


  # Verify that add_groups is valid
  if(!is.null(add_groups)){

    if(!is.character(add_groups)){
      stop('add_groups must only contain character values corresponding to column names present in the supplied dataset.')
    }

    if( !all( toupper(add_groups) %in% toupper(colnames(data)) ) ){
      stop('add_groups argument not found in column names of supplied dataset.')
    }

  }


  # Pull in all necessary data from functions --> functions do not need subj argument because data is already filtered above
  bp_center_tmp <- bp_center(data, inc_date = inc_date, bp_type = bp_type, add_groups = add_groups, inc_wake = inc_wake)
  arv_tmp       <- bp_arv(data, inc_date = inc_date, bp_type = bp_type, add_groups = add_groups, inc_wake = inc_wake)
  sv_tmp        <- bp_sv(data, inc_date = inc_date, bp_type = bp_type, add_groups = add_groups, inc_wake = inc_wake)
  cv_tmp        <- bp_cv(data, inc_date = inc_date, bp_type = bp_type, add_groups = add_groups, inc_wake = inc_wake)
  bp_range_tmp  <- bp_range(data, inc_date = inc_date, add_groups = add_groups, inc_wake = inc_wake)
  bp_mag_tmp    <- bp_mag(data, inc_date = inc_date, bp_type = bp_type, add_groups = add_groups, inc_wake = inc_wake)

  # Find all common column names to join by
  grps <- intersect(colnames(bp_center_tmp), colnames(arv_tmp))
  grps <- intersect(grps, colnames(sv_tmp))
  grps <- intersect(grps, colnames(cv_tmp))
  grps <- intersect(grps, colnames(bp_range_tmp))
  grps <- intersect(grps, colnames(bp_mag_tmp))

  if(inc_date == TRUE){

    # Ensure that there is a DATE column in supplied data set
    if( !"DATE" %in% toupper(colnames(data)) ){
      warning('inc_date = TRUE but no DATE column found in supplied data set. Ignored inc_date argument.')

    }else{

      grps <- c(grps, "DATE")

    }
  }


  # Remove any duplicates
  if( any(duplicated(grps)) ){
    grps <- grps[ - which(duplicated(grps) == TRUE) ]
  }


  # Summary Output
  out <- dplyr::left_join(bp_center_tmp, arv_tmp, by = c(grps))
  out <- dplyr::left_join(out, sv_tmp, by = c(grps))
  out <- dplyr::left_join(out, cv_tmp, by = c(grps))
  out <- dplyr::left_join(out, bp_range_tmp, by = c(grps))
  out <- dplyr::left_join(out, bp_mag_tmp, by = c(grps))





  #
  #
  # bp_type <- match.arg(bp_type)
  # switch(bp_type,
  #
  #        # Reorder columns - both
  #        both <- out %>% dplyr::relocate(ID, N, DATE) %>%
  #          dplyr::relocate(SD_SBP, .after = DBP_med) %>%
  #          dplyr::relocate(SD_DBP, .after = SD_SBP) %>% dplyr::relocate(SBP_range, .before = DBP_range),
  #
  #        sys <- out %>% dplyr::relocate(ID, N, DATE) %>%
  #          dplyr::relocate(SD, .after = SBP_med) %>%
  #          dplyr::select(-DBP_max, -DBP_min, -DBP_range),
  #
  #        dia <- out %>% dplyr::relocate(ID, N, DATE) %>%
  #          dplyr::relocate(SD, .after = DBP_med) %>%
  #          dplyr::select(-SBP_max, -SBP_min, -SBP_range)
  #
  # )

  # Reorder columns
  out <- out %>% dplyr::relocate(ID, N, DATE) %>%
    {if (bp_type == 0) dplyr::relocate(., SD_SBP, .after = DBP_med) %>%
                       dplyr::relocate(., SD_DBP, .after = SD_SBP) %>%
                       dplyr::relocate(., SBP_range, .before = DBP_range) else . } %>%
    {if (bp_type == 1) dplyr::relocate(., SD, .after = SBP_med) %>% dplyr::select(., -DBP_max, -DBP_min, -DBP_range) else . } %>%
    {if (bp_type == 2) dplyr::relocate(., SD, .after = DBP_med) %>% dplyr::select(., -SBP_max, -SBP_min, -SBP_range) else . }







#
#   # Reorder columns
#   out <- out %>% dplyr::relocate(ID, N, DATE)
#   out <- out %>% dplyr::relocate(SD_SBP, .after = DBP_med)
#   out <- out %>% dplyr::relocate(SD_DBP, .after = SD_SBP)
#   out <- out %>% dplyr::relocate(SBP_range, .before = DBP_range)

  return(out)
}

