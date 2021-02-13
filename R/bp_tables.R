#' Blood Pressure Tables
#'
#' @description Generate a list of pertinent table outputs that detail various information
#' specific to blood pressure.
#'
#' @param data A processed dataframe resulting from the \code{process_data} function that
#' contains the \code{SBP}, \code{DBP}, \code{DAY_OF_WEEK}, \code{Time_of_Day}, \code{SBP_CATEGORY},
#' and \code{DBP_CATEGORY} columns.
#'
#' @param bp_type An indicator of the type of blood pressure data to output based on either
#' 0 (both SBP and DBP), 1 (SBP only), or 2 (DBP only). Must be of type integer.
#'
#' @param bp_perc_margin An optional argument that determines which of the marginal totals
#' to include in the raw count tables expressed as percentages. The argument can take on
#' values either 0 (both SBP and DBP), 1 (SBP only), or 2 (DBP only).
#'
#' @param wake_perc_margin An optional argument that determines which of the marginal totals
#' to include in the tables pertaining to the percentages of awake / asleep readings if
#' applicable (i.e. if the WAKE column is present). The argument can take on values either
#' 0 (both SBP and DBP), 1 (SBP only), or 2 (DBP only).
#'
#' @param subj Optional argument. Allows the user to specify and subset specific subjects
#' from the \code{ID} column of the supplied data set. The \code{subj} argument can be a single
#' value or a vector of elements. The input type should be character, but the function will
#' comply with integers so long as they are all present in the \code{ID} column of the data.
#'
#'
#' @return A list of table outputs for various subsets of the data based on which bp_type is selected
#' (default is bp_type = 0 i.e. both SBP and DBP tables)
#'
#' @export
#'
#' @examples
#' data("bp_jhs")
#' data("hypnos_data")
#' hyp_proc <- process_data(hypnos_data,
#'                          sbp = "syst",
#'                          dbp = "DIAST",
#'                          bp_datetime = "date.time",
#'                          id = "id",
#'                          wake = "wake",
#'                          visit = "visit",
#'                          hr = "hr",
#'                          map = "map",
#'                          rpp = "rpp",
#'                          pp = "pp",
#'                          ToD_int = c(5, 13, 18, 23))
#'
#' jhs_proc <- process_data(bp_jhs,
#'                          sbp = "Sys.mmHg.",
#'                          dbp = "Dias.mmHg.",
#'                          bp_datetime = "DateTime",
#'                          hr = "pulse.bpm.")
#' rm(hypnos_data, bp_jhs)
#'
#' bp_tables(jhs_proc)
#' bp_tables(hyp_proc)
#'
bp_tables <- function(data, bp_type = 0, bp_perc_margin = NULL, wake_perc_margin = 2, subj = NULL){

  SBP_CATEGORY = DBP_CATEGORY = ID = NULL
  rm(list = c('SBP_CATEGORY', 'DBP_CATEGORY', 'ID'))

  if(!(bp_type %in% c(0, 1, 2)) ){
    stop('bp_type can only take on numeric values of either 0 (both SBP and DBP), 1 (SBP only), or 2 (DBP only).')
  }

  if( !is.null(bp_perc_margin) ){
    if( !(bp_perc_margin %in% c(1, 2)) ){
      stop('bp_perc_margin can only take on either NULL (overall (no marginal) percentages), 1 (row margin percentages), or 2 (column margin percentages).')
    }
  }

  if( !is.null(wake_perc_margin) ){
    if(!(wake_perc_margin %in% c(NULL, 1, 2)) ){
      stop('wake_perc_margin can only take on either NULL (overall (no marginal) percentages), 1 (row margin percentages), or 2 (column margin percentages).')
    }
  }


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


  # Create the following tables and report them as a list:
  # 1) Count of the number of recordings in each stage
  # 2) Contingency table of SBP vs DBP
  # 3) Contingency table of each bp type (SBP / DBP) and Weekday
  # 4) Contingency table of each bp type (SBP / DBP) and Time of Day

  # Number of Recordings in each stage and their respective percentages
  stages_SBP <- data %>% dplyr::count(SBP_CATEGORY) %>% dplyr::mutate(Perc = prop.table((data %>% dplyr::count(SBP_CATEGORY))$n), bp_type = "SBP")
  stages_DBP <- data %>% dplyr::count(DBP_CATEGORY) %>% dplyr::mutate(Perc = prop.table((data %>% dplyr::count(DBP_CATEGORY))$n), bp_type = "DBP")

  names(stages_SBP)[1] <- "Category"
  names(stages_DBP)[1] <- "Category"

  stages_SBP <- stages_SBP[,c(4,1,2,3)]
  stages_DBP <- stages_DBP[,c(4,1,2,3)]

  stages_all <- rbind(stages_SBP, stages_DBP) # may be able to comment this line out

  # Counts for each combination of SBP and DBP for each stage
  stages_combo <- data %>% dplyr::count(SBP_CATEGORY, DBP_CATEGORY)

  # Contingency table of SBP vs DBP (Counts)
  bp_count <- stats::xtabs(~ SBP_CATEGORY + DBP_CATEGORY, data = data)

  # Alternate: Contingency table of SBP vs DBP (Percentages)
  bp_perc <- prop.table(bp_count, margin = bp_perc_margin)

  bp_count <- as.data.frame.matrix( stats::addmargins( bp_count ) ) # Add marginal sums


  # Day of Week
  SBP_DoW <- as.data.frame.matrix( stats::addmargins( stats::xtabs(~ SBP_CATEGORY + DAY_OF_WEEK, data = data), margin = 2 ) )
  names(SBP_DoW)[ length(names(SBP_DoW)) ] <- "Total"

  DBP_DoW <- as.data.frame.matrix( stats::addmargins( stats::xtabs(~ DBP_CATEGORY + DAY_OF_WEEK, data = data), margin = 2 ) )
  names(DBP_DoW)[ length(names(DBP_DoW)) ] <- "Total"

  # Time of Day
  SBP_ToD <- as.data.frame.matrix( stats::addmargins( stats::xtabs(~ SBP_CATEGORY + TIME_OF_DAY, data = data), margin = 2 ) )
  DBP_ToD <- as.data.frame.matrix( stats::addmargins( stats::xtabs(~ DBP_CATEGORY + TIME_OF_DAY, data = data), margin = 2 ) )

  SBP_ToD <- SBP_ToD[,c(3,1,2,4,5)]
  DBP_ToD <- DBP_ToD[,c(3,1,2,4,5)]

  names(SBP_ToD)[ length(names(SBP_ToD)) ] <- "Total"
  names(DBP_ToD)[ length(names(DBP_ToD)) ] <- "Total"

  # Awake Status (if applicable)
  if("WAKE" %in% names(data)){

    SBP_wake <- stats::xtabs(~ SBP_CATEGORY + WAKE, data = data)
    DBP_wake <- stats::xtabs(~ DBP_CATEGORY + WAKE, data = data)

    SBP_wake_perc <- round(prop.table(SBP_wake, margin = wake_perc_margin),3)
    DBP_wake_perc <- round(prop.table(DBP_wake, margin = wake_perc_margin),3)

    SBP_wake <- as.data.frame.matrix( SBP_wake )
    DBP_wake <- as.data.frame.matrix( DBP_wake )

    # Consider for the future?
    # xtabs(~ SBP_CATEGORY + WAKE + TIME_OF_DAY, data = data)
    # xtabs(~ DBP_CATEGORY + WAKE + TIME_OF_DAY, data = data)
    #
    # xtabs(~ SBP_CATEGORY + WAKE + DAY_OF_WEEK, data = data)
    # xtabs(~ DBP_CATEGORY + WAKE + DAY_OF_WEEK, data = data)

  }else{

    SBP_wake <- "N/A - WAKE column not available"
    DBP_wake <- "N/A - WAKE column not available"
    SBP_wake_perc <- "N/A - WAKE column not available"
    DBP_wake_perc <- "N/A - WAKE column not available"
  }


  # Create list of tables to return
  if(bp_type == 0){

    # Both SBP and DBP tables
    bp_tables_list = list(

      'SBP_Counts_by_Stage'       = stages_SBP,
      'DBP_Counts_by_Stage'       = stages_DBP,
      'All_BP_Stage_Combinations' = stages_combo,
      'BP_contingency_count'      = bp_count,
      'BP_contingency_percent'    = bp_perc,
      'SBP_by_Day_of_Week'        = SBP_DoW,
      'DBP_by_Day_of_Week'        = DBP_DoW,
      'SBP_by_Time_of_Day'        = SBP_ToD,
      'DBP_by_Time_of_Day'        = DBP_ToD,
      'SBP_by_WAKE_status'        = SBP_wake,
      'DBP_by_WAKE_status'        = DBP_wake,
      'SBP_by_WAKE_perc'          = SBP_wake_perc,
      'DBP_by_WAKE_perc'          = DBP_wake_perc
    )

  }else if(bp_type == 1){

    # SBP tables only
    bp_tables_list = list(

      'SBP_Counts_by_Stage'       = stages_SBP,
      'All_BP_Stage_Combinations' = stages_combo,
      'BP_contingency_count'      = bp_count,
      'BP_contingency_percent'    = bp_perc,
      'SBP_by_Day_of_Week'        = SBP_DoW,
      'SBP_by_Time_of_Day'        = SBP_ToD,
      'SBP_by_WAKE_status'        = SBP_wake,
      'SBP_by_WAKE_perc'          = SBP_wake_perc

    )

  }else if(bp_type == 2){

    # DBP tables only
    bp_tables_list = list(

      'DBP_Counts_by_Stage'       = stages_DBP,
      'All_BP_Stage_Combinations' = stages_combo,
      'BP_contingency_count'      = bp_count,
      'BP_contingency_percent'    = bp_perc,
      'DBP_by_Day_of_Week'        = DBP_DoW,
      'DBP_by_Time_of_Day'        = DBP_ToD,
      'DBP_by_WAKE_status'        = DBP_wake,
      'DBP_by_WAKE_perc'          = DBP_wake_perc

    )

  }else{

    stop('Invalid bp_type function argument. Must either be 0 (both SBP & DBP - default), 1 (SBP only), or 2 (DBP only).')

  }

  return(bp_tables_list)

}
