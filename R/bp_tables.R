#' Blood Pressure Tables
#'
#' @description Generate a list of pertinent table outputs that detail various information
#' specific to blood pressure.
#'
#' @param data A processed dataframe resulting from the \code{process_data} function that
#' contains the \code{SBP}, \code{DBP}, \code{DAY_OF_WEEK}, \code{Time_of_Day}, \code{SBP_CATEGORY},
#' and \code{DBP_CATEGORY} columns.
#'
#' @param bp_type Optional argument. Determines whether to calculate tables for SBP
#' values or DBP values, or both. For \strong{both SBP and DBP} ARV values use bp_type = 'both',
#' for \strong{SBP-only} use bp_type = 'sbp, and for \strong{DBP-only} use bp_type = 'dbp'.
#' If no type specified, default will be set to 'both'
#'
#' @param bp_perc_margin An optional argument that determines which of the marginal totals
#' to include in the raw count tables expressed as percentages. The argument can take on
#' values either NULL (default, both SBP and DBP), 1 (SBP only), or 2 (DBP only).
#'
#' @param wake_perc_margin An optional argument that determines which of the marginal totals
#' to include in the tables pertaining to the percentages of awake / asleep readings if
#' applicable (i.e. if the WAKE column is present). The argument can take on values either
#' NULL (both SBP and DBP), 1 (SBP only), or 2 (DBP only).
#'
#' @param subj Optional argument. Allows the user to specify and subset specific subjects
#' from the \code{ID} column of the supplied data set. The \code{subj} argument can be a single
#' value or a vector of elements. The input type should be character, but the function will
#' comply with integers so long as they are all present in the \code{ID} column of the data.
#'
#'
#' @return A list of table outputs for various subsets of the data based on which bp_type is selected
#'
#' @export
#'
#' @examples
#' data("bp_jhs")
#' data("bp_hypnos")
#' hyp_proc <- process_data(bp_hypnos,
#'                          bp_type = 'abpm',
#'                          sbp = "syst",
#'                          dbp = "DIAST",
#'                          date_time = "date.time",
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
#'                          date_time = "DateTime",
#'                          hr = "pulse.bpm.")
#' rm(bp_hypnos, bp_jhs)
#'
#' bp_tables(jhs_proc)
#' bp_tables(hyp_proc)
#'
bp_tables <- function(data, bp_type = c('both', 'sbp', 'dbp'), bp_perc_margin = NULL, wake_perc_margin = 2, subj = NULL){

  SBP_CATEGORY = DBP_CATEGORY = BP_CLASS = ID = Perc = NULL
  rm(list = c('SBP_CATEGORY', 'DBP_CATEGORY', 'BP_CLASS', 'ID', 'Perc'))

  # Check that bp_type, bp_perc_margin and wake_perc_margin have acceptable input
  ###############################################################################3

  # Match argument for bp_type
  bp_type <- tolower(bp_type)
  bp_type <- match.arg(bp_type)

  # Check bp_perc_margin argument
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


  # Check if user supplies a vector corresponding to a subset of multiple subjects (multi-subject only)
  ######################################################################################
  if(!is.null(subj)){

    # check to ensure that supplied subject vector is compatible
    subject_subset_check(data, subj)

    if(length(unique(data$ID)) > 1){
      # Filter data based on subset of subjects
      data <- data %>%
        dplyr::filter(ID %in% subj)
    }
  }


  # Create the following tables and report them as a list:
  # 1) Count of the number of recordings in each stage
  # 2) Contingency table of SBP vs DBP
  # 3) Contingency table of each bp type (SBP / DBP) and Weekday
  # 4) Contingency table of each bp type (SBP / DBP) and Time of Day

  # Number of Recordings in each BP stage and their respective percentages
  stages_SBP <- data %>% dplyr::count(SBP_CATEGORY) %>% dplyr::mutate(Perc = prop.table((data %>% dplyr::count(SBP_CATEGORY))$n), type = "SBP")
  stages_DBP <- data %>% dplyr::count(DBP_CATEGORY) %>% dplyr::mutate(Perc = prop.table((data %>% dplyr::count(DBP_CATEGORY))$n), type = "DBP")
  stages_CLASS <- data %>% dplyr::count(BP_CLASS) %>% dplyr::mutate(Perc = prop.table((data %>% dplyr::count(BP_CLASS))$n), type = "CLASS")

  # Rename the first column of each data frame to the same Category
  names(stages_SBP)[1] <- "Category"
  names(stages_DBP)[1] <- "Category"
  names(stages_CLASS)[1] <- "Category"

  # Reorder columns so that first is type (SBP, DBP or class), then Category, then n and then Perc
  stages_SBP <- stages_SBP[,c(4,1,2,3)]
  stages_DBP <- stages_DBP[,c(4,1,2,3)]
  stages_CLASS <- stages_CLASS[,c(4,1,2,3)]

  # Combine all three in one data frame row-wise
  stages_all <- rbind(stages_SBP, stages_DBP, stages_CLASS) # may be able to comment this line out

  # Counts for each combination of SBP and DBP for each stage
  stages_combo <- data %>% dplyr::count(SBP_CATEGORY, DBP_CATEGORY)

  # Contingency table of SBP vs DBP (Counts)
  bp_count <- stats::xtabs(~ SBP_CATEGORY + DBP_CATEGORY, data = data)

  # Alternate: Contingency table of SBP vs DBP (Percentages)
  bp_perc <- prop.table(bp_count, margin = bp_perc_margin)

  # Add marginal sums to count table
  bp_count <- as.data.frame.matrix( stats::addmargins( bp_count ) ) # Add marginal sums


  if( ("DAY_OF_WEEK" %in% names(data)) == TRUE ){

    # Day of Week
    SBP_DoW <- as.data.frame.matrix( stats::addmargins( stats::xtabs(~ SBP_CATEGORY + DAY_OF_WEEK, data = data), margin = 2 ) )
    names(SBP_DoW)[ length(names(SBP_DoW)) ] <- "Total"

    DBP_DoW <- as.data.frame.matrix( stats::addmargins( stats::xtabs(~ DBP_CATEGORY + DAY_OF_WEEK, data = data), margin = 2 ) )
    names(DBP_DoW)[ length(names(DBP_DoW)) ] <- "Total"

    CLASS_DoW <- as.data.frame.matrix( stats::addmargins( stats::xtabs(~ BP_CLASS + DAY_OF_WEEK, data = data), margin = 2 ) )
    names(CLASS_DoW)[ length(names(CLASS_DoW)) ] <- "Total"

  }else{

    SBP_DoW <-   "N/A - DAY_OF_WEEK column not available"
    DBP_DoW <-   "N/A - DAY_OF_WEEK column not available"
    CLASS_DoW <- "N/A - DAY_OF_WEEK column not available"
  }


  if( ("TIME_OF_DAY" %in% names(data)) == TRUE ){

    # Time of Day
    SBP_ToD <- as.data.frame.matrix( stats::addmargins( stats::xtabs(~ SBP_CATEGORY + TIME_OF_DAY, data = data), margin = 2 ) )
    DBP_ToD <- as.data.frame.matrix( stats::addmargins( stats::xtabs(~ DBP_CATEGORY + TIME_OF_DAY, data = data), margin = 2 ) )
    CLASS_ToD <- as.data.frame.matrix( stats::addmargins( stats::xtabs(~ BP_CLASS + TIME_OF_DAY, data = data), margin = 2 ) )

    SBP_ToD <- SBP_ToD[,c(3,1,2,4,5)]
    DBP_ToD <- DBP_ToD[,c(3,1,2,4,5)]
    CLASS_ToD <- CLASS_ToD[,c(3,1,2,4,5)]

    names(SBP_ToD)[ length(names(SBP_ToD)) ] <- "Total"
    names(DBP_ToD)[ length(names(DBP_ToD)) ] <- "Total"
    names(CLASS_ToD)[ length(names(CLASS_ToD)) ] <- "Total"


  }else{

    SBP_ToD <-   "N/A - TIME_OF_DAY column not available"
    DBP_ToD <-   "N/A - TIME_OF_DAY column not available"
    CLASS_ToD <- "N/A - TIME_OF_DAY column not available"
  }


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
  if(bp_type == 'both'){

    # Both SBP and DBP tables
    bp_tables_list = list(

      'SBP_Counts_by_Stage'       = stages_SBP,
      'DBP_Counts_by_Stage'       = stages_DBP,
      'CLASS_Counts'              = stages_CLASS,
      'All_BP_Stage_Combinations' = stages_combo,
      'BP_contingency_count'      = bp_count,
      'BP_contingency_percent'    = bp_perc,
      'SBP_by_Day_of_Week'        = SBP_DoW,
      'DBP_by_Day_of_Week'        = DBP_DoW,
      'CLASS_Day_of_Week'         = CLASS_DoW,
      'SBP_by_Time_of_Day'        = SBP_ToD,
      'DBP_by_Time_of_Day'        = DBP_ToD,
      'CLASS_Time_of_Day'         = CLASS_ToD,
      'SBP_by_WAKE_status'        = SBP_wake,
      'DBP_by_WAKE_status'        = DBP_wake,
      'SBP_by_WAKE_perc'          = SBP_wake_perc,
      'DBP_by_WAKE_perc'          = DBP_wake_perc
    )

  }else if(bp_type == 'sbp'){

    # SBP tables only
    bp_tables_list = list(

      'SBP_Counts_by_Stage'       = stages_SBP,
      'CLASS_Counts'              = stages_CLASS,
      'All_BP_Stage_Combinations' = stages_combo,
      'BP_contingency_count'      = bp_count,
      'BP_contingency_percent'    = bp_perc,
      'SBP_by_Day_of_Week'        = SBP_DoW,
      'CLASS_Day_of_Week'         = CLASS_DoW,
      'SBP_by_Time_of_Day'        = SBP_ToD,
      'CLASS_Time_of_Day'         = CLASS_ToD,
      'SBP_by_WAKE_status'        = SBP_wake,
      'SBP_by_WAKE_perc'          = SBP_wake_perc

    )

  }else if(bp_type == 'dbp'){

    # DBP tables only
    bp_tables_list = list(

      'DBP_Counts_by_Stage'       = stages_DBP,
      'CLASS_Counts'              = stages_CLASS,
      'All_SBP_DBP_Combinations' = stages_combo,
      'BP_contingency_count'      = bp_count,
      'BP_contingency_percent'    = bp_perc,
      'DBP_by_Day_of_Week'        = DBP_DoW,
      'CLASS_Day_of_Week'         = CLASS_DoW,
      'DBP_by_Time_of_Day'        = DBP_ToD,
      'CLASS_Time_of_Day'         = CLASS_ToD,
      'DBP_by_WAKE_status'        = DBP_wake,
      'DBP_by_WAKE_perc'          = DBP_wake_perc

    )

  }

  return(bp_tables_list)

}
