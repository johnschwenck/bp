sleep_stages <- function(data, grps){

      # THE GOAL OF THIS FUNCTION IS TO CREATE INDICATORS FOR EACH OF THE 4 ABPM PERIODS FROM
      # KARIO ET AL (2003):
      #   - EVENING BP: AVG OF ALL BP READINGS WITHIN THE FIRST TWO HOURS PRECEDING FALLING ASLEEP
      #   - LOWEST BP: AVG OF 3 READINGS CENTERED AROUND THE MINIMUM DURING SLEEP (1 BEFORE LOWEST, LOWEST, 1 AFTER LOWEST)
      #   - PREWAKE BP: AVG OF ALL BP READINGS WITHIN TWO HOURS IMMEDIATELY PRECEDING WAKING UP
      #   - MORNING BP: AVG OF ALL BP READINGS WITHIN THE FIRST TWO HOURS UPON WAKING UP


  # Initialize variables for dplyr
  date_grp = idx = roll_idx = time_diff = roll_tmp = time_rev = wake_ind = time_elapsed = NULL
  rm(list = c("date_grp", "idx", "roll_idx", "time_diff", "roll_tmp", "time_rev", "wake_ind", "time_elapsed"))


    # Couldn't get dplyr arrange to work with grps due to various functions deprecated, etc
    # Using base R to order data frame based on what is supplied in grps and then by DATE_TIME
    # 1) Order data by smallest to largest ID (factor) then VISIT (factor)
    # 2) Order DATE_TIME in reverse chronological order for each of the above groups (earliest first within group)
    data <- data[do.call(order, data[grps]), ]
    #
    #   Old Code
    #     # Sort data according to ID then VISIT then DATE_TIME if ID, VISIT are supplied
    #     if(auto_sort == TRUE){
    #
    #           data <- data %>%
    #             # Sort by whichever variables are present in grps vector
    #             dplyr::arrange( across(c( grps, "DATE_TIME")) )
    #
    #     }


    # Reset row names
    row.names(data) <- NULL


    tmp  <- data %>%

        # Identify unusally large gaps in time
        dplyr::mutate( date_grp = dplyr::case_when(

          # First row will always be in first group
          dplyr::row_number() == 1 ~ 1,

          # Throw flag if next value is greater than 24 hours ahead or behind
          TRUE ~ ifelse( (lubridate::ymd_hms(DATE_TIME) > dplyr::lag(lubridate::ymd_hms(DATE_TIME)) + lubridate::dhours(24) ) |
                           (lubridate::ymd_hms(DATE_TIME) < dplyr::lag(lubridate::ymd_hms(DATE_TIME)) - lubridate::dhours(24) ), 1, 0)

        )
        ) %>%

        # Create cumulative count of the groups to associate each row to a particular group
        dplyr::mutate( date_grp = cumsum(date_grp) ) %>%

        # Use new smoothed grouping based on date
        dplyr::group_by(date_grp) %>%

        # Process & Smooth WAKE indicator column into new column --> wake_ind
        dplyr::mutate( wake_ind = dplyr::case_when(

          # First value in group takes value from WAKE always
          dplyr::row_number() == 1 ~ as.integer(as.character( WAKE[dplyr::row_number() == 1] )),

          # Last number in group takes value from WAKE always
          dplyr::row_number() == dplyr::n() ~ as.integer(as.character( WAKE[dplyr::row_number() == dplyr::n()] )),

          # Smooth Artifacts (i.e. 1 1 0 1 1 in succession) --> within a small time window --> verify method
          dplyr::lag(WAKE) == 1 & WAKE == 0 & dplyr::lead(WAKE) == 1 &
            lubridate::as.duration( dplyr::lag(DATE_TIME, n = 0) - dplyr::lag(DATE_TIME, n = 1) ) < 2 *
            lubridate::as.duration( dplyr::lag(DATE_TIME, n = 1) - dplyr::lag(DATE_TIME, n = 2) ) ~ as.integer(1),

          dplyr::lag(WAKE) == 0 & WAKE == 1 & dplyr::lead(WAKE) == 0 &
            lubridate::as.duration( dplyr::lag(DATE_TIME, n = 0) - dplyr::lag(DATE_TIME, n = 1) ) < 2 *
            lubridate::as.duration( dplyr::lag(DATE_TIME, n = 1) - dplyr::lag(DATE_TIME, n = 2) ) ~ as.integer(0),

          TRUE ~ as.integer(as.character( WAKE ))

        )) %>%

        dplyr::mutate( wake_ind = dplyr::case_when(

          # First Sleep instance
          dplyr::lag(wake_ind) == 1 & wake_ind == 0 & dplyr::lead(wake_ind) == 0  ~ as.integer(022), # originally 0

          # First Wake instance
          dplyr::lag(wake_ind) == 0 & wake_ind == 1 & dplyr::lead(wake_ind) == 1 ~ as.integer(11), # originally 1

          # All else
          TRUE ~ as.integer(wake_ind)

          # At this point there are just 4 possible values that wake_ind can take:
          # 11 - which is the first instance of being awake
          #  1 - which is a regular indicator for awake
          # 22 - which is the first instance of falling asleep
          #  0 - which is a regular indicator for asleep

        )) %>%

        # Since wake_ind does not distinguish between groups, idx is created to differentiate them
        dplyr::mutate(idx = dplyr::case_when(

          # First in the group
          dplyr::row_number() == 1 ~ as.integer(wake_ind),

          # Last in the group
          dplyr::row_number() == dplyr::n() ~ as.integer(10),

          # First sleep instance (22) and the next value is asleep --> re-code from 22 to 2
          wake_ind == 22 & dplyr::lead(wake_ind) == 0 ~ as.integer(2),

          # First awake instance (11) and the next value is awake --> re-code from 11 to 3
          wake_ind == 11 & dplyr::lead(wake_ind) == 1 ~ as.integer(3),

          # All else
          TRUE ~ as.integer(10)


        )) %>%

        # Isolate 1, 2, 3 values from above
        dplyr::mutate(idx = ifelse(idx == 10, NA, idx)) %>%

        # fill (repeat value) from one value to the next
        tidyr::fill(idx) %>%

        dplyr::group_by(date_grp, idx) %>%

        # Calculate time differences of successive values after grouping by date_grp then idx
        dplyr::mutate(time_diff = dplyr::case_when(

          dplyr::row_number() == 1 ~ lubridate::as.duration(0),

          TRUE ~ lubridate::as.duration( DATE_TIME - dplyr::lag(DATE_TIME, n = 1) )

        )) %>%

        dplyr::ungroup() %>%

        # Rolling index counter that continues beyond grouping to uniquely identify
        # Adds a value for each unique period of sleep or awake given the groupings
        dplyr::mutate(roll_idx = rep(1:length(rle(idx)[[1]]), rle(idx)[[1]]) ) %>%

        ### Begin indicator columns for each stage and group accordingly
        dplyr::group_by(date_grp, roll_idx) %>%

        # Calculate how much time has elapsed
        dplyr::mutate(time_elapsed = lubridate::as.duration( cumsum(time_diff) ) ) %>%

        # Create temporary roll_temp which shifts roll_temp down by one so that time_rev can correctly calculated
        dplyr::ungroup() %>%
        dplyr::group_by(date_grp) %>%
        dplyr::mutate(roll_tmp = dplyr::lag(roll_idx) ) %>%
        dplyr::mutate(roll_tmp = ifelse( is.na( roll_tmp ), dplyr::lead(roll_tmp), roll_tmp ) )  %>%
        dplyr::group_by(date_grp, roll_idx) %>%

        # Create reverse time elapsed column which calculates how much time until the next roll_idx period
        dplyr::mutate(time_rev = lubridate::as.duration( rev( cumsum( time_diff ) ) ) ) %>%

        # DELETE - just for reference
        #tmp <- tmp %>% relocate(SBP, DBP, ID, GROUP, DATE_TIME, VISIT, WAKE, date_grp, wake_ind, idx, time_diff, roll_idx, time_elapsed, roll_tmp, time_rev)

        # At this point, now there are four differentiating values for idx:

        # 0 - The first period of the group which happens to be asleep
        # 1 - The first period of the group which happens to be awake
        # 2 - Following the first period of the group, this is the next period of being asleep
        # 3 - Following the first period of the group, this is the next period of being awake

      # Also, roll_idx and roll_tmp are the unique CUMULATIVE identifiers for each group (within the actual dplyr groups --> grps)


      dplyr::ungroup() %>%
        dplyr::group_by(date_grp, roll_idx) %>%


        ################################################################################
      ##                                                                            ##
      ##    KEY - 1: Evening BP | 2: Lowest BP | 3: Pre-Wake BP | 4: Morning BP     ##
      ##                                                                            ##
      ################################################################################

      # Evening Indicator
      dplyr::mutate(eve_ind = ifelse(time_rev <= lubridate::dhours(2) & wake_ind == 1 & idx != 3, 1, 999)) %>%

        # Pre-Wake Indicator
        dplyr::mutate(prewake_ind = ifelse(time_rev <= lubridate::dhours(2) & wake_ind == 0, 3, 999)) %>%

        # Morning Indicator
        dplyr::mutate(morn_ind = ifelse(time_elapsed <= lubridate::dhours(2) & (wake_ind == 1 | wake_ind == 11) & idx != 1, 4, 999 )) %>%

        # Lowest Reading Indicator
        dplyr::mutate(low_idx = dplyr::case_when(

          wake_ind == 1 | wake_ind == 11 ~ as.numeric(999),
          TRUE ~ as.numeric( which.min(SBP) ) # Need to change to account for both SBP and DBP

        ))


    return(tmp)

}




#' Blood Pressure Sleep Metrics
#'
#' The \code{bp_sleep_metrics} function serves to calculate sleep-dependent metrics
#' from relevant medical literature.
#'
#' @param data User-supplied data set containing blood pressure data. Must
#' contain a Systolic blood pressure (\code{SBP}), Diastolic blood pressure (\code{DBP})
#' as well as \code{ID}, \code{WAKE}, and \code{DATE_TIME} columns.
#'
#' @param subj Optional argument. Allows the user to specify and subset specific subjects
#' from the \code{ID} column of the supplied data set. The \code{subj} argument can be a single
#' value or a vector of elements. The input type should be character, but the function will
#' comply with integers so long as they are all present in the \code{ID} column of the data.
#'
#' @return The function outputs a list containing: (1) a list of count data related
#' to how many instances were observed during night / during day, (2) a list of all
#' summary statistics for SBP, (3) a list of all summary statistics for DBP, and (4)
#' a list of all medical literature metrics.
#'
#' @export
#'
#' @examples
#' hypnos_proc <- process_data(bp_hypnos,
#'                            sbp = "syst",
#'                            dbp = "DIAST",
#'                            date_time = "date.time",
#'                            id = "id",
#'                            wake = "wake",
#'                            visit = "visit",
#'                            hr = "hr",
#'                            map = "map",
#'                            rpp = "rpp",
#'                            pp = "pp")
#'
#' bp_sleep_metrics(hypnos_proc)
#'
bp_sleep_metrics <- function(data, subj = NULL){

    # Function requires: SBP, DBP, DATE_TIME, WAKE, ID

    # Initialize variables for dplyr
    ID = SBP = DBP = wake_ind = eve_ind = morn_ind = prewake_ind = lowest_SBP = lowest_DBP = evening_SBP = evening_DBP = morning_SBP = morning_DBP = NULL
    rm(list = c("ID", "SBP", "DBP", "wake_ind", "eve_ind", "morn_ind", "prewake_ind", "lowest_SBP", "lowest_DBP", "evening_SBP", "evening_DBP", "morning_SBP", "morning_DBP"))


      # Uppercase all column names
      colnames(data) <- toupper( colnames(data) )


      # Ensure SBP column is present
      if( ("SBP" %in% colnames(data)) == FALSE ){

        stop('Could not find SBP column in supplied data set. Make sure to run process_data() for data processing.')

      }

      # Ensure DBP column is present
      if( ("DBP" %in% colnames(data)) == FALSE ){

        stop('Could not find DBP column in supplied data set. Make sure to run process_data() for data processing.')

      }

      # Ensure that both DATE_TIME column is present
      if( ("DATE_TIME" %in% colnames(data)) == FALSE){

        stop('No DATE_TIME column found. Cannot compute sleep stage metrics.')

      }

      # Ensure WAKE column is present
      if( ("WAKE" %in% colnames(data)) == FALSE ){

        stop('No WAKE column found. Cannot compute sleep stage metrics.')

      }

      # Ensure that both DATE_TIME column is present
      if( ("ID" %in% colnames(data)) == FALSE){

        stop('No ID column found. Cannot compute sleep stage metrics.')

      }

      # Check function for whether or not DATE_TIME values are proper date/time format
      is.POSIXct <- function(x) inherits(x, "POSIXct")

      if(is.POSIXct(data$DATE_TIME) == FALSE){

        stop('DATE_TIME column not proper time format. Make sure to run process_data() for data processing.')

      }

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


      #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#

      # Grouping variables

      grps = c("ID", "VISIT", "GROUP", "DATE_TIME")
      grps = grps[which( grps %in% colnames(data) == TRUE)]

      #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#


      # Calculate sleep stage indicator columns using sleep_stages() helper function from above
      data <- sleep_stages(data, grps = grps)





      # ******************************************************************************************************************************* #
      #                                               BP Sleep Periods
      # ******************************************************************************************************************************* #


      ##
      ## FIX ISSUE WITH INCLUDING VALUES OUTSIDE OF LAG/LEAD 4 --> if 11 1 1 22 0 then Morning BP CANNOT include values corresponding to 22 or 0
      ##
      ## Make sure to include option for SBP, DBP, or BOTH
      ##

      if("DATE_TIME" %in% grps){
        grps <- grps[-which(grps %in% "DATE_TIME")]
      }


      #########################
      ##        Counts       ##
      #########################

      # Total Readings for Period
      readings <- data %>%
        dplyr::group_by_at(dplyr::vars( c(grps, "date_grp") ) ) %>%
        dplyr::summarise(N_total = dplyr::n(), .groups = 'drop')

      # Total Awake Readings for Period
      awake_readings <- data %>%
        dplyr::group_by_at(dplyr::vars( c(grps, "date_grp") ) ) %>%
        dplyr::filter(wake_ind == 1 | wake_ind == 11) %>%
        dplyr::summarise(N_awake = dplyr::n(), .groups = 'drop')

      # Total Sleep Readings for Period
      sleep_readings <- data %>%
        dplyr::group_by_at(dplyr::vars( c(grps, "date_grp") ) ) %>%
        dplyr::filter(wake_ind == 0 | wake_ind == 22) %>%
        dplyr::summarise(N_sleep = dplyr::n(), .groups = 'drop')



      #########################
      ##    Systolic (SBP)   ##
      #########################

      # Sleep BP
      sleep_SBP <- data %>%
        dplyr::group_by_at(dplyr::vars( c(grps, "date_grp") ) ) %>%
        dplyr::filter(wake_ind == 0 | wake_ind == 22) %>%
        dplyr::summarise( sleep_SBP = mean(SBP), .groups = 'drop' )

      # Sleep SD
      sleep_SBP_sd <- data %>%
        dplyr::group_by_at(dplyr::vars( c(grps, "date_grp") ) ) %>%
        dplyr::filter(wake_ind == 0 | wake_ind == 22) %>%
        dplyr::summarise( sleep_SBP_sd = sd(SBP), .groups = 'drop' )

      # Awake BP
      awake_SBP <- data %>%
        dplyr::group_by_at(dplyr::vars( c(grps, "date_grp") ) ) %>%
        dplyr::filter(wake_ind == 1 | wake_ind == 11) %>%
        dplyr::summarise( awake_SBP = mean(SBP), .groups = 'drop' )

      # Awake SD
      awake_SBP_sd <- data %>%
        dplyr::group_by_at(dplyr::vars( c(grps, "date_grp") ) ) %>%
        dplyr::filter(wake_ind == 1 | wake_ind == 11) %>%
        dplyr::summarise( awake_SBP_sd = sd(SBP), .groups = 'drop' )



      # Evening BP
      eve_SBP <- data %>%
        dplyr::group_by_at(dplyr::vars( c(grps, "date_grp") ) ) %>%
        dplyr::filter(eve_ind == 1) %>%
        dplyr::summarise(evening_SBP = mean(SBP), .groups = 'drop')


      # Morning BP
      morn_SBP <- data %>%
        dplyr::group_by_at(dplyr::vars( c(grps, "date_grp") ) ) %>%
        dplyr::filter(morn_ind == 4) %>%
        dplyr::summarise(morning_SBP = mean(SBP), .groups = 'drop')


      # Pre-Wake BP
      prewake_SBP <- data %>%
        dplyr::group_by_at(dplyr::vars( c(grps, "date_grp") ) ) %>%
        dplyr::filter(prewake_ind == 3) %>%
        dplyr::summarise(prewake_SBP = mean(SBP), .groups = 'drop')


      # Lowest BP
      low_SBP <- data %>%
        dplyr::group_by_at(dplyr::vars( c(grps, "date_grp", "roll_idx") ) ) %>%
        dplyr::summarise(.groups = 'drop', lowest_SBP = dplyr::case_when(

          # when low is end, take lowest and 2 before
          low_idx[1] == length(SBP) ~ mean(c(SBP[low_idx[1] - 2], SBP[low_idx[1] - 1],
                                             SBP[low_idx[1]])),

          # when low is very beginning, take lowest and two after
          low_idx[1] == 1 ~ mean(SBP[1:3]),

          # when low is in the middle, take low and one before/one after
          TRUE ~ mean(c(SBP[low_idx[1] - 1], SBP[low_idx[1]], SBP[low_idx[1] + 1]))

        )) %>%
        dplyr::filter(!is.na(lowest_SBP))



      ##########################
      ##    Diastolic (SBP)   ##
      ##########################

      # Sleep DBP
      sleep_DBP <- data %>%
        dplyr::group_by_at(dplyr::vars( c(grps, "date_grp") ) ) %>%
        dplyr::filter(wake_ind == 0 | wake_ind == 22) %>%
        dplyr::summarise( sleep_DBP = mean(DBP), .groups = 'drop' )

      # Sleep SD
      sleep_DBP_sd <- data %>%
        dplyr::group_by_at(dplyr::vars( c(grps, "date_grp") ) ) %>%
        dplyr::filter(wake_ind == 0 | wake_ind == 22) %>%
        dplyr::summarise( sleep_DBP_sd = sd(DBP), .groups = 'drop' )

      # Awake DBP
      awake_DBP <- data %>%
        dplyr::group_by_at(dplyr::vars( c(grps, "date_grp") ) ) %>%
        dplyr::filter(wake_ind == 1 | wake_ind == 11) %>%
        dplyr::summarise( awake_DBP = mean(DBP), .groups = 'drop' )

      # Awake SD
      awake_DBP_sd <- data %>%
        dplyr::group_by_at(dplyr::vars( c(grps, "date_grp") ) ) %>%
        dplyr::filter(wake_ind == 1 | wake_ind == 11) %>%
        dplyr::summarise( awake_DBP_sd = sd(DBP), .groups = 'drop' )



      # Evening BP
      eve_DBP <- data %>%
        dplyr::group_by_at(dplyr::vars( c(grps, "date_grp") ) ) %>%
        dplyr::filter(eve_ind == 1) %>%
        dplyr::summarise(evening_DBP = mean(DBP), .groups = 'drop')


      # Morning BP
      morn_DBP <- data %>%
        dplyr::group_by_at(dplyr::vars( c(grps, "date_grp") ) ) %>%
        dplyr::filter(morn_ind == 4) %>%
        dplyr::summarise(morning_DBP = mean(DBP), .groups = 'drop')


      # Pre-Wake BP
      prewake_DBP <- data %>%
        dplyr::group_by_at(dplyr::vars( c(grps, "date_grp") ) ) %>%
        dplyr::filter(prewake_ind == 3) %>%
        dplyr::summarise(prewake_DBP = mean(DBP), .groups = 'drop')


      # Lowest BP
      low_DBP <- data %>%
        dplyr::group_by_at(dplyr::vars( c(grps, "date_grp", "roll_idx") ) ) %>%
        dplyr::summarise(.groups = 'drop', lowest_DBP = dplyr::case_when(

          # when low is end, take lowest and 2 before
          low_idx[1] == length(DBP) ~ mean(c(DBP[low_idx[1] - 2], DBP[low_idx[1] - 1],
                                             DBP[low_idx[1]])),

          # when low is very beginning, take lowest and two after
          low_idx[1] == 1 ~ mean(DBP[1:3]),

          # when low is in the middle, take low and one before/one after
          TRUE ~ mean(c(DBP[low_idx[1] - 1], DBP[low_idx[1]], DBP[low_idx[1] + 1]))

        )) %>%
        dplyr::filter(!is.na(lowest_DBP))



      # Count data summary
      sleep_counts <- dplyr::left_join(readings, awake_readings, by = c(grps, "date_grp"))
      sleep_counts <- dplyr::left_join(sleep_counts, sleep_readings, by = c(grps, "date_grp"))

      # SBP Metrics
      sleep_summary_SBP <- dplyr::left_join(sleep_SBP, awake_SBP, by = c(grps, "date_grp"))
      sleep_summary_SBP <- dplyr::left_join(sleep_summary_SBP, eve_SBP, by = c(grps, "date_grp"))
      sleep_summary_SBP <- dplyr::left_join(sleep_summary_SBP, low_SBP, by = c(grps, "date_grp"))
      sleep_summary_SBP <- dplyr::left_join(sleep_summary_SBP, prewake_SBP, by = c(grps, "date_grp"))
      sleep_summary_SBP <- dplyr::left_join(sleep_summary_SBP, morn_SBP, by = c(grps, "date_grp"))
      sleep_summary_SBP <- dplyr::left_join(sleep_summary_SBP, sleep_SBP_sd, by = c(grps, "date_grp"))
      sleep_summary_SBP <- dplyr::left_join(sleep_summary_SBP, awake_SBP_sd, by = c(grps, "date_grp"))


      # DBP Metrics
      sleep_summary_DBP <- dplyr::left_join(sleep_DBP, awake_DBP, by = c(grps, "date_grp"))
      sleep_summary_DBP <- dplyr::left_join(sleep_summary_DBP, eve_DBP, by = c(grps, "date_grp"))
      sleep_summary_DBP <- dplyr::left_join(sleep_summary_DBP, low_DBP, by = c(grps, "date_grp"))
      sleep_summary_DBP <- dplyr::left_join(sleep_summary_DBP, prewake_DBP, by = c(grps, "date_grp"))
      sleep_summary_DBP <- dplyr::left_join(sleep_summary_DBP, morn_DBP, by = c(grps, "date_grp"))
      sleep_summary_DBP <- dplyr::left_join(sleep_summary_DBP, sleep_DBP_sd, by = c(grps, "date_grp"))
      sleep_summary_DBP <- dplyr::left_join(sleep_summary_DBP, awake_DBP_sd, by = c(grps, "date_grp"))


      # Remove roll_idx column
      sleep_summary_SBP <- sleep_summary_SBP %>% dplyr::select(-c("roll_idx"))
      sleep_summary_DBP <- sleep_summary_DBP %>% dplyr::select(-c("roll_idx"))


      ### Sleep Metrics from Literature

      # Dip Calc Percentage
      # TO DO: include dip calc classification
      sleep_metrics <- sleep_summary_SBP %>% dplyr::mutate(dip_calc_SBP = 1 - sleep_SBP / awake_SBP) %>% dplyr::select(grps, "dip_calc_SBP") # matches dip_calc function
      dip2 <- sleep_summary_DBP %>% dplyr::mutate(dip_calc_DBP = 1 - sleep_DBP / awake_DBP) %>% dplyr::select(grps, "dip_calc_DBP")

      sleep_metrics <- dplyr::left_join(sleep_metrics, dip2, by = c(grps))

      # "Nocturnal BP Fall" (Kario et al 2002)
      tmp1 <- sleep_summary_SBP %>% dplyr::mutate(noct_fall_SBP = evening_SBP / lowest_SBP) %>% dplyr::select(grps, "noct_fall_SBP")
      tmp2 <- sleep_summary_DBP %>% dplyr::mutate(noct_fall_DBP = evening_DBP / lowest_DBP) %>% dplyr::select(grps, "noct_fall_DBP")

      sleep_metrics <- dplyr::left_join(sleep_metrics, tmp1, by = c(grps))
      sleep_metrics <- dplyr::left_join(sleep_metrics, tmp2, by = c(grps))

      # Sleep-Trough MBPS (Kario et al 2002)
      tmp1 <- sleep_summary_SBP %>% dplyr::mutate(ST_mbps_SBP = morning_SBP - lowest_SBP) %>% dplyr::select(grps, "ST_mbps_SBP")
      tmp2 <- sleep_summary_DBP %>% dplyr::mutate(ST_mbps_DBP = morning_DBP - lowest_DBP) %>% dplyr::select(grps, "ST_mbps_DBP")

      sleep_metrics <- dplyr::left_join(sleep_metrics, tmp1, by = c(grps))
      sleep_metrics <- dplyr::left_join(sleep_metrics, tmp2, by = c(grps))

      # Prewake MBPS (Kario et al 2002)
      tmp1 <- sleep_summary_SBP %>% dplyr::mutate(PW_mbps_SBP = morning_SBP - prewake_SBP) %>% dplyr::select(grps, "PW_mbps_SBP")
      tmp2 <- sleep_summary_DBP %>% dplyr::mutate(PW_mbps_DBP = morning_DBP - prewake_DBP) %>% dplyr::select(grps, "PW_mbps_DBP")

      sleep_metrics <- dplyr::left_join(sleep_metrics, tmp1, by = c(grps))
      sleep_metrics <- dplyr::left_join(sleep_metrics, tmp2, by = c(grps))

          # SBP ME Metrics (Kario 2005)
          tmp1 <- sleep_summary_SBP %>%

              # Morningness-Eveningness Average (Kario 2005)
              dplyr::mutate(ME_SBP_avg = (morning_SBP + evening_SBP) / 2 ) %>%

              # Morningness-Eveningness Difference (Kario 2005)
              dplyr::mutate(ME_SBP_diff = morning_SBP - evening_SBP) %>%

            dplyr::select(grps, "ME_SBP_avg", "ME_SBP_diff")

          # DBP ME Metrics
          tmp2 <- sleep_summary_DBP %>%

              # Morningness-Eveningness Average (Kario 2005)
              dplyr::mutate(ME_DBP_avg = (morning_DBP + evening_DBP) / 2 ) %>%

              # Morningness-Eveningness Difference (Kario 2005)
              dplyr::mutate(ME_DBP_diff = morning_DBP - evening_DBP) %>%

            dplyr::select(grps, "ME_DBP_avg", "ME_DBP_diff")

          sleep_metrics <- dplyr::left_join(sleep_metrics, tmp1, by = c(grps))
          sleep_metrics <- dplyr::left_join(sleep_metrics, tmp2, by = c(grps))
      #
      # # Weighted Standard Deviation (wSD)
      # tmp1 <- sleep_summary_SBP %>%
      #               dplyr::mutate( wSD_SBP = ( ( awake_SBP_sd * N_awake ) + ( sleep_SBP_sd * N_sleep ) ) / ( N_awake + N_sleep ) ) %>%
      #               dplyr::select("wSD_SBP")
      # tmp2 <- sleep_summary_DBP %>%
      #               dplyr::mutate( wSD_DBP = ( ( awake_DBP_sd * N_awake ) + ( sleep_DBP_sd * N_sleep ) ) / ( N_awake + N_sleep ) ) %>%
      #               dplyr::select("wSD_SBP")
      #
      # sleep_metrics <- dplyr::left_join(sleep_metrics, tmp1)
      # sleep_metrics <- dplyr::left_join(sleep_metrics, tmp2)
      #

      return(list("Sleep_Counts" = sleep_counts,
                  "SBP_Sleep_Summary" = sleep_summary_SBP,
                  "DBP_Sleep_Summary" = sleep_summary_DBP,
                  "Sleep_Metrics" = sleep_metrics))
}
