# Calculate summaries for SBP and DBP to use with sleep metric calculations
# First does check on unusual sleep periods, or periods with unusually short naps
# For each subject, visit, group, want to do the following
# Give a time indicator that is more descriptive than sleep/wake by obeying the following
# pre-wake - 2 last hours in the sleep period before wake
# after-wake - 2 hours after waking up
# pre-sleep - 2 hours before falling asleep
# lowest SBP - average of 3 readings centered around lowest


# First, need to identify the sleep periods start and end. This is in general not easy because going to sleep time and BP measurement time are not going to be the same. If we use ToD as differentiation, and say measurements are 00:20, 00:50, 1:20, 1:50, 2:20, then technically the first 5 will be used rather than the first 4. Same is going to happen with sleep end.

# Approximation - the first time we observe 1 is going to be treated as sleep start, the last time we observe 1 is going to be treated as sleep end? or is it 0? something to discuss.

# In any case, for each subject/group/visit, we can create Diff column that takes consecutive measurements differences of Wake column, so that
# 1 - 0 = 1 sleep end
# 0 - 1 = -1 sleep start
# 0 - 0 = 1 - 1 = 0 no change

# Then for each 1, one can use lubridate to allocate pre-wake, post-wake
# For each -1, one can use lubridate to allocate pre-sleep
get_summaries_SBP_DBP <- function(data){

  # Initialize variables for dplyr
  SBP = DBP = WAKE = period = NULL
  rm(list = c("SBP", "DBP", "WAKE", "period"))

  # Assume the times are in chronological order
  # Figure out total number of measurements
  nread = nrow(data)

  # Create difference column (here I assume the time is already sorted, should probably check)
  wake_binary = as.numeric(as.character(data$WAKE))
  # 1 - 0 = 1 sleep end
  # 0 - 1 = -1 sleep start
  # 0 - 0 = 1 - 1 = 0 no change
  data$diff = c(wake_binary[2:nread] - wake_binary[1:(nread - 1)], 0)

  # Number of sleep period starts
  n_starts = sum(data$diff == -1)
  # Number of sleep period ends
  n_ends = sum(data$diff == 1)

  # No sleep starts identified
  if (n_starts == 0){
    stop(paste("Can not detect the beginning of sleep period for subject", unique(data$ID), ". Double check the WAKE column."))
  }

  # No sleep ends identified
  if (n_ends == 0){
    stop(paste("Can not detect the ending of sleep period for subject", unique(data$ID), ". Double check the WAKE column."))
  }

  # Check if there is more than one sleep period. Otherwise use sleep_start and sleep_end times based on diff
  if ((n_starts > 1) | (n_ends > 1)){
    message(paste("More than one sleep period detected for subject "), unique(data$ID))
    if (n_starts != n_ends){
      stop(paste("Incomplete sleep period for subject", unique(data$ID), ". Double check the WAKE column."))
    }
    # Extract the times of start and end, should have the same length now
    wake_ends = data$DATE_TIME[which(data$diff == -1)] # This is the last time WAKE = 1
    sleep_starts = data$DATE_TIME[which(data$diff == -1) + 1] # This is the first time WAKE = 0
    sleep_ends = data$DATE_TIME[which(data$diff == 1)] # This is the last time WAKE = 0
    wake_starts = data$DATE_TIME[which(data$diff == 1) + 1] # This is the first time WAKE = 1

    # Calculate the durations
    durations = difftime(sleep_ends, sleep_starts, units = "hours")

    # If any duration is less than two hours, treat as nap and remove
    no_naps = which(as.numeric(durations) > 2)

    # If after that filtering no periods are left or more than one period, stop and exit
    if (length(no_naps) == 0){
      stop(paste("Identified sleep period for subject", unique(data$ID), "is less than 2 hours. Double check the WAKE column."))
    }

    if (length(no_naps) > 1){
      warning(paste("Current functionality only supports one sleep period per subject. Only the first sleep period is used for calculations."))
      wake_end = wake_ends[no_naps[1]]
      sleep_start = sleep_starts[no_naps[1]]
      sleep_end = sleep_ends[no_naps[1]]
      wake_start = wake_starts[no_naps[1]]
    }else{
      message(paste("The sleep period shorter than 2 hours is treated as nap, and not used in calculations of sleep metrics."))
      # Only one sleep period that has no gaps
      wake_start = wake_starts[no_naps]
      sleep_start = sleep_starts[no_naps]
      sleep_end = sleep_ends[no_naps]
      wake_end = wake_ends[no_naps]
    }
  }else{
    # Extract the times of start and end, should be exactly one
    wake_end = data$DATE_TIME[which(data$diff == -1)] # last time WAKE = 1
    sleep_start = data$DATE_TIME[which(data$diff == -1) + 1] # This is the first time WAKE = 0
    sleep_end = data$DATE_TIME[which(data$diff == 1)] # This is the last time WAKE = 0
    wake_start = data$DATE_TIME[which(data$diff == 1) + 1] # First time WAKE = 1
  }

  # Allocate period to pre-sleep, post-wake, pre-wake or just regular wake/sleep depending on the hours relative to sleep start/end
  data = data%>%
    dplyr::mutate(period = dplyr::case_when(
      (DATE_TIME < sleep_start) & (DATE_TIME >= sleep_start - lubridate::hours(2)) ~ "presleep",
      (DATE_TIME > sleep_end) & (DATE_TIME <= sleep_end + lubridate::hours(2)) ~ "postwake",
      (DATE_TIME <= sleep_end) & (DATE_TIME > sleep_end - lubridate::hours(2)) ~ "prewake",
      TRUE ~ ifelse(WAKE == 1, "wake", "sleep")
    ))

  # Sometimes nothing is allocated to a particular period. Can adjust definition of sleep_start/sleep_end a little to see if extra measurements could be taken. This can only be done for presleep and postwake.
  if (sum(data$period == "presleep") == 0){
    # Presleep was calculated as 2 hours from sleep start, where sleep_start was the 1st measurement with WAKE = 0. Try to stretch out by counting 2 hours from wake_last instead
    data = data%>%
      dplyr::mutate(period = dplyr::case_when(
        (DATE_TIME <= wake_end) & (DATE_TIME > wake_end - lubridate::hours(2)) ~ "presleep",
        TRUE ~ period
      ))
  }

  if (sum(data$period == "postwake") == 0){
    # Postwake was calculated as 2 hours from sleep end. Try to stretch out by counting 2 hours from wake_start
    data = data%>%
      dplyr::mutate(period = dplyr::case_when(
        (DATE_TIME >= wake_start) & (DATE_TIME < wake_start + lubridate::hours(2)) ~ "postwake",
        TRUE ~ period
      ))
  }

  # Create a new tibble where only 3 periods are separated, and are factors, so that NA are formed if some are absent
  data_period = data %>%
    dplyr::filter(period %in% c("presleep", "prewake", "postwake"))%>%
    dplyr::mutate(period = factor(period, levels = c("presleep", "prewake", "postwake")))

  # SBP
  ###########################################################################
  # Mean and sd by WAKE
  stat_SBP_wake = tidyr::pivot_wider(data %>%
                                       dplyr::group_by(WAKE) %>%
                                       dplyr::summarize(meanSBP = mean(SBP), sdSBP = sd(SBP)),
                                     values_from = 2:3, names_from = 1)
  names(stat_SBP_wake) = c("sleep_SBP", "wake_SBP", "sleep_SBP_sd", "wake_SBP_sd")

  # Mean by period
  stat_SBP_period = tidyr::pivot_wider(data_period %>%
                                         dplyr::group_by(period) %>%
                                         dplyr::summarize(SBP = mean(SBP)),
                                       values_from = 2, names_from = 1)

  names(stat_SBP_period) = c( "presleep_SBP", "prewake_SBP", "postwake_SBP")

  # Combine the two
  summary_SBP = dplyr::bind_cols(stat_SBP_wake, stat_SBP_period)

  # Mean over lowest BP reading during sleep, and two surrounding it
  lowest_id = which((data$SBP == min(data$SBP[data$WAKE == 0]))&(data$WAKE == 0))
  summary_SBP$lowest_SBP = mean(data$SBP[c(lowest_id - 1, lowest_id, lowest_id + 1)])

  # DBP
  ###########################################################################
  # Mean and sd by WAKE
  stat_DBP_wake = tidyr::pivot_wider(data %>%
                                       dplyr::group_by(WAKE) %>%
                                       dplyr::summarize(meanDBP = mean(DBP), sdDBP = sd(DBP)),
                                     values_from = 2:3, names_from = 1)
  names(stat_DBP_wake) = c("sleep_DBP", "wake_DBP", "sleep_DBP_sd", "wake_DBP_sd")

  # Mean by period
  stat_DBP_period = tidyr::pivot_wider(data_period %>%
                                         dplyr::group_by(period) %>%
                                         dplyr::summarize(DBP = mean(DBP)),
                                       values_from = 2, names_from = 1)
  names(stat_DBP_period) = c("presleep_DBP", "prewake_DBP", "postwake_DBP")

  # Combine the two
  summary_DBP = dplyr::bind_cols(stat_DBP_wake, stat_DBP_period)

  # Mean over lowest BP reading during sleep, and two surrounding it
  lowest_id = which((data$DBP == min(data$DBP[data$WAKE == 0]))&(data$WAKE == 0))
  summary_DBP$lowest_DBP = mean(data$DBP[c(lowest_id - 1, lowest_id, lowest_id + 1)])

  # Return the output (summary_SBP and summary_DBP) together
  return(dplyr::bind_cols(summary_DBP, summary_SBP))
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
#' @details
#' The calculation of BP metrics related to sleep is based on averages of BP readings from four periods as identified in Kairo et al. (2003):
#' presleep or evening (2 hours before sleep start),
#' prewake (2 hours before wake),
#' postwake or morning (2 hours after wake),
#' lowest (3 measurements centered at the minimal BP reading over sleep). T
#' he function uses \code{WAKE} column to automatically allocate BP measurements to various periods. The following metrics are defined as a
#' function of the period averages (separately for SBP and DBP)
#'
#' \code{dip_calc} = 1 - mean_sleep_BP/mean_wake_BP (dip proportion)
#'
#' \code{noct_fall} = mean_presleep_BP - mean_lowest_BP (nocturnal fall)
#'
#' \code{ST_mbps} = mean_postwake_BP - mean_lowest_BP (sleep through morning blood pressure surge)
#'
#' \code{PW_mbps} = mean_postwake_BP - mean_prewake_BP (prewake morning blood pressure surge)
#'
#' \code{ME_avg} = (mean_presleep_BP + mean_postwake_BP)/2 (morning-evening average)
#'
#' \code{ME_diff} = mean_postwake_BP - mean_presleep_BP (morning-evening difference)
#'
#' \code{wSD} = ( (wake_SD x HRS_wake) + (sleep_SD x HRS_sleep) ) / (HRS_wake + HRS_sleep) (weighted standard deviation)
#'
#' @return The function outputs a list containing 4 tibble objects corresponding to the following tables:
#' \item{[[1]]}{Counts of how many BP measurements were observed overall (\code{N_total}),
#' total number of readings during sleep (\code{N_sleep}),
#' total number of readings during wake (\code{N_wake}),
#' number of unique hours recorded during the sleep period (\code{HRS_sleep}), and
#' number of unique hours recorded during the wake period (\code{HRS_wake}) for each subject ID and grouping variable}
#' \item{[[2]]}{Summary statistics for systolic BP measurements (SBP): mean SBP value over Sleep and Wake, sd SBP value
#' over Sleep and Wake, mean SBP value over presleep period (evening in Kario et al. (2003)), mean SBP value over prewake period,
#' mean SBP value over postwake period (morning in Kario et al. (2003)), mean SBP value over 3 reading centered at the lowest SBP
#' value during sleep}
#' \item{[[3]]}{Summary statistics for diastolic BP measurements (DBP), same as for SBP}
#' \item{[[4]]}{BP metrics associated with sleep as defined above, separately for SBP and DBP}
#'
#' @references
#' Kario, K., Pickering, T. G., Umeda, Y., Hoshide, S., Hoshide, Y., Morinari, M., ... & Shimada, K. (2003). Morning surge in blood
#' pressure as a predictor of silent and clinical cerebrovascular disease in elderly hypertensives: a prospective study. Circulation,
#' 107(10), 1401-1406.
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
#'                            pp = "pp",
#'                            bp_type = "abpm")
#'
#' bp_sleep_metrics(hypnos_proc)
#'
bp_sleep_metrics <- function(data, subj = NULL){

    # Function requires: SBP, DBP, DATE_TIME, WAKE, ID

    # Initialize variables for dplyr
    ID = DATE_TIME = SBP = DBP = WAKE = HOUR = lowest_SBP = lowest_DBP = presleep_SBP = presleep_DBP = postwake_SBP = postwake_DBP = prewake_SBP = prewake_DBP = sleep_DBP = wake_DBP = sleep_SBP = wake_SBP = wSD_DBP = wake_SBP_sd = wake_DBP_sd = sleep_SBP_sd = sleep_DBP_sd = HRS_sleep = HRS_wake = NULL
    rm(list = c("ID", "DATE_TIME", "SBP", "DBP", "WAKE", "HOUR", "lowest_SBP", "lowest_DBP", "presleep_SBP", "presleep_DBP", "postwake_SBP", "postwake_DBP", "prewake_SBP", "prewake_DBP", "sleep_DBP", "wake_DBP", "sleep_SBP", "wake_SBP", "wSD_DBP", "wake_SBP_sd", "wake_DBP_sd", "sleep_SBP_sd", "sleep_DBP_sd", "HRS_sleep", "HRS_wake"))


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

      # Check the data type
      if ( ("BP_TYPE" %in% colnames(data)) == FALSE){
        stop('No BP_TYPE column found. Cannot automatically identify the type of BP data. Make sure to process the data using process_data function.')
      }

      if (unique(data$BP_TYPE) != "ABPM"){
        if (unique(data$BP_TYPE) == "HBPM"){
          warning("The supplied data has HBPM type, for which calculation of sleep BP metrics is not recommended. If the supplied data should be ABPM type, please rerun process_data function with correct BP type specfication.")
        }
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

      grps = c("ID", "VISIT", "GROUP")
      grps = grps[which( grps %in% colnames(data) == TRUE)]

      #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#
      # Calculation of sleep counts table (table 1) based on WAKE column

      sleep_counts = data %>%
        dplyr::group_by_at(dplyr::vars(grps)) %>%
        dplyr::summarize(N_total = dplyr::n(), # Total number of readings
                         N_wake = sum(WAKE == 1), # Number of total readings while awake
                         N_sleep = sum(WAKE == 0), # Number of total readings while asleep
                         HRS_wake = dplyr::n_distinct(HOUR[WAKE == 1]), # Number of unique hours recorded while awake (for wSD calc)
                         HRS_sleep = dplyr::n_distinct(HOUR[WAKE == 0]), # Number of unique hours recorded during sleep (for wSD calc)
                         .groups = 'drop')



      # ******************************************************************************************************************************* #
      #                                               BP Sleep Periods
      # ******************************************************************************************************************************* #

      # Order time from oldest to newest
      data = data[order(data$DATE_TIME), ]

      # Calculate all summaries on SBP and DBP using get_summaries_SBP_DBP
      output = data %>%
        dplyr::group_by_at(dplyr::vars(grps)) %>%
        dplyr::summarise(get_summaries_SBP_DBP(data.frame(ID, DATE_TIME, SBP, DBP, WAKE)))

      # Extract ids of grouping variables in output
      idgrps = which(names(output) %in% grps)

      #########################
      ##    Systolic (SBP)   ##
      #########################

      # Extract positions of SBP summaries in output
      idSBP = grep("SBP", names(output))

      # Create SBP table
      sleep_summary_SBP = output[ , c(idgrps, idSBP)]

      ##########################
      ##    Diastolic (DBP)   ##
      ##########################

      # Extract positions of DBP summaries in output
      idDBP = grep("DBP", names(output))

      # Create DBP table
      sleep_summary_DBP = output[ , c(idgrps, idDBP)]


      #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#
      ### Calculation of Sleep Metrics (Table 4) based on SBP/DBP sleep summaries (Tables 2-3)
      ####################################

      # Join the sleep_summary_SBP/DBP with the sleep counts to include the HRS_wake/sleep for the wSD calculation
      sleep_summary_SBP_all <- suppressMessages(dplyr::left_join(sleep_summary_SBP, sleep_counts)) #suppressMessages ignores the "joining by: ..." message
      sleep_summary_DBP_all <- suppressMessages(dplyr::left_join(sleep_summary_DBP, sleep_counts))

      # Metrics based on SBP
      sleep_metrics_SBP <- sleep_summary_SBP_all %>%
        dplyr::group_by_at(dplyr::vars(grps)) %>%
        dplyr::transmute(
          # dip calculation (proportion)
          dip_calc_SBP = 1 - sleep_SBP / wake_SBP,
          # "Nocturnal BP Fall" (Kario et al 2002)
          noct_fall_SBP = presleep_SBP / lowest_SBP,
          # Sleep-Trough MBPS (Kario et al 2002)
          ST_mbps_SBP = postwake_SBP - lowest_SBP,
          # Prewake MBPS (Kario et al 2002)
          PW_mbps_SBP = postwake_SBP - prewake_SBP,
          # Morningness-Eveningness Average (Kario 2005)
          ME_SBP_avg = (postwake_SBP + presleep_SBP) / 2,
          # Morningness-Eveningness Difference (Kario 2005)
          ME_SBP_diff = postwake_SBP - presleep_SBP,
          # Weighted Standard Deviation (Bilo et al 2007)
          wSD_SBP = ( ( (wake_SBP_sd * HRS_wake) + (sleep_SBP_sd * HRS_sleep) ) / (HRS_wake + HRS_sleep) )
        )

      # Metrics based on DBP
      sleep_metrics_DBP <- sleep_summary_DBP_all %>%
        dplyr::group_by_at(dplyr::vars(grps)) %>%
        dplyr::transmute(
          # dip calculation (proportion)
          dip_calc_DBP = 1 - sleep_DBP / wake_DBP,
          # "Nocturnal BP Fall" (Kario et al 2002)
          noct_fall_DBP = presleep_DBP / lowest_DBP,
          # Sleep-Trough MBPS (Kario et al 2002)
          ST_mbps_DBP = postwake_DBP - lowest_DBP,
          # Prewake MBPS (Kario et al 2002)
          PW_mbps_DBP = postwake_DBP - prewake_DBP,
          # Morningness-Eveningness Average (Kario 2005)
          ME_DBP_avg = (postwake_DBP + presleep_DBP) / 2,
          # Morningness-Eveningness Difference (Kario 2005)
          ME_DBP_diff = postwake_DBP - presleep_DBP,
          # Weighted Standard Deviation (Bilo et al 2007)
          wSD_DBP = ( ( (wake_DBP_sd * HRS_wake) + (sleep_DBP_sd * HRS_sleep) ) / (HRS_wake + HRS_sleep) )
        )

      # Combine the two together
      sleep_metrics <- dplyr::left_join(sleep_metrics_SBP, sleep_metrics_DBP, by = c(grps))

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
