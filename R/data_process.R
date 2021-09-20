#' Data Pre-Processor
#'
#' @description A helper function to assist in pre-processing the user-supplied
#' input data in a standardized format for use with other functions in the \code{bp} package.
#' See Vignette for further details.
#'
#' @param data User-supplied dataset containing blood pressure data. Must
#' contain data for Systolic blood pressure and Diastolic blood pressure at a
#' minimum.
#'
#' @param bp_type Required argument specifying which of the three BP data types
#' ("HBPM", "ABPM", or "AP") the input data is. Default \code{bp_type} set to "HBPM".
#' This argument determines which processing steps are necessary to yield sensible
#' output.
#'
#' @param ap (For AP data only) Required column name (character string) corresponding
#' to continuous Arterial Pressure (AP) (mmHg). Note that this is a required argument
#' so long as bp_type = "AP". Ensure that bp_type is set accordingly.
#'
#' @param time_elap (For AP data only) Column name corresponding to the time elapsed
#' for the given AP waveform data.
#'
#' @param sbp Required column name (character string) corresponding to Systolic Blood
#' Pressure (mmHg)
#'
#' @param dbp Required column name (character string) corresponding to Diastolic Blood
#' Pressure (mmHg)
#'
#' @param date_time Optional column name (character string) corresponding to Date/Time,
#' but HIGHLY recommended to supply if available.
#'
#' For DATE-only columns (with no associated time), leave date_time = NULL. DATE-only
#' adjustments are automatic. Dates can be automatically calculated off DATE_TIME column
#' provided that it is called "DATE_TIME" exactly.
#'
#' @param id Optional column name (character string) corresponding to subject ID. Typically
#' needed for data corresponding to more than one subject. For one-subject datasets, ID
#' will default to 1 (if ID column not found in dataset)
#'
#' @param group Optional column name (character string) corresponding to an additional
#' grouping variable that can be used to further break down data. NOTE that this simply
#' sets the column as "GROUP" so that other functions recognize which column to use as
#' the grouping variable.
#'
#' @param wake Optional column name (character string) corresponding to sleep status. A
#' WAKE value of 1 indicates that the subject is awake and 0 implies asleep.
#'
#' @param visit Optional column name (character string) corresponding to Visit number
#'
#' @param hr Optional column name (character string) corresponding to Heart Rate (bpm)
#'
#' @param pp Optional column name (character string) corresponding to Pulse Pressure
#' (SBP - DBP). If not supplied, it will be calculated automatically.
#'
#' @param map Optional column name (character string) corresponding to Mean Arterial
#' Pressure
#'
#' @param rpp Optional column name (character string) corresponding to Rate Pulse
#' Pressure (SBP * HR). If not supplied, but HR column available, then
#' RPP will be calculated automatically.
#'
#' @param DoW Optional column name (character string) corresponding to the Day of the Week.
#' If not supplied, but DATE or DATE_TIME columns available, then DoW will be created
#' automatically. DoW values must be abbreviated as such \code{c("Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat")}
#'
#' @param ToD_int Optional vector of length 4, acceptable values are from 0 to 23 in a an order corresponding to hour for Morning, Afternoon, Evening, Night). This vector allows to override the default interval for the Time-of-Day periods: if NULL, the Morning, Afternoon, Evening, and Night periods are set at 6, 12, 18, 0 respectively,
#' where 0 corresponds to the 24th hour of the day (i.e. Midnight).
#' For example, ToD_int = c(5, 13, 18, 23) would correspond to a period for
#' Morning starting at 5:00 (until 13:00), Afternoon starting at 13:00 (until 18:00),
#' Evening starting at 18:00 (until 23:00), and Night starting at 23:00 (until 5:00)
#'
#' @param eod Optional argument to adjust the delineation for the end of day (eod). The supplied value should be a character string with 4 characters representing the digits of 24-hour time, e.g. "1310" corresponds to 1:10pm. For individuals who
#' do not go to bed early or work night-shifts, this argument adjusts the \code{DATE} column so that the days change at specified time. \code{eod = "0000"} means no change. \code{eod = "1130"} will adjust the date of the readings up to 11:30am to the previous date. \code{eod = "1230"} will adjust the date of the readings after 12:30pm to the next date.
#'
#' @param data_screen Optional logical argument; default set to TRUE. Screens for extreme values in the data
#' for both \code{SBP} and \code{DBP} according to Omboni, et al (1995) paper - Calculation of Trough:Peak
#' Ratio of Antihypertensive Treatment from Ambulatory Blood Pressure: Methodological Aspects
#'
#' @param SUL Systolic Upper Limit (SUL). If \code{data_screen = TRUE}, then \code{SUL} sets the upper limit by which
#' to exclude any \code{SBP} values that exceed this threshold. The default is set to 240 per Omboni, et al (1995)
#' paper - Calculation of Trough:Peak Ratio of Antihypertensive Treatment from Ambulatory Blood Pressure:
#' Methodological Aspects
#'
#' @param SLL Systolic Lower Limit (SLL). If \code{data_screen = TRUE}, then \code{SLL} sets the lower limit by which
#' to exclude any \code{SBP} values that fall below this threshold. The default is set to 50 per Omboni, et al (1995)
#' paper - Calculation of Trough:Peak Ratio of Antihypertensive Treatment from Ambulatory Blood Pressure:
#' Methodological Aspects
#'
#' @param DUL Diastolic Upper Limit (DUL). If \code{data_screen = TRUE}, then \code{DUL} sets the upper limit by which
#' to exclude any \code{DBP} values that exceed this threshold. The default is set to 140 per Omboni, et al (1995)
#' paper - Calculation of Trough:Peak Ratio of Antihypertensive Treatment from Ambulatory Blood Pressure:
#' Methodological Aspects
#'
#' @param DLL Diastolic Lower Limit (DLL). If \code{data_screen = TRUE}, then \code{DLL} sets the lower limit by which
#' to exclude any \code{DBP} values that fall below this threshold. The default is set to 40 per Omboni, et al (1995)
#' paper - Calculation of Trough:Peak Ratio of Antihypertensive Treatment from Ambulatory Blood Pressure:
#' Methodological Aspects
#'
#' @param HRUL Heart Rate Upper Limit (HRUL). If \code{data_screen = TRUE}, then \code{HRUL} sets the upper limit
#' by which to exclude any \code{HR} values that exceed this threshold. The default is set to 220 per the upper limit
#' of the common max heart rate formula: 220 - age
#'
#' see https://www.cdc.gov/physicalactivity/basics/measuring/heartrate.htm
#'
#' @param HRLL Heart Rate Upper Limit (HRUL). If \code{data_screen = TRUE}, then \code{HRUL} sets the upper limit
#' by which to exclude any \code{HR} values that exceed this threshold. The default is set to 27 per Guinness
#' World Records - lowest heart rate (https://www.guinnessworldrecords.com/world-records/lowest-heart-rate)
#'
#' @param inc_low Optional logical argument dictating whether or not to include the "Low" category for BP
#' classification column (and the supplementary SBP/DBP Category columns). Default set to TRUE.
#'
#' @param inc_crisis Optional logical argument dictating whether or not to include the "Crisis" category for BP
#' classification column (and the supplementary SBP/DBP Category columns). Default set to TRUE.
#'
#' @param agg Optional argument specifying whether or not to aggregate the data based on the amount of time
#' between observations. If \code{agg = TRUE} then any two (or more) observations within the amount of
#' time alloted by the agg_thresh argument will be averaged together.
#'
#' @param agg_thresh Optional argument specifying the threshold of how many minutes can pass between readings
#' (observations) and still be considered part of the same sitting. The default is set to 3 minutes. This implies
#' that if two or more readings are within 3 minutes of each other, they will be averaged together (if agg is
#' set to TRUE).
#'
#' @param collapse_df Optional argument that collapses the dataframe to eliminate repeating rows after
#' aggregation.
#'
#' @param dt_fmt Optional argument that specifies the input date/time format (dt_fmt). Default set to "ymd HMS"
#' but can take on any format specified by the lubridate package.
#'
#' @param chron_order Optional argument that specifies whether to order the data in chronological (Oldest
#' dates & times at the top / first) or reverse chronological order (Most recent dates & times at the top / first).
#' TRUE refers to chronological order; FALSE refers to reverse chronological order. The default is set to
#' FALSE (i.e. most recent observations listed first in the dataframe).
#'
#' See https://lubridate.tidyverse.org/reference/parse_date_time.html for more details.
#'
#' @param tz Optional argument denoting the respective time zone. Default time zone set to "UTC". See
#' Use \code{OlsonNames()} for a complete listing of all available time zones that can be used in this
#' argument.
#'
#' @return A processed dataframe object with standardized column names and formats to use with the rest of bp package functions. The following standardized column names are used throughout
#' \item{BP_TYPE}{One of AP, HBPM or ABPM}
#' \item{ID}{Subject ID}
#' \item{SBP}{Systolic Blood Pressure}
#' \item{DBP}{Diastolic Blood Pressure}
#' \item{SBP_CATEGORY}{Ordinal, SBP characterization into "Low" < "Normal"<"Elevated"<"Stage 1"< "Stage 2" < "Crisis". "Low" is not included if \code{inc_low = FALSE}. "Crisis" is not included if \code{inc_crisis = FALSE}.}
#' \item{DBP_CATEGORY}{Ordinal, DBP characterization into "Low" < "Normal"<"Elevated"<"Stage 1"< "Stage 2" < "Crisis". "Low" is not included if \code{inc_low = FALSE}. "Crisis" is not included if \code{inc_crisis = FALSE}.}
#' \item{BP_CLASS}{Blood pressure categorization based on paired values (SBP, DBP) into one of the 8 stages according to Lee et al. 2020. See \code{\link{bp_scatter}}}
#' \item{HR}{Heart Rate}
#' \item{MAP}{Mean Arterial Pressure}
#' \item{PP}{Pulse Pressure, SBP-DBP}
#' \item{DATE_TIME}{Date and time in POSIXct format}
#' \item{DATE}{Date only in Date format}
#' \item{MONTH}{Month, integer from 1 to 12}
#' \item{DAY}{Day, integer from 1 to 31}
#' \item{YEAR}{Four digit year}
#' \item{DAY_OF_WEEK}{Ordinal, with "Sun"<"Mon"<"Tue"<"Wed"<"Thu"<"Fri"<"Sat"}
#' \item{TIME}{Time in character format}
#' \item{HOUR}{Integer, from 0 to 23}
#' \item{TIME_OF_DAY}{One of "Morning", "Afternoon", "Evening" or "Night"}
#'
#' @references
#' Lee H, Yano Y, Cho SMJ, Park JH, Park S, Lloyd-Jones DM, Kim HC. Cardiovascular risk of isolated
#' systolic or diastolic hypertension in young adults. \emph{Circulation}. 2020; 141:1778–1786.
#' \doi{10.1161/CIRCULATIONAHA.119.044838}
#'
#' Omboni, S., Parati, G*., Zanchetti, A., Mancia, G. Calculation of trough: peak ratio of
#' antihypertensive treatment from ambulatory blood pressure: methodological aspects
#' \emph{Journal of Hypertension}. October 1995 - Volume 13 - Issue 10 - p 1105-1112
#' \doi{10.1097/00004872-199510000-00005}
#'
#' Unger, T., Borghi, C., Charchar, F., Khan, N. A., Poulter, N. R., Prabhakaran, D., ... & Schutte,
#' A. E. (2020). 2020 International Society of Hypertension global hypertension practice guidelines.
#' \emph{Hypertension}, 75(6), 1334-1357.
#' \doi{10.1161/HYPERTENSIONAHA.120.15026}
#'
#' @export
#'
#' @examples
#' # Load bp_hypnos
#' data("bp_hypnos")
#'
#' # Process data for bp_hypnos
#' hypnos_proc <- process_data(bp_hypnos,
#'                               bp_type = 'abpm',
#'                               sbp = 'syst',
#'                               dbp = 'diast',
#'                               date_time = 'date.time',
#'                               hr = 'hr',
#'                               pp = 'PP',
#'                               map = 'MaP',
#'                               rpp = 'Rpp',
#'                               id = 'id',
#'                               visit = 'Visit',
#'                               wake = 'wake',
#'                               data_screen = FALSE)
#'
#' hypnos_proc
#'
#'
#' # Load bp_jhs data
#' data("bp_jhs")
#'
#' # Process data for bp_jhs
#' # Note that bp_type defaults to "hbpm" and is therefore not specified
#' jhs_proc <- process_data(bp_jhs,
#'                          sbp = "Sys.mmHg.",
#'                          dbp = "Dias.mmHg.",
#'                          date_time = "DateTime",
#'                          hr = "Pulse.bpm.")
#'
#' jhs_proc
#'
process_data <- function(data,

                             # Home Blood Pressure Monitor (HBPM) | Ambulatory Blood Pressure Monitor (ABPM) | Arterial Pressure (AP)
                             bp_type = c("hbpm", "abpm", "ap"),

                             # For AP data
                             ap = NULL,
                             time_elap = NULL,

                             # For all other data (HBPM, ABPM)
                             sbp = NULL,
                             dbp = NULL,
                             date_time = NULL,
                             id = NULL,
                             group = NULL,
                             wake = NULL,
                             visit = NULL,
                             hr = NULL,
                             pp = NULL,
                             map = NULL,
                             rpp = NULL,


                             # Options
                             DoW = NULL,
                             ToD_int = NULL,
                             eod = NULL,
                             data_screen = TRUE,
                             SUL = 240,
                             SLL = 50,
                             DUL = 140,
                             DLL = 40,
                             HRUL = 220,
                             HRLL = 27,
                             inc_low = TRUE,
                             inc_crisis = TRUE,
                             agg = FALSE,
                             agg_thresh = 3,
                             collapse_df = FALSE,
                             dt_fmt = "ymd HMS",
                             chron_order = FALSE,
                             tz = "UTC"){


  # Prepare all variables used via dplyr
  SBP = DBP = HR = SBP_Category = DBP_Category = TIME_OF_DAY = BP_CLASS = NULL
  rm(list = c("SBP", "DBP", "HR", "SBP_Category", "DBP_Category", "TIME_OF_DAY", "BP_CLASS"))



  # Match BP Type: Home Blood Pressure Monitor (HBPM) | Ambulatory Blood Pressure Monitor (ABPM) | Arterial Pressure (AP)
  bp_type <- tolower(bp_type)
  bp_type <- toupper( match.arg(bp_type) )


  # Ensure that data is either data.frame or matrix
  if(is.data.frame(data) == FALSE){

    if(is.matrix(data) == FALSE){

      stop('Invalid data type. Please use either data.frame or matrix\n')

    }else{

      warning('Converted matrix data type to data.frame\n')

      data <- as.data.frame(data)

    }
  }


  # Convert to data frame
  if(!is.data.frame(data)){
    stop('Error: did not convert to data frame\n')
  }

  # Convert all column names to upper case for consistency
  colnames(data) <- toupper(colnames(data))



  # ************************************************************************************************************ #


  # Arterial Pressure
  if(bp_type == "AP"){

    # Adjust AP Values
    data <- ap_adj(data = data, ap = ap)

    # Time Adjustment - Numeric Values representing a fraction of time that has elapsed (i.e. 0.25 minutes, 1.75 seconds, etc)
    # This column is typically the sampling rate column
    data <- time_adj(data = data, time_elap = time_elap)

    # ID
    data <- id_adj(data = data, id = id)

    # Group
    data <- group_adj(data = data, group = group)

    # Create column indicating blood pressure type (bp_type)
    data$BP_TYPE <- bp_type

  }


  # ************************************************************************************************************ #


  if(toupper(bp_type) == "ABPM" | toupper(bp_type) == "HBPM"){


        # Throw error if SBP and DBP columns aren't specified
        if(is.null(sbp) | is.null(dbp)){

          stop('Both "SBP" and "DBP" column names must be specified.\n')

        }


        # Adjust Systolic Blood Pressure (SBP)
        data <- sbp_adj(data = data, sbp = sbp, data_screen = data_screen, SUL = SUL, SLL = SLL)
        sbp_tmp <- names(data)[1] # For bp_stages function

        # Adjust Diastolic Blood Pressure (DBP)
        data <- dbp_adj(data = data, dbp = dbp, data_screen = data_screen, DUL = DUL, DLL = DLL)
        dbp_tmp <- names(data)[2] # For bp_stages function

        # Adjust ID
        data <- id_adj(data = data, id = id)

        # Adjust Group
        data <- group_adj(data = data, group = group)


        # Adjust Visit
        data <- visit_adj(data = data, visit = visit)

        # Adjust Date/Time values
        if(!is.null(date_time)){

            data <- date_time_adj(data = data, date_time = date_time, dt_fmt = dt_fmt, ToD_int = ToD_int, chron_order = chron_order, tz = tz)

        }

        # Adjust eod / dates
        if(!is.null(eod)){

              # Incorporate End-of-Day argument and calibrate dates
              data <- eod_adj(data = data, eod = eod)

        }

        # Adjust WAKE indicator
        data <- wake_adj(data = data, wake = wake, bp_type = bp_type)

        # Adjust Day of Week
        data <- dow_adj(data = data, DoW = DoW)

        # ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ #

        #+++++++++++++++++++++++++++++++++++#
        # KEEP THIS ORDER: RPP, PP, MAP, HR #
        #+++++++++++++++++++++++++++++++++++#

        # Adjust Heart Rate (HR)
        data <- hr_adj(data = data, hr = hr, data_screen = data_screen, HRUL = HRUL, HRLL = HRLL)

        # Adjust Rate Pressure Product (RPP)
        data <- rpp_adj(data = data, rpp = rpp)

        # Adjust Pulse Pressure (PP)
        data <- pp_adj(data = data, pp = pp)

        # Adjust Mean Arterial Pressure (MAP)
        data <- map_adj(data = data, map = map)


        # ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ #

        # Aggregate data if selected
        if(agg == TRUE){

          data <- agg_adj(data = data, bp_type = bp_type, agg_thresh = agg_thresh, collapse_df = collapse_df)

        }


        # BP Stages
        data <- bp_stages(data = data,
                          sbp = sbp_tmp,
                          dbp = dbp_tmp,
                          inc_low = inc_low,
                          inc_crisis = inc_crisis,
                          data_screen = data_screen,
                          SUL = SUL,
                          SLL = SLL,
                          DUL = DUL,
                          DLL = DLL)


        # Move Classification columns to correct positions
        data <- data %>%
                  dplyr::relocate(BP_CLASS, .after = DBP) #%>%
                  #dplyr::relocate(SBP_CATEGORY, .after = BP_CLASS) %>%
                  #dplyr::relocate(DBP_CATEGORY, .after = SBP_CATEGORY)



  }


  # Create column indicating blood pressure type (bp_type)
  data$BP_TYPE <- bp_type

  # Sanity check for any future additions to this function to ensure all columns are capitalized for consistency
  colnames(data) <- toupper( colnames(data) )

  # Convert back to data frame
  data <- as.data.frame(data)


  return(data)
}
