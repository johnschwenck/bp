
#' Data Pre-Processor
#'
#' @description A helper function to assist in pre-processing the user-supplied
#' input data for use with other functions.
#' Typically, this function will process the data and be stored as another
#' dataframe. This function ensures that the supplied data complies with every
#' function within the \code{bp} package. See Vignette for further details.
#'
#' @param data User-supplied dataset containing blood pressure data. Must
#' contain data for Systolic blood pressure and Diastolic blood pressure at a
#' minimum.
#' @param sbp Required column name (character string) corresponding to Systolic Blood
#' Pressure (mmHg)
#' @param dbp Required column name (character string) corresponding to Diastolic Blood
#' Pressure (mmHg)
#' @param bp_datetime Optional column name (character string) corresponding to Date/Time,
#' but HIGHLY recommended to supply if available.
#' @param id Optional column name (character string) corresponding to subject ID. Typically
#' needed for data corresponding to more than one subject.
#' @param wake Optional column name (character string) corresponding to sleep status. A
#' WAKE value of 1 indicates that the subject is awake and 0 implies asleep.
#' @param visit Optional column name (character string) corresponding to Visit number
#' @param hr Optional column name (character string) corresponding to Heart Rate (bpm)
#' @param pp Optional column name (character string) corresponding to Pulse Pressure
#' (SBP - DBP). If not supplied, it will be calculated automatically.
#' @param map Optional column name (character string) corresponding to Mean Arterial
#' Pressure
#' @param rpp Optional column name (character string) corresponding to Rate Pulse
#' Pressure (SBP * HR). If not supplied, but HR column available, then
#' RPP will be calculated automatically.
#' @param ToD_int Optional vector that overrides the default interval for the Time-of-Day periods.
#' By default, the Morning, Afternoon, Evening, and Night periods are set at 6, 12, 18, 0 respectively,
#' where 0 corresponds to the 24th hour of the day (i.e. Midnight). By inputting a vector for the
#' \code{ToD_int} function argument, the default period can be re-arranged accordingly.
#' For example, ToD_int = c(5, 13, 18, 23) would correspond to a period for
#' Morning starting at 5:00 (until 13:00), Afternoon starting at 13:00 (until 18:00),
#' Evening starting at 18:00 (until 23:00), and Night starting at 23:00 (until 5:00)
#'
#' @return A processed dataframe object that cooperates with every other
#' function within the bp package - all column names and formats comply.
#' @export
#'
#' @examples
#' # Load hypnos_data
#' data(hypnos_data)
#'
#' # Process data for hypnos_data
#' data1 <- process_data(hypnos_data, sbp = "SYST", dbp = "DIAST", bp_datetime = "date.time",
#' id = "id", wake = "wake", visit = "visit", hr = "hr", pp ="pp", map = "map", rpp = "rpp")
#'
#' data1
#'
#' # Load bp_jhs data
#' data(bp_jhs)
#'
#' # Process data for bp_jhs
#' data2 <- process_data(bp_jhs, sbp = "Sys.mmHg.", dbp = "Dias.mmHg.", bp_datetime = "DateTime",
#' hr = "Pulse.bpm.")
#'
#' data2
#'
process_data <- function(data, sbp = NULL, dbp = NULL, bp_datetime = NULL, id = NULL, wake = NULL, visit = NULL, hr = NULL, pp = NULL, map = NULL, rpp = NULL, ToD_int = NULL){


  # Ensure that data is either data.frame or matrix
  if(is.data.frame(data) == FALSE){

    if(is.matrix(data) == FALSE){

      stop('Invalid data type. Please use either data.frame or matrix\n')

    }else{

      warning('Converted matrix data type to data.frame\n')

      data <- as.data.frame(data)

    }
  }


  # Convert all column names to upper case for consistency
  if(!is.data.frame(data)){
    stop('Error: did not convert to data frame\n')
  }

  colnames(data) <- toupper(colnames(data))

  # Throw error if SBP and DBP columns aren't specified
  if(is.null(sbp) | is.null(dbp)){

    stop('Both "SBP" and "DBP" column names must be specified.\n')

  }




  # Systolic BP (SBP)
  if(is.character(sbp)){

    if(toupper(sbp) %in% colnames(data) == FALSE){

      warning('Could not find user-defined SBP argument name in dataset. \ni.e. for example, if user improperly defines sbp = "syst" but that column name does not exist in the dataset, \nthen there will be no matches for "syst". \nCheck spelling of SBP argument.\n')

      if(length(grep(paste("\\bSBP\\b", sep = ""), names(data))) == 1){

        stop('Fix user-defined argument name for SBP. \nNote: A column in the dataset DOES match the name "SBP": \nif this is the correct column, indicate as such in function argument. \ni.e. sbp = "SBP" \n ')

      }

    }else{

      col_idx <- grep(paste("\\b",toupper(sbp),"\\b", sep = ""), names(data))
      data <- data[, c(col_idx, (1:ncol(data))[-col_idx])]

      if(colnames(data)[1] != "SBP"){

        colnames(data)[1] <- "SBP"
        data$SBP <- as.numeric(data$SBP)
      }
    }
  } else {
    stop('User-defined SBP name must be character.\n')
  }# if working with numeric below, remove this bracket





  # Diastolic BP (DBP)
  if(is.character(dbp)){

    if(toupper(dbp) %in% colnames(data) == FALSE){

      warning('User-defined DBP name does not match column name of supplied dataset. \ni.e. for example, if user improperly defines dbp = "diast" but there is no column name in the dataset, \nthen there will be no matches for "diast". \nCheck spelling of DBP argument.\n')

      if(length(grep(paste("\\bDBP\\b", sep = ""), names(data))) == 1){

        stop('Fix user-defined argument name for DBP. \nNote: A column in the dataset DOES match the name "DBP": \nif this is the correct column, indicate as such in function argument. \ni.e. sbp = "DBP" \n ')

      }
    }else{

      col_idx <- grep(paste("\\b",toupper(dbp),"\\b", sep = ""), names(data))
      data <- data[, c(1, col_idx, (2:ncol(data))[-col_idx+1])]

      if(colnames(data)[2] != "DBP"){

        colnames(data)[2] <- "DBP"
        data$DBP <- as.numeric(data$DBP)
      }
    }
  } else {
    stop('User-defined DBP name must be character.\n')
  }




  # Pulse Pressure
  if(is.null(pp)){

    if(length(grep(paste("\\bPP\\b", sep = ""), names(data))) == 0){

      data$PP <- data$SBP - data$DBP
      message('No PP column found. Automatically generated from SBP and DBP columns.\n')

    }

    col_idx <- grep(paste("\\bPP\\b", sep = ""), names(data))
    colnames(data)[col_idx] <- "PP"
    data <- data[ , c(1:2, col_idx, (3:(ncol(data)))[-col_idx + 2])]

    data$PP <- as.numeric(data$PP)

  }else if(is.character(pp)){ # if character (i.e. by name)

    if(toupper(pp) %in% colnames(data) == FALSE){ # is pp argument found in data colnames

      stop('User-defined PP name does not match column name of supplied dataset\n')

    }else{

      col_idx <- grep(paste("\\b",toupper(pp),"\\b", sep = ""), names(data))
      colnames(data)[col_idx] <- "PP"
      data <- data[, c(1:2, col_idx, (3:ncol(data))[-col_idx+2])]

      data$PP <- as.numeric(data$PP)
    }
  } else {

    stop('User-defined PP name must be character.\n')
  }






  # Heart Rate
  if(is.null(hr)){

    if(length(grep(paste("\\bHR\\b", sep = ""), names(data))) == 1){

      warning('HR column found in data. \nIf this column corresponds to Heart Rate, \nuse hr = "HR" in the function argument.\n')

    }

  } else if(is.character(hr)){

    if(toupper(hr) %in% colnames(data) == FALSE){

      stop('User-defined HR name does not match column name of supplied dataset\n')

    }else{

      col_idx <- grep(paste("\\b",toupper(hr),"\\b", sep = ""), names(data))
      colnames(data)[col_idx] <- "HR"
      data <- data[, c(1:3, col_idx, (4:ncol(data))[-col_idx+3])]
      data$HR <- as.numeric(data$HR)
    }
  } else {
    stop('User-defined HR name must be character.\n')
  }






  # Rate Pulse Product
  if(is.null(rpp)){

    # Try to find "RPP" column in data: if found, length is 1 (a number), if not found, length is 0 (i.e. logical(0) )
    if(length(grep(paste("\\bRPP\\b", sep = ""), names(data))) == 1){

      warning('"RPP" argument not specified in function, but "RPP" column found in data. \n Ensure that the "RPP" column in the data is not the desired column.')

    } else if( (length(grep(paste("\\bRPP\\b", sep = ""), names(data))) == 0) & (length(grep(paste("\\bHR\\b", sep = ""), names(data))) == 1)){

      data$RPP <- data$SBP * data$HR
      data$RPP <- as.numeric(data$RPP)

      hr_idx <- grep(paste("\\bHR\\b", sep = ""), names(data))
      rpp_idx <- grep(paste("\\bRPP\\b", sep = ""), names(data))
      data <- data[, c(1:hr_idx, rpp_idx, ((hr_idx+1):ncol(data))[-rpp_idx+hr_idx])]

      message('No RPP column found. Automatically generated from SBP and HR columns.\n')
    }

  }else if( (toupper(rpp) %in% colnames(data)) == FALSE){

    stop('User-defined RPP name does not match column name of supplied dataset\n')

  }else if( (length(grep(paste("\\bHR\\b", sep = ""), names(data))) == 1) & (toupper(rpp) %in% colnames(data)) ){ # HR column is present and in position 4

    col_idx <- grep(paste("\\b",toupper(rpp),"\\b", sep = ""), names(data))
    colnames(data)[col_idx] <- "RPP"
    data <- data[, c(1:4, col_idx, (5:ncol(data))[-col_idx+4])]

  }else if( (length(grep(paste("\\bHR\\b", sep = ""), names(data))) == 0) & (toupper(rpp) %in% colnames(data)) ){ # HR column is NOT present

    col_idx <- grep(paste("\\b",toupper(rpp),"\\b", sep = ""), names(data))
    colnames(data)[col_idx] <- "RPP"
    data <- data[, c(1:3, col_idx, (4:ncol(data))[-col_idx+3])]

  }





  # Mean Arterial Pressure
  if(is.null(map)){

    if(length(grep(paste("\\bMAP\\b", sep = ""), names(data))) == 0){

      data$MAP <- (1/3) * data$SBP + (2/3) * data$DBP
      message('No MAP column found. Automatically generated from SBP and DBP columns.\n')

      col_idx <- grep(paste("\\bMAP\\b", sep = ""), names(data))
      colnames(data)[col_idx] <- "MAP"
      data <- data[, c(1:2, col_idx, (3:ncol(data))[-col_idx+2])]

      data$MAP <- as.numeric(data$MAP)

    }else if(length(grep(paste("\\bMAP\\b", sep = ""), names(data))) == 1){

      warning('MAP column found in data. \nIf this column corresponds to Mean Arterial Pressure, \nuse map = "MAP" in the function argument.\n')

    }
  } else if(toupper(map) %in% colnames(data) == FALSE){

    stop('User-defined MAP name does not match column name of supplied dataset\n')

  } else {

    col_idx <- grep(paste("\\b",toupper(map),"\\b", sep = ""), names(data))
    colnames(data)[col_idx] <- "MAP"
    data <- data[, c(1:2, col_idx, (3:ncol(data))[-col_idx+2])]

  }





  # Wake (1: Awake | 0: Asleep)
  if(!is.null(wake)){

    if(toupper(wake) %in% colnames(data) == FALSE){

      stop('User-defined ID name does not match column name of supplied dataset\n')

    }

    col_idx <- grep(paste("\\b",toupper(wake),"\\b", sep = ""), names(data))
    colnames(data)[col_idx] <- "WAKE"

    if(length(unique(data$WAKE)) > 2){

      warning('Ignoring wake argument. Wake column must only contain 2 unique values corresponding to awake or asleep status. \nTypically, these are denoted as 1 for Awake and 0 for Asleep.\n')

    }else{

      data$WAKE <- as.integer(data$WAKE) # coerce to integers

      # Assuming there are only two unique values, move column to beginning of df
      data <- data[, c(col_idx, (1:ncol(data))[-col_idx])]

    }
  }






  # Visit
  if(!is.null(visit)){

    if(toupper(visit) %in% colnames(data) == FALSE){

      stop('User-defined VISIT name does not match column name of supplied dataset\n')

    } else {

      col_idx <- grep(paste("\\b",toupper(visit),"\\b", sep = ""), names(data))
      colnames(data)[col_idx] <- "VISIT"

      data$VISIT <- as.integer(data$VISIT)
      data <- data[, c(col_idx, (1:ncol(data))[-col_idx])]

    }
  }


  # Prepare all variables used via dplyr
  SBP_Category = DBP_Category = Time_of_Day = NULL
  rm(list = c(SBP_Category, DBP_Category, Time_of_Day))


  # Date & Time (DateTime object)
  if(!is.null(bp_datetime)){

    if(toupper(bp_datetime) %in% colnames(data) == FALSE){

      stop('User-defined bp_datetime name does not match column name of supplied dataset\n')

    } else {

      col_idx <- grep(paste("\\b",toupper(bp_datetime),"\\b", sep = ""), names(data))
      colnames(data)[col_idx] <- "DATE_TIME"
      data <- data[, c(col_idx, (1:ncol(data))[-col_idx])]

      data$DATE_TIME <- as.POSIXct(data$DATE_TIME) # coerce to proper time format

      data$Weekday = ordered(weekdays(as.Date(data$DATE_TIME), abbreviate = TRUE),
                             levels = c("Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat"))

      # Time of Day
      if(is.null(ToD_int)){

        # Assume --> Night: 0 - 6, Morning: 6 - 12, Afternoon: 12 - 18, Evening: 18 - 24
        data <- data %>% dplyr::mutate(Time_of_Day =
                                         dplyr::case_when(lubridate::hour(DATE_TIME) >= 0  & lubridate::hour(DATE_TIME) < 6  ~ "Night",
                                                          lubridate::hour(DATE_TIME) >= 6  & lubridate::hour(DATE_TIME) < 12 ~ "Morning",
                                                          lubridate::hour(DATE_TIME) >= 12 & lubridate::hour(DATE_TIME) < 18 ~ "Afternoon",
                                                          lubridate::hour(DATE_TIME) >= 18 & lubridate::hour(DATE_TIME) < 24 ~ "Evening",))

      # ToD_int should be a vector that contains the starting hour for Morning, Afternoon, Evening, Night in that order
      }else {

      if(!is.vector(ToD_int)){

        warning('ToD_int must be of type vector. Coerced input to vector.')
        ToD_int <- as.vector(ToD_int)

      }else if( length(ToD_int) != 4 ){

        stop('Time of Day Interval (ToD_int) must only contain 4 numbers corresponding to the start of the Morning, Afternoon, Evening, and Night periods.')

      }else if( any(ToD_int > 24) == TRUE ){

        stop('Time of Day Interval (ToD_int) must only lie within the 0 - 24 hour interval. Use 24-hour time.')

      }else if( any( duplicated( ToD_int ) ) == TRUE ){

        stop('Cannot have overlapping / duplicate values within the ToD interval.')

      }else if( ToD_int[1] == 24 ){

        ToD_int[1] <- 0

      }else if( ToD_int[length(ToD_int)] == 24 ){

        ToD_int[length(ToD_int)] <- 0

      }else if( utils::head(ToD_int, 1) == utils::tail(ToD_int, 1) ){

          stop('Same starting and ending Time of Day interval values. Coerced ending value to next hour.')

          # if(head(ToD_int, 1) == 23){
          #
          #   ToD_int[ 1 ] <- 0
          #   ToD_int[ length(ToD_int) ] <- 23
          #
          # }else{
          #
          #   ToD_int[ 1 ] <- ToD_int[ 1 ] + 1
          # }

      }else if( all(ToD_int[2:3] == cummax(ToD_int[2:3])) == FALSE ){

        stop('values within interval must be increasing for the Morning and Afternoon periods (the second and third values in the interval).')

      }

      tmp <- c(ToD_int, ToD_int[1])
      difftmp <- diff(tmp)
      difftmp[which(difftmp < 0)] <- (24 - tmp[which(difftmp < 0)]) + tmp[which(difftmp < 0)+1]
      testval <- sum(difftmp)

      if(testval > 24){
        stop('Invalid interval for Time of Day. Ensure that hours do not overlap each other and are consistent within a 24 hour period.')
        }

      if( (ToD_int[1] < ToD_int[2]) & (ToD_int[3] < ToD_int[4]) ){

        data <- data %>% dplyr::mutate(Time_of_Day =
                         dplyr::case_when(lubridate::hour(DATE_TIME) >= ToD_int[4] | lubridate::hour(DATE_TIME) < ToD_int[1]  ~ "Night",
                                          lubridate::hour(DATE_TIME) >= ToD_int[1] & lubridate::hour(DATE_TIME) < ToD_int[2] ~ "Morning",
                                          lubridate::hour(DATE_TIME) >= ToD_int[2] & lubridate::hour(DATE_TIME) < ToD_int[3] ~ "Afternoon",
                                          lubridate::hour(DATE_TIME) >= ToD_int[3] & lubridate::hour(DATE_TIME) < ToD_int[4] ~ "Evening"))

      }else if( (ToD_int[1] > ToD_int[2]) & (ToD_int[3] < ToD_int[4]) ){

        data <- data %>% dplyr::mutate(Time_of_Day =
                         dplyr::case_when(lubridate::hour(DATE_TIME) >= ToD_int[4] & lubridate::hour(DATE_TIME) < ToD_int[1]  ~ "Night",
                                        ((lubridate::hour(DATE_TIME) >= ToD_int[1] & lubridate::hour(DATE_TIME) < 24)) | (lubridate::hour(DATE_TIME) < ToD_int[2]) ~ "Morning",
                                          lubridate::hour(DATE_TIME) >= ToD_int[2] & lubridate::hour(DATE_TIME) < ToD_int[3] ~ "Afternoon",
                                          lubridate::hour(DATE_TIME) >= ToD_int[3] & lubridate::hour(DATE_TIME) < ToD_int[4] ~ "Evening",))

      }else if( (ToD_int[1] < ToD_int[2]) & (ToD_int[3] > ToD_int[4]) ){

        data <- data %>% dplyr::mutate(Time_of_Day =
                         dplyr::case_when(lubridate::hour(DATE_TIME) >= ToD_int[4] & lubridate::hour(DATE_TIME) < ToD_int[1]  ~ "Night",
                                          lubridate::hour(DATE_TIME) >= ToD_int[1] & lubridate::hour(DATE_TIME) < ToD_int[2] ~ "Morning",
                                          lubridate::hour(DATE_TIME) >= ToD_int[2] & lubridate::hour(DATE_TIME) < ToD_int[3] ~ "Afternoon",
                                        ((lubridate::hour(DATE_TIME) >= ToD_int[3]) & (lubridate::hour(DATE_TIME) < 24)) | (lubridate::hour(DATE_TIME) < ToD_int[4]) ~ "Evening"))

      }


      }
    }
  }




  # Date-Only
  if(length(grep("^DATE$", names(data))) == 1){

    # If DATE column found

    message('NOTE: DATE column found in data and coerced to as.Date() format.\n')

    data[,grep("^DATE$", names(data))] <- as.Date(data[,grep("^DATE$", names(data))])

    col_idx <- grep("^DATE$", names(data))
    colnames(data)[col_idx] <- "DATE"
    data <- data[, c(col_idx, (1:ncol(data))[-col_idx])]

  } else if(length(grep("^DATE_TIME$", names(data))) == 1){

    # If no DATE column found

    message('NOTE: Created DATE column from DATE_TIME column\n')

    # Create DATE column using as.Date of DATE_TIME
    data$DATE <- as.Date(data$DATE_TIME)

    col_idx <- grep("^DATE$", names(data))
    colnames(data)[col_idx] <- "DATE"
    data <- data[, c(col_idx, (1:ncol(data))[-col_idx])]

  }




  # ID
  if(!is.null(id)){

    if(toupper(id) %in% colnames(data) == FALSE){

      stop('User-defined ID name does not match column name of supplied dataset\n')

    } else {

      col_idx <- grep(paste("\\b",toupper(id),"\\b", sep = ""), names(data))
      colnames(data)[col_idx] <- "ID"
      data <- data[, c(col_idx, (1:ncol(data))[-col_idx])]

    }
  }


  # BP Categories / Stages
  # Only require SBP and DBP

  data <- data %>%
    dplyr::mutate(SBP_Category = dplyr::case_when(SBP <= 100 ~ "Hypotension",
                                    SBP > 100 & SBP <= 120 ~ "Normal",
                                    SBP > 120 & SBP <= 130 ~ "Elevated",
                                    SBP > 130 & SBP <= 140 ~ "Stage 1",
                                    SBP > 140 & SBP <= 180 ~ "Stage 2",
                                    SBP > 180 ~ "Crisis"),
           SBP_Category = factor(SBP_Category, levels = c("Hypotension", "Normal", "Elevated", "Stage 1", "Stage 2", "Crisis")),

           DBP_Category = dplyr::case_when(DBP <= 60 ~ "Hypotension",
                                    DBP > 60  & DBP <= 80 ~ "Normal",
                                    DBP > 80  & DBP <= 85 ~ "Elevated",
                                    DBP > 85  & DBP <= 90 ~ "Stage 1",
                                    DBP > 90 & DBP <= 120 ~ "Stage 2",
                                    DBP > 120 ~ "Crisis"),
           DBP_Category = factor(DBP_Category, levels = c("Hypotension", "Normal", "Elevated", "Stage 1", "Stage 2", "Crisis")),
           )


  return(data)
}



