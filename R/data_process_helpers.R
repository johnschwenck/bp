## Helper Functions for process_data()




########################################################################################################
#                                                                                                      #
#                                      Arterial Pressure                                         #
#                                                                                                      #
########################################################################################################

ap_adj <- function(data, ap = NULL){

  AP = NULL
  rm(list = c("AP"))

  # Arterial Pressure (AP)
  if(is.character(ap)){

    if(toupper(ap) %in% toupper( colnames(data) ) == FALSE){

      warning('Could not find user-defined AP argument name in dataset. \ni.e. for example, if user improperly defines ap = "art_pres" but that column name does not exist in the dataset, \nthen there will be no matches for "art_pres". \nCheck spelling of AP argument.\n')

      if(length(grep(paste("\\bAP\\b", sep = ""), toupper(names(data)))) == 1){

        stop('Fix user-defined argument name for AP. \nNote: A column in the dataset DOES match the name "AP": \nif this is the correct column, indicate as such in function argument. \ni.e. ap = "AP" \n ')

      }

    }else{

      col_idx <- grep(paste("\\b",toupper(ap),"\\b", sep = ""), toupper(names(data)) )
      data <- data[, c(col_idx, (1:ncol(data))[-col_idx])]

      if(colnames(data)[1] != "AP"){

        colnames(data)[1] <- "AP"
        data$AP <- as.numeric(data$AP)

      }

    }
  } else {
    stop('User-defined AP name must be character.\n')
  }

  return(data)

}




########################################################################################################
#                                                                                                      #
#                                      Systolic Blood Pressure                                         #
#                                                                                                      #
########################################################################################################

sbp_adj <- function(data, sbp = NULL, data_screen){

  SBP = NULL
  rm(list = c("SBP"))

      # Systolic BP (SBP)
      if(is.character(sbp)){

        if(toupper(sbp) %in% toupper( colnames(data) ) == FALSE){

          warning('Could not find user-defined SBP argument name in dataset. \ni.e. for example, if user improperly defines sbp = "syst" but that column name does not exist in the dataset, \nthen there will be no matches for "syst". \nCheck spelling of SBP argument.\n')

          if(length(grep(paste("\\bSBP\\b", sep = ""), toupper(names(data)))) == 1){

            stop('Fix user-defined argument name for SBP. \nNote: A column in the dataset DOES match the name "SBP": \nif this is the correct column, indicate as such in function argument. \ni.e. sbp = "SBP" \n ')

          }

        }else{

          col_idx <- grep(paste("\\b",toupper(sbp),"\\b", sep = ""), toupper(names(data)) )
          data <- data[, c(col_idx, (1:ncol(data))[-col_idx])]

          if(colnames(data)[1] != "SBP"){

            colnames(data)[1] <- "SBP"
            data$SBP <- as.numeric(data$SBP)

          }

          # Screen for extreme values
          if(data_screen == TRUE){

            # Screening criteria: Eliminate values {SBP > 240 | SBP < 50} according to Omboni, et al (1995) paper
            #   - Calculation of Trough:Peak Ratio of Antihypertensive Treatment from Ambulatory
            #     Blood Pressure: Methodological Aspects
            data <- data %>%
              dplyr::filter(SBP < 240 & SBP > 50)

          }

        }
      } else {
        stop('User-defined SBP name must be character.\n')
      }# if working with numeric below, remove this bracket

  return(data)

}




########################################################################################################
#                                                                                                      #
#                                     Diastolic Blood Pressure                                         #
#                                                                                                      #
########################################################################################################

dbp_adj <- function(data, dbp = NULL, data_screen){

  DBP = NULL
  rm(list = c("DBP"))

      # Diastolic BP (DBP)
      if(is.character(dbp)){

        if(toupper(dbp) %in% toupper(colnames(data)) == FALSE){

          warning('User-defined DBP name does not match column name of supplied dataset. \ni.e. for example, if user improperly defines dbp = "diast" but there is no column name in the dataset, \nthen there will be no matches for "diast". \nCheck spelling of DBP argument.\n')

          if(length(grep(paste("\\bDBP\\b", sep = ""), toupper(names(data)))) == 1){

            stop('Fix user-defined argument name for DBP. \nNote: A column in the dataset DOES match the name "DBP": \nif this is the correct column, indicate as such in function argument. \ni.e. sbp = "DBP" \n ')

          }
        }else{

          col_idx <- grep(paste("\\b",toupper(dbp),"\\b", sep = ""), toupper(names(data)) )
          data <- data[, c(1, col_idx, (2:ncol(data))[-col_idx+1])]

          if(colnames(data)[2] != "DBP"){

            colnames(data)[2] <- "DBP"
            data$DBP <- as.numeric(data$DBP)

          }

          # Screen for extreme values
          if(data_screen == TRUE){

            # Screening criteria: Eliminate values {DBP > 140 | DBP < 40} according to Omboni, et al (1995) paper
            #   - Calculation of Trough:Peak Ratio of Antihypertensive Treatment from Ambulatory Blood Pressure: Methodological Aspects
            data <- data %>%
              dplyr::filter(DBP < 140 & DBP > 40)

          }

        }
      } else {
        stop('User-defined DBP name must be character.\n')
      }

  return(data)
}





########################################################################################################
#                                                                                                      #
#                                            Pulse Pressure                                            #
#                                                                                                      #
########################################################################################################

pp_adj <- function(data, pp = NULL){


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

  return(data)

}




########################################################################################################
#                                                                                                      #
#                                            Heart Rate                                                #
#                                                                                                      #
########################################################################################################

hr_adj <- function(data, hr = NULL, data_screen){

  HR = NULL
  rm(list = c("HR"))

      # Heart Rate
      if(is.null(hr)){

        if(length(grep(paste("\\bHR\\b", sep = ""), names(data))) == 1){

          warning('HR column found in data. \nIf this column corresponds to Heart Rate, \nuse hr = "HR" in the function argument.\n')

          # Screen for extreme values
          if(data_screen == TRUE){

            # Screening Criteria:
            # - Lowest HR recorded: https://www.guinnessworldrecords.com/world-records/lowest-heart-rate
            # - High HR from the common {220 - age} formula
            data <- data %>%
              dplyr::filter(HR < 220 & HR > 20)

          }

        }

      } else if(is.character(hr)){

        if(toupper(hr) %in% colnames(data) == FALSE){

          stop('User-defined HR name does not match column name of supplied dataset\n')

        }else{

          col_idx <- grep(paste("\\b",toupper(hr),"\\b", sep = ""), names(data))
          colnames(data)[col_idx] <- "HR"
          data <- data[, c(1:3, col_idx, (4:ncol(data))[-col_idx+3])]
          data$HR <- as.numeric(data$HR)

          # Screen for extreme values
          if(data_screen == TRUE){

            # Screening Criteria:
            # - Lowest HR recorded: https://www.guinnessworldrecords.com/world-records/lowest-heart-rate
            # - High HR from the common {220 - age} formula
            data <- data %>%
              dplyr::filter(HR < 220 & HR > 20)

          }
        }
      } else {
        stop('User-defined HR name must be character.\n')
      }

  return(data)
}






########################################################################################################
#                                                                                                      #
#                                        Rate Pressure Product                                         #
#                                                                                                      #
########################################################################################################

rpp_adj <- function(data, rpp = NULL){

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

  return(data)
}







########################################################################################################
#                                                                                                      #
#                                       Mean Arterial Pressure                                         #
#                                                                                                      #
########################################################################################################

map_adj <- function(data, map = NULL){

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

  return(data)
}







########################################################################################################
#                                                                                                      #
#                                         Sleep/Wake Indicator                                         #
#                                                                                                      #
########################################################################################################

wake_adj <- function(data, wake = NULL){

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

  return(data)
}








########################################################################################################
#                                                                                                      #
#                                                Visit #                                               #
#                                                                                                      #
########################################################################################################

visit_adj <- function(data, visit = NULL){

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

          # if( length( unique(data$VISIT) ) > 1){
          #
          #   tmp <- data %>%
          #     group_by(ID, VISIT) %>%
          #     select(SBP, DBP) %>%
          #     dplyr::mutate(
          #       first_SBP = dplyr::first(SBP),
          #       first_DBP = dplyr::first(DBP)
          #     ) %>%
          #     mutate( tmp = SBP - first_SBP ) %>%
          #     select(-first_SBP)
          #   }

        }

  return(data)
}







########################################################################################################
#                                                                                                      #
#                                          DATE/TIME Values                                            #
#                                                                                                      #
########################################################################################################

# dt_fmt = date/time format corresponding to valid lubridate order. Default set to "ymd HMS" but can be
# adjusted based on user's supplied data
# See documentation here: https://lubridate.tidyverse.org/reference/parse_date_time.html

date_time_adj <- function(data, date_time = NULL, dt_fmt = "ymd HMS", ToD_int = NULL){

  Time_of_Day = HOUR = DATE_TIME = NULL
  rm(list = c("Time_of_Day", "HOUR", "DATE_TIME"))

  colnames(data) <- toupper( colnames(data) )

    # Date & Time (DateTime object)
    if(!is.null(date_time)){

          if(toupper(date_time) %in% colnames(data) == FALSE){

            stop('User-defined date_time name does not match column name within supplied dataset\n')

          } else {

              col_idx <- grep(paste("\\b",toupper(date_time),"\\b", sep = ""), names(data))
              colnames(data)[col_idx] <- "DATE_TIME"
              data <- data[, c(col_idx, (1:ncol(data))[-col_idx])]

              #data$DATE_TIME <- as.POSIXct(data$DATE_TIME, tz = "UTC") # coerce to proper time format
              data$DATE_TIME <- lubridate::parse_date_time(data$DATE_TIME, orders = dt_fmt, tz = "UTC")

              # Hour
              data$HOUR <- lubridate::hour(data$DATE_TIME)

            # Time of Day
            if(is.null(ToD_int)){

              # Assume --> Night: 0 - 6, Morning: 6 - 12, Afternoon: 12 - 18, Evening: 18 - 24
              data <- data %>% dplyr::mutate(Time_of_Day =
                                               dplyr::case_when(HOUR >= 0  & HOUR < 6  ~ "Night",
                                                                HOUR >= 6  & HOUR < 12 ~ "Morning",
                                                                HOUR >= 12 & HOUR < 18 ~ "Afternoon",
                                                                HOUR >= 18 & HOUR < 24 ~ "Evening",))

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

                  }else if( all(ToD_int[2:3] == cummax(ToD_int[2:3])) == FALSE ){

                    stop('values within interval must be increasing for the Morning and Afternoon periods (the second and third values in the interval).')

                  }

                  tmp <- c(ToD_int, ToD_int[1])
                  difftmp <- diff(tmp)
                  difftmp[which(difftmp < 0)] <- (24 - tmp[which(difftmp < 0)]) + tmp[which(difftmp < 0) + 1]
                  testval <- sum(difftmp)

                  if(testval > 24){
                    stop('Invalid interval for Time of Day. Ensure that hours do not overlap each other and are consistent within a 24 hour period.')
                  }

                  if( (ToD_int[1] < ToD_int[2]) & (ToD_int[3] < ToD_int[4]) ){

                    data <- data %>% dplyr::mutate(Time_of_Day =
                                                     dplyr::case_when(HOUR >= ToD_int[4] | HOUR < ToD_int[1]  ~ "Night",
                                                                      HOUR >= ToD_int[1] & HOUR < ToD_int[2] ~ "Morning",
                                                                      HOUR >= ToD_int[2] & HOUR < ToD_int[3] ~ "Afternoon",
                                                                      HOUR >= ToD_int[3] & HOUR < ToD_int[4] ~ "Evening"))

                  }else if( (ToD_int[1] > ToD_int[2]) & (ToD_int[3] < ToD_int[4]) ){

                    data <- data %>% dplyr::mutate(Time_of_Day =
                                                     dplyr::case_when(HOUR >= ToD_int[4] & HOUR < ToD_int[1]  ~ "Night",
                                                                    ((HOUR >= ToD_int[1] & HOUR < 24)) | (HOUR < ToD_int[2]) ~ "Morning",
                                                                      HOUR >= ToD_int[2] & HOUR < ToD_int[3] ~ "Afternoon",
                                                                      HOUR >= ToD_int[3] & HOUR < ToD_int[4] ~ "Evening",))

                  }else if( (ToD_int[1] < ToD_int[2]) & (ToD_int[3] > ToD_int[4]) ){

                    data <- data %>% dplyr::mutate(Time_of_Day =
                                                     dplyr::case_when(HOUR >= ToD_int[4]  & HOUR < ToD_int[1]  ~ "Night",
                                                                      HOUR >= ToD_int[1]  & HOUR < ToD_int[2] ~ "Morning",
                                                                      HOUR >= ToD_int[2]  & HOUR < ToD_int[3] ~ "Afternoon",
                                                                    ((HOUR >= ToD_int[3]) & (HOUR < 24)) | (HOUR < ToD_int[4]) ~ "Evening"))

              }
            }

        data <- data %>% dplyr::relocate(HOUR, .after = DATE_TIME)

      }
    }

  return(data)

}








########################################################################################################
#                                                                                                      #
#                                          DATES (Only) Values                                         #
#                                                                                                      #
########################################################################################################

dates_adj <- function(data){

      # DATE column identified in dataset
      if(length(grep("^DATE$", names(data))) == 1){

        # If DATE column found

        # Coerce to Date type
        if( inherits(data[,grep("^DATE$", names(data))], "Date") == FALSE ){

          message('NOTE: DATE column found in data and coerced to as.Date() format.\n')
          data[,grep("^DATE$", names(data))] <- as.Date(data[,grep("^DATE$", names(data))])

        }

        # DATE_TIME column AND identified DATE column present
        if(length(grep("^DATE_TIME$", names(data))) == 1){

          # If applicable, Check that all date values of the identified date column match the date_time values in as.Date format
          if( !all(data[,grep("^DATE$", names(data))] == as.Date(data[,grep("^DATE_TIME$", names(data))])) ){
            data$DATE_OLD <- data$DATE
            data$DATE <- as.Date( lubridate::ymd_hms(data$DATE_TIME, tz = "UTC") )
            warning('User-supplied DATE column does not align with DATE_TIME values.\nCreated additional column DATE_OLD in place of DATE.\nMismatches between rows among DATE_OLD and DATE_TIME columns\n')
            #which(as.Date(data$DATE_TIME) != data$DATE_OLD)
          }

        } # No DATE_TIME column but identified DATE column present --> continue

        col_idx <- grep("^DATE$", names(data))
        colnames(data)[col_idx] <- "DATE"
        data <- data[, c(col_idx, (1:ncol(data))[-col_idx])]


        # DATE column NOT identified in dataset
      } else if(length(grep("^DATE_TIME$", names(data))) == 1){

        # DATE_TIME column is present AND no DATE column found:

        message('NOTE: Created DATE column from DATE_TIME column\n')

        # Create DATE column using as.Date of DATE_TIME
        data$DATE <- as.Date( lubridate::ymd_hms(data$DATE_TIME, tz = "UTC") )

        col_idx <- grep("^DATE$", names(data))
        colnames(data)[col_idx] <- "DATE"
        data <- data[, c(col_idx, (1:ncol(data))[-col_idx])]

      }

  return(data)
}








########################################################################################################
#                                                                                                      #
#                                          Day of the Week (DoW)                                       #
#                                                                                                      #
########################################################################################################

dow_adj <- function(data, DoW = NULL){

      # Coerce all column names are all upper case
      colnames(data) <- toupper( colnames(data) )

      # DoW argument supplied by user
      if(!is.null(DoW)){

        # Ensure that DoW argument matches corresponding column in dataset
        if(toupper(DoW) %in% colnames(data) == FALSE){

          stop('User-defined day of week column name, DoW, does not match column name within supplied dataset\n')

        }

        # Find the index of the supplied DoW column
        col_idx <- grep(paste("\\b",toupper(DoW),"\\b", sep = ""), names(data))
        colnames(data)[col_idx] <- "DAY_OF_WEEK"

        # If all of the unique elements of the User-Supplied Day of Week do not match, run the Day of Week line to create column
        if( !all( toupper(unique(data$DAY_OF_WEEK)) %in% toupper(c("Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat"))) ){

              if( !("DATE_TIME" %in% colnames(data)) & !("DATE" %in% colnames(data)) ){

                stop('Not all unique values from DoW column are valid. (i.e. "Tues" instead of "Tue").
                           \nNo DATE_TIME or DATE column found. Remove DoW argument and re-process dataset.')

              }else{

                # Not all unique DoW values are valid, create another column and warn user that old DoW column was renamed
                warning('Not all unique values from DoW column are valid.
                            \nRenamed user-supplied DoW column to "DAY_OF_WEEK_OLD" and created new column from DATE/DATE_TIME column if available')
                if( !("DATE_TIME" %in% colnames(data)) ){

                    data$DAY_OF_WEEK_OLD <- data$DAY_OF_WEEK
                    data$DAY_OF_WEEK <- ordered(weekdays(as.Date(data$DATE), abbreviate = TRUE),
                                                levels = c("Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat"))

                }else{

                    data$DAY_OF_WEEK_OLD <- data$DAY_OF_WEEK
                    data$DAY_OF_WEEK = ordered(weekdays(as.Date(data$DATE_TIME), abbreviate = TRUE),
                                               levels = c("Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat"))

                }
            }
        }

        # Supplied days of week are correct (i.e. no mis-spellings) and need to be ordered
        data$DAY_OF_WEEK = ordered(data$DAY_OF_WEEK,
                                   levels = c("Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat"))


        # DoW argument NOT supplied by user
      }else{

          # First check if datetime supplied then check for date otherwise nothing
          if( "DATE_TIME" %in% colnames(data) ){

            # Day of Week from DATE_TIME column
            data$DAY_OF_WEEK <- ordered(weekdays(as.Date(data$DATE_TIME), abbreviate = TRUE),
                                        levels = c("Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat"))

          }else if( "DATE" %in% colnames(data) ){

            # Day of Week from DATE column
            data$DAY_OF_WEEK <- ordered(weekdays(as.Date(data$DATE), abbreviate = TRUE),
                                        levels = c("Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat"))

          }

      }

  return(data)

}









########################################################################################################
#                                                                                                      #
#                                   Time Adjustment (Continuous Data)                                  #
#                                                                                                      #
########################################################################################################

## NOTE: Time refers to the particular observation per time elapsed (according to sampling rate)

time_adj <- function(data, time_elap = NULL){

  TIME_ELAPSED = NULL
  rm(list = c("TIME_ELAPSED"))

  # Group
  if(!is.null(time_elap)){

    if(toupper(time_elap) %in% toupper( colnames(data) ) == FALSE){

      stop('User-defined time_elap name does not match column name of supplied dataset\n')

    } else {

      col_idx <- grep(paste("\\b",toupper(time_elap),"\\b", sep = ""), toupper( names(data)) )
      colnames(data)[col_idx] <- "TIME_ELAPSED"
      data <- data[, c(col_idx, (1:ncol(data))[-col_idx])]

    }

  }

  return(data)

}






########################################################################################################
#                                                                                                      #
#                                       End-of-Day Adjustment (EOD)                                    #
#                                                                                                      #
########################################################################################################

eod_adj <- function(data, eod = NULL){


  # Clean global variables
  DATE = DATE_TIME = NULL
  rm(list = c("DATE", "DATE_TIME"))


  if(!is.null(eod)){

      # count of digits - 3 : am | 4 : pm)
      if( floor(log10(abs(eod))) + 1 > 4 ){
        stop('eod argument can only take on either 3 or 4 digits that represent 24-hour time format. \n\ni.e. 130 implies 1:30 AM and 2230 imples 10:30 PM')
      }

      # Check whether user only entered hours (i.e. 22 for 10 PM instead of 2200)
      if(floor(log10(abs(eod))) + 1 <= 2){

        hour_input <- eod
        min_input <- 0
        warning('eod argument should be either 3 or 4 digits that correspond to 24-hour time. Coerced minutes to zero. If only supplying an hour, it is best to add zeros to the end. \n\ni.e. 22 should be represented as 2200 for 10:00 PM')

      }else{

        # Extract hour from eod argument
        hour_input <- ifelse( floor(log10(abs(eod))) + 1 == 3, as.numeric( substr(eod, 1, 1) ),
                              ifelse( floor(log10(abs(eod))) + 1 == 4, as.numeric( substr(eod, 1, 2) ), 0 ) )
        # Extract minute from eod argument
        min_input <- eod %% 100

      }

      # Check for correct hour input
      if(hour_input > 23 | hour_input < 0){
        stop('eod hour argument cannot be less than 0 or greater than 23. Hours of the day range from 0 to 23 where 0 denotes midnight.')
      }

      # Check for correct minute input
      if(min_input > 59 | min_input < 0){
        stop('eod minute argument cannot be less than 0 or exceed 59. Minutes range from 0 to 59.')
      }

      # Ensure that DATE_TIME column is present
      if( "DATE_TIME" %in% toupper(colnames(data)) == FALSE){
        stop('DATE_TIME column not present in supplied dataset\n')
      }

      # Set time zone to UTC --> check ymd_hms format in process helpers to ensure posixct format
      data$DATE_TIME <- lubridate::ymd_hms(data$DATE_TIME, tz = "UTC")


      # Adjust dates according to eod argument
      data <- data %>%
        dplyr::mutate(DATE = dplyr::case_when(

          hour_input < 12 ~ {dplyr::case_when(

            # less than minutes only (i.e. 0023 < 0030 if eod = 0030) & hour is midnight
            lubridate::hour(DATE_TIME) == 0 & lubridate::minute(DATE_TIME) <= min_input ~ as.Date( DATE_TIME - lubridate::days(1) ),

            lubridate::hour(DATE_TIME) == hour_input & lubridate::minute(DATE_TIME) <= min_input ~ as.Date( DATE_TIME - lubridate::days(1) ),

            lubridate::hour(DATE_TIME) < hour_input ~ as.Date( DATE_TIME - lubridate::days(1) ),

            TRUE ~ as.Date(DATE_TIME)

          )},

          hour_input >= 12 ~ {dplyr::case_when(

            lubridate::hour(DATE_TIME) == hour_input & lubridate::minute(DATE_TIME) >= min_input ~ as.Date( DATE_TIME + lubridate::days(1) ),

            lubridate::hour(DATE_TIME) > hour_input ~ as.Date( DATE_TIME + lubridate::days(1) ),

            TRUE ~ as.Date(DATE_TIME)

          )},

          TRUE ~ as.Date(DATE_TIME)

        )) %>%
        dplyr::relocate(DATE, .after = DATE_TIME)


  }

  return(data)

}







########################################################################################################
#                                                                                                      #
#                                                Group                                                 #
#                                                                                                      #
########################################################################################################

## NOTE: Group contains all IDs (ID subset of GROUP)

## Create another group for extra variable (i.e. # cigarettes smoked, salt intake, etc.)?

group_adj <- function(data, group){

  GROUP = NULL
  rm(list = c("GROUP"))

  # Group
  if(!is.null(group)){

    if(toupper(group) %in% toupper( colnames(data) ) == FALSE){

      stop('User-defined Group name does not match column name of supplied dataset\n')

    } else {

      col_idx <- grep(paste("\\b",toupper(group),"\\b", sep = ""), toupper( names(data) ) )
      colnames(data)[col_idx] <- "GROUP"
      data <- data[, c(col_idx, (1:ncol(data))[-col_idx])]

    }

  }else{

    if(!("GROUP" %in% toupper( colnames(data) ))){
      # Create placeholder GROUP column for use with other functions / plots
      data <- data %>% dplyr::mutate(GROUP = 1)
    }

  }

  return(data)

}



########################################################################################################
#                                                                                                      #
#                                                  ID                                                  #
#                                                                                                      #
########################################################################################################

## NOTE: ID is a subset of Group (if group is specified)

id_adj <- function(data, id){

  ID = NULL
  rm(list = c("ID"))

  # ID
  if(!is.null(id)){

      if(toupper(id) %in% toupper( colnames(data) ) == FALSE){

        stop('User-defined ID name does not match column name of supplied dataset\n')

      } else {

        col_idx <- grep(paste("\\b",toupper(id),"\\b", sep = ""), toupper( names(data) ) )
        colnames(data)[col_idx] <- "ID"
        data <- data[, c(col_idx, (1:ncol(data))[-col_idx])]

      }

  }else{

    if(!("ID" %in% toupper( colnames(data) ) )){
      # Create placeholder ID column for use with other functions / plots
      data <- data %>% dplyr::mutate(ID = 1)
    }

  }

  return(data)

}






