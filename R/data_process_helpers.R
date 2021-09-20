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

    if(toupper(ap) %in% colnames(data) == FALSE){

      warning('Could not find user-defined AP argument name in dataset. \ni.e. for example, if user improperly defines ap = "art_pres" but that column name does not exist in the dataset, \nthen there will be no matches for "art_pres". \nCheck spelling of AP argument.\n')

      if(length(grep(paste("\\bAP\\b", sep = ""), names(data))) == 1){

        stop('Fix user-defined argument name for AP. \nNote: A column in the dataset DOES match the name "AP": \nif this is the correct column, indicate as such in function argument. \ni.e. ap = "AP" \n ')

      }

    }else{

      col_idx <- grep(paste("\\b",toupper(ap),"\\b", sep = ""), names(data) )
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

sbp_adj <- function(data, sbp = NULL, data_screen, SUL, SLL){

  SBP = NULL
  rm(list = c("SBP"))

      # Systolic BP (SBP)
      if(is.character(sbp)){

        if(toupper(sbp) %in% colnames(data)  == FALSE){

          warning('Could not find user-defined SBP argument name in dataset. \ni.e. for example, if user improperly defines sbp = "syst" but that column name does not exist in the dataset, \nthen there will be no matches for "syst". \nCheck spelling of SBP argument.\n')

          if(length(grep(paste("\\bSBP\\b", sep = ""), names(data))) == 1){

            stop('Fix user-defined argument name for SBP. \nNote: A column in the dataset DOES match the name "SBP": \nif this is the correct column, indicate as such in function argument. \ni.e. sbp = "SBP" \n ')

          }

        }else{

          col_idx <- grep(paste("\\b",toupper(sbp),"\\b", sep = ""), names(data) )
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
              dplyr::filter(SBP < SUL & SBP > SLL)

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

dbp_adj <- function(data, dbp = NULL, data_screen, DUL, DLL){

  DBP = NULL
  rm(list = c("DBP"))

      # Diastolic BP (DBP)
      if(is.character(dbp)){

        if(toupper(dbp) %in% colnames(data) == FALSE){

          warning('User-defined DBP name does not match column name of supplied dataset. \ni.e. for example, if user improperly defines dbp = "diast" but there is no column name in the dataset, \nthen there will be no matches for "diast". \nCheck spelling of DBP argument.\n')

          if(length(grep(paste("\\bDBP\\b", sep = ""), names(data))) == 1){

            stop('Fix user-defined argument name for DBP. \nNote: A column in the dataset DOES match the name "DBP": \nif this is the correct column, indicate as such in function argument. \ni.e. sbp = "DBP" \n ')

          }
        }else{

          col_idx <- grep(paste("\\b",toupper(dbp),"\\b", sep = ""), names(data) )
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
              # DUL: DBP Upper Limit
              # DLL: DBP Lower Limit
              dplyr::filter(DBP < DUL & DBP > DLL)

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


      PP = DBP = NULL
      rm(list = c("PP", "DBP"))

      # Pulse Pressure
      if(is.null(pp)){

            if(length(grep(paste("\\bPP\\b", sep = ""), names(data))) == 0){

                data$PP <- data$SBP - data$DBP

                message('No PP column found. Automatically generated from SBP and DBP columns.\n')

            }

            col_idx <- grep(paste("\\bPP\\b", sep = ""), names(data))
            colnames(data)[col_idx] <- "PP"

            # Relocate to after DBP column
            data <- data %>% dplyr::relocate(PP, .after = DBP)

            # Convert to numeric
            data$PP <- as.numeric(data$PP)

      }else if(is.character(pp)){ # if character (i.e. by name)

            if(toupper(pp) %in% colnames(data) == FALSE){ # is pp argument found in data colnames

                stop('User-defined PP name does not match column name of supplied dataset\n')

            }else{

                col_idx <- grep(paste("\\b",toupper(pp),"\\b", sep = ""), names(data))
                colnames(data)[col_idx] <- "PP"

                # Relocate to after DBP column
                data <- data %>% dplyr::relocate(PP, .after = DBP)

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

hr_adj <- function(data, hr = NULL, data_screen, HRUL, HRLL){

  HR = DBP = NULL
  rm(list = c("HR", "DBP"))

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
              dplyr::filter(HR < HRUL & HR > HRLL)

          }

        }

      } else if(is.character(hr)){

        if(toupper(hr) %in% colnames(data) == FALSE){

          stop('User-defined HR name does not match column name of supplied dataset\n')

        }else{

          col_idx <- grep(paste("\\b",toupper(hr),"\\b", sep = ""), names(data))
          colnames(data)[col_idx] <- "HR"

          # Relocate to after DBP column
          data <- data %>% dplyr::relocate(HR, .after = DBP)

          # Convert to numeric
          data$HR <- as.numeric(data$HR)

          # Screen for extreme values
          if(data_screen == TRUE){

            # Screening Criteria:
            # - Lowest HR recorded: https://www.guinnessworldrecords.com/world-records/lowest-heart-rate
            # - High HR from the common {220 - age} formula
            data <- data %>%
              dplyr::filter(HR < HRUL & HR > HRLL)

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

    DBP = RPP = NULL
    rm(list = c("DBP", "RPP"))

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

          # Relocate to after DBP column
          data <- data %>% dplyr::relocate(RPP, .after = DBP)

          message('No RPP column found. Automatically generated from SBP and HR columns.\n')
        }else if( (length(grep(paste("\\bRPP\\b", sep = ""), names(data))) == 0) & (length(grep(paste("\\bHR\\b", sep = ""), names(data))) == 0)){

          warning('No RPP column found and no HR column to calculate from')

        }

      }else if( (toupper(rpp) %in% colnames(data)) == FALSE){

        stop('User-defined RPP name does not match column name of supplied dataset\n')

      }else if( (length(grep(paste("\\bHR\\b", sep = ""), names(data))) == 1) & (toupper(rpp) %in% colnames(data)) ){ # HR column is present and in position 4

        col_idx <- grep(paste("\\b",toupper(rpp),"\\b", sep = ""), names(data))
        colnames(data)[col_idx] <- "RPP"

        # Relocate to after DBP column
        data <- data %>% dplyr::relocate(RPP, .after = DBP)

      }else if( (length(grep(paste("\\bHR\\b", sep = ""), names(data))) == 0) & (toupper(rpp) %in% colnames(data)) ){ # HR column is NOT present

        col_idx <- grep(paste("\\b",toupper(rpp),"\\b", sep = ""), names(data))
        colnames(data)[col_idx] <- "RPP"

        # Relocate to after DBP column
        data <- data %>% dplyr::relocate(RPP, .after = DBP)

      }

  return(data)
}







########################################################################################################
#                                                                                                      #
#                                       Mean Arterial Pressure                                         #
#                                                                                                      #
########################################################################################################

map_adj <- function(data, map = NULL){

    MAP = DBP = NULL
    rm(list = c("MAP", "DBP"))

    # Mean Arterial Pressure
    if(is.null(map)){

        if(length(grep(paste("\\bMAP\\b", sep = ""), names(data))) == 0){

          data$MAP <- (1/3) * data$SBP + (2/3) * data$DBP
          message('No MAP column found. Automatically generated from SBP and DBP columns.\n')

          col_idx <- grep(paste("\\bMAP\\b", sep = ""), names(data))
          colnames(data)[col_idx] <- "MAP"

          # Relocate to after DBP column
          data <- data %>% dplyr::relocate(MAP, .after = DBP)

          # Convert to numeric
          data$MAP <- as.numeric(data$MAP)

        }else if(length(grep(paste("\\bMAP\\b", sep = ""), names(data))) == 1){

          warning('MAP column found in data. \nIf this column corresponds to Mean Arterial Pressure, \nuse map = "MAP" in the function argument.\n')

        }
      } else if(toupper(map) %in% colnames(data) == FALSE){

        stop('User-defined MAP name does not match column name of supplied dataset\n')

      } else {

        col_idx <- grep(paste("\\b",toupper(map),"\\b", sep = ""), names(data))
        colnames(data)[col_idx] <- "MAP"

        # Relocate to after DBP column
        data <- data %>% dplyr::relocate(MAP, .after = DBP)

        # Convert to numeric
        data$MAP <- as.numeric(data$MAP)

      }

  return(data)
}







########################################################################################################
#                                                                                                      #
#                                         Sleep/Wake Indicator                                         #
#                                                                                                      #
########################################################################################################

wake_adj <- function(data, wake = NULL, bp_type){

  WAKE = DBP = TIME_OF_DAY = NULL
  rm(list = c("WAKE", "DBP", "TIME_OF_DAY"))

  # Wake (1: Awake | 0: Asleep)
  if(!is.null(wake)){

    if(toupper(wake) %in% colnames(data) == FALSE){

      stop('User-defined WAKE name does not match column name of supplied dataset\n')

    }

    col_idx <- grep(paste("\\b",toupper(wake),"\\b", sep = ""), names(data))
    colnames(data)[col_idx] <- "WAKE"

    if(length(unique(data$WAKE)) > 2){

      warning('Ignoring wake argument. Wake column must only contain 2 unique values corresponding to awake or asleep status. \nTypically, these are denoted as 1 for Awake and 0 for Asleep.\n')

    }else{

      data$WAKE <- as.integer(data$WAKE) # coerce to integers

      # Relocate to after DBP column
      data <- data %>% dplyr::relocate(WAKE, .after = DBP)

    }

    data$WAKE <- as.factor(data$WAKE)

  }else if (("TIME_OF_DAY" %in% colnames(data)) & (toupper(bp_type) == "ABPM")){
    # if there is time of day information, then assign all night to sleep and rest to wake with a message
    message("Absent wake column. Allocating night as sleep.")
    data <- data %>%
      dplyr::mutate(WAKE = ifelse(TIME_OF_DAY == "Night", 0, 1))

    # Relocate to after DBP column
    data <- data %>% dplyr::relocate(WAKE, .after = DBP)

    data$WAKE <- as.factor(data$WAKE)
  }

  return(data)
}








########################################################################################################
#                                                                                                      #
#                                                Visit #                                               #
#                                                                                                      #
########################################################################################################

visit_adj <- function(data, visit = NULL){

    VISIT = DBP = NULL
    rm(list = c("VISIT", "DBP"))

    # Visit
    if(!is.null(visit)){

          if(toupper(visit) %in% colnames(data) == FALSE){

            stop('User-defined VISIT name does not match column name of supplied dataset\n')

          } else {

            col_idx <- grep(paste("\\b",toupper(visit),"\\b", sep = ""), names(data))
            colnames(data)[col_idx] <- "VISIT"

            data$VISIT <- as.integer(data$VISIT)

            # Relocate to after DBP column
            data <- data %>% dplyr::relocate(VISIT, .after = DBP)

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

        data$VISIT <- as.factor(data$VISIT)

      }

  return(data)
}







########################################################################################################
#                                                                                                      #
#                                          DATE/TIME Values                                            #
#                                                                                                      #
########################################################################################################

# Helper function to check the correctness of supplied ToD_int
# Want this to be a vector of length 4 of integer values between 0 and 23 corresponding to breaks between night/morning, morning/afternoon, afternoon/evening, and evening/night
ToD_int_check <- function(ToD_int){
  if (!is.numeric(ToD_int)){
    stop("ToD_int must be an integer vector of length 4.")
  }

  if(!is.vector(ToD_int)){
    warning('ToD_int must be a vector, coerced input to vector.')
    ToD_int <- as.vector(ToD_int)
  }

  if(length(ToD_int) != 4){
    stop('ToD_int must be an integer vector of length 4.')
  }

  # Check that integers are all 0 to 24
  if (any(!(ToD_int %in% c(0:24)))){
    stop('ToD_int must contain integer values corresponding to hours of the day from 0 to 23.')
  }

  # Check that in case 24 is supplied, it is changed to 0
  if (any(ToD_int == 24)){
    warning('One of the supplied hours is 24, which is treated as midnight and coerced to 0.')
    ToD_int[ToD_int == 24] = 0
  }

  # Check for duplicates
  if( any( duplicated( ToD_int ) ) == TRUE ){
    stop('Cannot have overlapping / duplicate values within the ToD interval.')
  }

  # Check if the last one is midnight, bring it back to 24 for internal use ease of sorting
  if (ToD_int[4] == 0){
    ToD_int[4] = 24
  }

  # Check for the right sorting
  if ( any(ToD_int != sort(ToD_int))){
    warning('The supplied ToD_int hours are not in chronological order, and are automatically resorted')
    ToD_int = sort(ToD_int)
  }

  ToD_int
}

# dt_fmt = date/time format corresponding to valid lubridate order. Default set to "ymd HMS" but can be
# adjusted based on user's supplied data
# See documentation here: https://lubridate.tidyverse.org/reference/parse_date_time.html


# Here date_time - column_name for column containing date and time
# ToD_int - optional argument that changes default allocation into morning, afternoon, evening and night
date_time_adj <- function(data, date_time = NULL, dt_fmt = "ymd HMS", ToD_int = NULL, chron_order = FALSE, tz = "UTC"){

  TIME_OF_DAY = DATE = HOUR = DATE_TIME = ID = GROUP = YEAR = MONTH = DAY = SBP = DBP = NULL
  rm(list = c("TIME_OF_DAY", "DATE", "HOUR", "DATE_TIME", "ID", "GROUP", "YEAR", "MONTH", "DAY", "SBP", "DBP"))

  # Date & Time (DateTime object)
  if(!is.null(date_time)){

    if(toupper(date_time) %in% colnames(data) == FALSE){

      stop('User-defined date_time name does not match column name within supplied dataset\n')

    }

    # Find the column corresponding to date_time and rename it DATE_TIME
    col_idx <- grep(paste("\\b",toupper(date_time),"\\b", sep = ""), names(data))
    colnames(data)[col_idx] <- "DATE_TIME"

    # Make that column go first
    data <- data[, c(col_idx, (1:ncol(data))[-col_idx])]

    #data$DATE_TIME <- as.POSIXct(data$DATE_TIME, tz = "UTC") # coerce to proper time format
    data$DATE_TIME <- lubridate::parse_date_time(data$DATE_TIME, orders = dt_fmt, tz = tz)

    # Year
    data$YEAR <- lubridate::year(data$DATE_TIME)

    # Month
    data$MONTH <- lubridate::month(data$DATE_TIME)

    # Day
    data$DAY <- lubridate::day(data$DATE_TIME)

    # Hour
    data$HOUR <- lubridate::hour(data$DATE_TIME)

    # Ordering of date time values
    # Possible groupings for dplyr
    grps = c("ID", "VISIT", "GROUP")

    grps = grps[which(grps %in% colnames(data) == TRUE)]

    ### Chronological Order: Oldest date/times at the top / first ###
    if(chron_order == TRUE){

        #data <- data[order(data$DATE_TIME, decreasing = FALSE),] # old code

        data <- data %>%
                dplyr::group_by_at(dplyr::vars(grps) ) %>%
                dplyr::arrange(DATE_TIME, .by_group = TRUE)

            ### Reverse Chronological Order: Most recent date/times at the top / first ###
    }else{

      #data <- data[order(data$DATE_TIME, decreasing = TRUE),] # old code

      data <- data %>%
                dplyr::group_by_at(dplyr::vars(grps) ) %>%
                dplyr::arrange(dplyr::desc(DATE_TIME), .by_group = TRUE)
    }

    ## Time of Day ##
    if(is.null(ToD_int)){
      # No ToD_int supplied
      # Assume --> Night: 0 - 6, Morning: 6 - 12, Afternoon: 12 - 18, Evening: 18 - 24
      data <- data %>% dplyr::mutate(TIME_OF_DAY =
                                                   dplyr::case_when(HOUR >= 0  & HOUR < 6  ~ "Night",
                                                                    HOUR >= 6  & HOUR < 12 ~ "Morning",
                                                                    HOUR >= 12 & HOUR < 18 ~ "Afternoon",
                                                                    HOUR >= 18 & HOUR < 24 ~ "Evening",))

    }else {
      # Call automated checks on time of day and adjustments,  ToD_int should be a vector that contains the starting hour for Morning, Afternoon, Evening, Night in that order
      ToD_int = ToD_int_check(ToD_int)

      data <- data %>% dplyr::mutate(TIME_OF_DAY =
                                                     dplyr::case_when(HOUR >= ToD_int[4] | HOUR < ToD_int[1]  ~ "Night",
                                                                      HOUR >= ToD_int[1] & HOUR < ToD_int[2] ~ "Morning",
                                                                      HOUR >= ToD_int[2] & HOUR < ToD_int[3] ~ "Afternoon",
                                                                      HOUR >= ToD_int[3] & HOUR < ToD_int[4] ~ "Evening"))
    }

    # Adjust TIME_OF_DAY to be factor with fixed 4 levels
    data$TIME_OF_DAY <- factor(data$TIME_OF_DAY, levels = c("Morning", "Afternoon", "Evening", "Night"))

    # Rearrange columns for consistency
    data <- data %>% dplyr::relocate(ID, GROUP, DATE_TIME, YEAR, MONTH, DAY, HOUR, TIME_OF_DAY, SBP, DBP)

  }


  ### Date only

  # Check whether DATE is in data set
  if("DATE" %in% colnames(data)){

    # Check that the specified Date column is actually of the type: Date
    if(inherits(data$DATE, "Date") == FALSE){

      warning("Original DATE column is not of the type as.Date. Coerced to proper format.")
      data$DATE <- as.Date( data$DATE_TIME )
      data <- data %>% dplyr::relocate(DATE, .after = DBP) # Place after DBP

    }

  }else if("DATE_TIME" %in% colnames(data)){

      # Check to see if all of the Dates in the DATE column match with as.Date(data$DATE_TIME)
      # In this case, check for differences
      if( !all(data$DATE == as.Date(data$DATE_TIME)) ){

          data$DATE_OLD <- data$DATE
          data$DATE <- as.Date( data$DATE_TIME )
          warning('User-supplied DATE column does not align with DATE_TIME values.\nCreated additional column DATE_OLD in place of DATE.\nMismatches between rows among DATE_OLD and DATE_TIME columns\n')

      }else{

          # Ensure that DATE is of the proper format
          data$DATE <- as.Date( data$DATE_TIME )

      }

    data <- data %>% dplyr::relocate(DATE, .after = DATE_TIME) # Place after DATE_TIME

  }

  # Convert tibble back to dataframe
  data <- as.data.frame(data)

  return(data)

}








########################################################################################################
#                                                                                                      #
#                       AGGREGATION of Values by Date/Time Threshold (minutes)                         #
#                                                                                                      #
########################################################################################################

agg_adj <- function(data, bp_type, agg = TRUE, agg_thresh = 3, collap = FALSE, collapse_df = FALSE){


  DATE_TIME = TIME_DIFF = collap2 = collap3 = DATE = HOUR = collap_fin = ID = GROUP = DAY_OF_WEEK = YEAR = MONTH = DAY = TIME_OF_DAY = SBP = DBP = date_first = date_time_first = NULL
  rm(list = c('DATE_TIME', 'TIME_DIFF', 'collap2', 'collap3', 'DATE', 'HOUR', 'collap_fin', 'ID', 'GROUP', 'DAY_OF_WEEK', 'YEAR', 'MONTH', 'DAY', 'TIME_OF_DAY', 'SBP', 'DBP', 'date_first', 'date_time_first'))

  # Ensure that there is a DATE_TIME column
  if(!"DATE_TIME" %in% colnames(data)){
    stop('Cannot aggregate data. No DATE_TIME column found. Make sure to specify in process_data function.')
  }

  # This function cannot currently support Arterial Pressure data
  if(bp_type == "AP"){
    stop('The aggregation feature does not currently support Arterial Pressure (AP) data')
  }


  # Possible groupings for dplyr
  grps = c("ID", "VISIT", "GROUP")

  grps = grps[which(grps %in% colnames(data) == TRUE)]


  # Inclusion variables --> there may be other variables in a user-supplied dataset that the user does not wish to aggregate
  inc_vars <- c("SBP", "DBP", "MAP", "RPP", "HR", "PP", "AP")

  # Aggregation steps
  data <- data %>%

    #dplyr::group_by(ID) %>%
    dplyr::group_by_at(dplyr::vars(grps) ) %>%

    # Create a TIME_DIFF column that takes difference in minutes between rows starting with 1 (last row is 0)
    dplyr::mutate(TIME_DIFF = abs(DATE_TIME - dplyr::lead(DATE_TIME)) ) %>%
    dplyr::relocate(TIME_DIFF, .after = DATE_TIME) %>%

    # Use zero for last row as there is no differencing
    dplyr::mutate(TIME_DIFF = ifelse(dplyr::row_number() == dplyr::n(), 0, TIME_DIFF) ) %>%

    # Create three placeholder columns to properly indicate whether rows should be aggregated together or not
    dplyr::mutate(collap = ifelse(TIME_DIFF < agg_thresh, 1, 0),
                  collap2 = ifelse(dplyr::lag(collap) == 1, 1, 0),
                  collap3 = ifelse(collap == 0 & collap2 == 1, 1, 0) ) %>%
    dplyr::relocate(collap, collap2, collap3, .after = TIME_DIFF) %>%

    dplyr::group_by(DATE, HOUR) %>%

    # Create new column that relies on three placeholder columns to create one final indicator column
    dplyr::mutate(collap_fin = ifelse(collap == 1 | collap2 == 1 | collap3 == 1, 1, 0) ) %>%
    dplyr::ungroup() %>%

    # Create a unique number for any row that has a zero
    dplyr::mutate(collap_fin = ifelse(collap_fin == 0, dplyr::row_number(), collap_fin) ) %>%
    dplyr::group_by(DATE, HOUR, collap_fin) %>%

    # Create unique grouping by DATE by collap_fin column to indicate which consecutive readings to average over
    dplyr::mutate(agg = dplyr::cur_group_id() ) %>%
    dplyr::relocate(collap_fin, agg, .after = collap3) %>%
    dplyr::ungroup() %>%

    # Identify first value in each agg group
    dplyr::group_by(agg) %>%
    dplyr::mutate(date_first = dplyr::first(DATE),
                  date_time_first = dplyr::first(DATE_TIME)) %>%
    #dplyr::relocate(date_first, date_time_first, .after = DATE_TIME) %>%


    # Remove placeholder columns
    dplyr::select(-c("collap", "collap2", "collap3", "collap_fin")) %>%
    #dplyr::group_by(agg) %>%

    # Average all numeric columns over all consecutive readings --> fix to only include processed columns, whichever exist
    # old code --> averages ALL numeric columns not just the ones in process function
    #dplyr::mutate(across(where(is.numeric) & !c(TIME_DIFF), mean)) %>%
    #dplyr::mutate(across(where(is.numeric) & !c(TIME_DIFF) & inc_vars[inc_vars %in% colnames(data)], as.integer))
    dplyr::mutate(dplyr::across(!c(TIME_DIFF) & inc_vars[inc_vars %in% colnames(data)], mean)) %>%
    dplyr::mutate(dplyr::across(!c(TIME_DIFF) & inc_vars[inc_vars %in% colnames(data)], as.integer)) %>%

    # Rearrange columns for consistency
    dplyr::relocate(ID, GROUP, DATE_TIME, TIME_DIFF, DATE, DAY_OF_WEEK, YEAR, MONTH, DAY, HOUR, TIME_OF_DAY, SBP, DBP)


  # Collapse repeating rows
  if(collapse_df == TRUE){

        # data <- data[which(data$DATE_TIME %in% data$date_time_first), c(grps,
        #                                                                         "DATE_TIME",
        #                                                                         "TIME_DIFF",
        #                                                                         "DATE",
        #                                                                         "DAY_OF_WEEK",
        #                                                                         "YEAR",
        #                                                                         "MONTH",
        #                                                                         "DAY",
        #                                                                         "HOUR",
        #                                                                         "TIME_OF_DAY",
        #                                                                         "agg",
        #                                                                         inc_vars[inc_vars %in% colnames(data)])]


        data <- data[which(data$DATE_TIME %in% data$date_time_first),]


  }


  # Remove intermediate helper columns
  data <- data %>%
            dplyr::ungroup() %>%
            dplyr::select(-c(agg, date_first, date_time_first)) %>%


  return(data)

}












########################################################################################################
#                                                                                                      #
#                                          DATES (Only) Values                                         #
#                                                                                                      #
########################################################################################################

#### NOTE: This function and the eod helper funcion must be contained within a conditional in the process_data
#         function as one will overwrite the other. i.e. If eod is specified, ignore dates_adj and vice versa.
#
# dates_adj <- function(data){
#
#       DATE = SBP = DBP = DATE_TIME = NULL
#       rm(list = c("DATE", "SBP", "DBP", "DATE_TIME"))
#
#       # DATE column identified in dataset
#       if(length(grep("^DATE$", names(data))) == 1){
#
#         # If DATE column found
#
#         # # Coerce to Date type
#         # if( inherits(data[,grep("^DATE$", names(data))], "Date") == FALSE ){
#         #
#         #   message('NOTE: DATE column found in data and coerced to as.Date() format.\n')
#         #   data[,grep("^DATE$", names(data))] <- as.Date(data[,grep("^DATE$", names(data))])
#         #
#         # }
#
#
#         # DATE_TIME column AND identified DATE column present
#         if(length(grep("^DATE_TIME$", names(data))) == 1){
#
#           message('NOTE: DATE column found in data and coerced to as.Date() format.\n')
#
#           # Coerce to Date type
#           data$DATE <- as.Date(data$DATE)
#
#           # If applicable, Check that all date values of the identified date column match the date_time values in as.Date format
#           if( !all(data$DATE == as.Date(data$DATE_TIME)) ){
#             data$DATE_OLD <- data$DATE
#             data$DATE <- as.Date( lubridate::ymd_hms(data$DATE_TIME, tz = "UTC") )
#             warning('User-supplied DATE column does not align with DATE_TIME values.\nCreated additional column DATE_OLD in place of DATE.\nMismatches between rows among DATE_OLD and DATE_TIME columns\n')
#             #which(as.Date(data$DATE_TIME) != data$DATE_OLD)
#           }
#
#         } # No DATE_TIME column but identified DATE column present --> continue
#
#         col_idx <- grep("^DATE$", names(data))
#         colnames(data)[col_idx] <- "DATE"
#         data <- data %>% dplyr::relocate(DATE, .after = DBP) # No DATE_TIME so place after DBP
#
#
#         # DATE column NOT identified in dataset
#       } else if(length(grep("^DATE_TIME$", names(data))) == 1){
#
#         # DATE_TIME column is present AND no DATE column found:
#
#         message('NOTE: Created DATE column from DATE_TIME column\n')
#
#         # Create DATE column using as.Date of DATE_TIME
#         data$DATE <- as.Date( lubridate::ymd_hms(data$DATE_TIME, tz = "UTC") )
#
#         col_idx <- grep("^DATE$", names(data))
#         colnames(data)[col_idx] <- "DATE"
#         data <- data %>% dplyr::relocate(DATE, .after = DATE_TIME) # Place after DATE_TIME
#
#       }
#
#   return(data)
# }
#









########################################################################################################
#                                                                                                      #
#                                       End-of-Day Adjustment (EOD)                                    #
#                                                                                                      #
########################################################################################################

eod_adj <- function(data, eod){

  # Clean global variables
  DATE = DATE_TIME = NULL
  rm(list = c("DATE", "DATE_TIME"))

  if ("DATE_TIME" %in% colnames(data) == FALSE){
    warning("The supplied eod argument is ignored as no DATE_TIME column is found")
    return(data)
  }

  # Check that supplied eod is a character string
  if (!is.character(eod)){
    stop('eod must be a character (string) with four characters that represent 24-hour time format.  \n\ni.e. 0130 implies 1:30 AM and 2230 imples 10:30 PM')
  }

  # Check that the string has exactly 4 characters
  if (nchar(eod) != 4){
    stop('eod must be a character (string) with four characters that represent 24-hour time format.  \n\ni.e. 0130 implies 1:30 AM and 2230 imples 10:30 PM')
  }

  # Extract the hour corresponding to time
  hour_input = as.numeric(substr(eod, 1, 2))

  # Extract the minutes corresponding to time
  min_input = as.numeric(substr(eod, 3, 4))

  # Check that both hour and minute are valid
  if(!(hour_input %in% c(0:23)) | !(min_input %in% c(0:59))){
    stop('eod hour argument must be an integer between 0 and 23, eod minutes argument must be an integer between 0 and 59')
  }

  # Adjust dates according to eod argument
  # If 00:00 - no adjustment should happen, Day 1 up to 00:00, Day 2 starting at 00:00
  # If 00:20 - then those extra 20 minutes should count as previous day, Day is stretched up to 00:19, 00:20 and more is Day 2
  # Up to 12:00 - all of these should count as previous day
  # If 23:30 - the minutes from 23:30 to 00:00 should already count as next day
  # If 12:30 - then from 12:30 to 00:00 should count as next day
  # 12:00 - can go either way, currently does the next day

    # If hour_input < 12, then
    data <- data %>%
      dplyr::mutate(DATE = dplyr::case_when(

        hour_input < 12 ~ {dplyr::case_when(

          lubridate::hour(DATE_TIME) == hour_input & lubridate::minute(DATE_TIME) < min_input ~ as.Date( DATE_TIME - lubridate::days(1) ),

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

  return(data)

}







########################################################################################################
#                                                                                                      #
#                                          Day of the Week (DoW)                                       #
#                                                                                                      #
########################################################################################################

dow_adj <- function(data, DoW = NULL){

      DAY_OF_WEEK = DATE = DATE_TIME = NULL
      rm(list = c("DAY_OF_WEEK", "DATE", "DATE_TIME"))

      # Coerce all column names are all upper case
      # colnames(data) <- toupper( colnames(data) )

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

        # First check if DATE supplied (so that DAY_OF_WEEK can go after it),
        # then check for DATE_TIME (if no DATE),
        # otherwise do nothing

        if( "DATE" %in% colnames(data) ){

                # Day of Week from DATE column
                data$DAY_OF_WEEK <- ordered(weekdays(as.Date(data$DATE), abbreviate = TRUE),
                                            levels = c("Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat"))

                # Relocate to after DATE column
                data <- data %>% dplyr::relocate(DAY_OF_WEEK, .after = DATE)


        }else if( "DATE_TIME" %in% colnames(data) ){

                # Day of Week from DATE_TIME column
                data$DAY_OF_WEEK <- ordered(weekdays(as.Date(data$DATE_TIME), abbreviate = TRUE),
                                            levels = c("Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat"))

                # Relocate to after DATE_TIME column
                data <- data %>% dplyr::relocate(DAY_OF_WEEK, .after = DATE_TIME)


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

    if(toupper(time_elap) %in%  colnames(data)  == FALSE){

      stop('User-defined time_elap name does not match column name of supplied dataset\n')

    } else {

      col_idx <- grep(paste("\\b",toupper(time_elap),"\\b", sep = ""),  names(data) )
      colnames(data)[col_idx] <- "TIME_ELAPSED"
      data <- data[, c(col_idx, (1:ncol(data))[-col_idx])]

    }

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

group_adj <- function(data, group = NULL){

  GROUP = ID = NULL
  rm(list = c("GROUP", "ID"))

  # Group
  if(!is.null(group)){

    if(toupper(group) %in% colnames(data)  == FALSE){

      stop('User-defined Group name does not match column name of supplied dataset\n')

    } else {

      col_idx <- grep(paste("\\b",toupper(group),"\\b", sep = ""), names(data) )
      colnames(data)[col_idx] <- "GROUP"
    }

  }else{

    if(!("GROUP" %in% colnames(data))){
      # Create placeholder GROUP column for use with other functions / plots
      data <- data %>% dplyr::mutate(GROUP = 1)
    }

  }

  # Relocate to after ID column --> Make sure this function (group_adj) comes after id_adj in process_data
  data <- data %>% dplyr::relocate(GROUP, .after = ID)

  data$GROUP <- as.factor(data$GROUP)

  return(data)

}



########################################################################################################
#                                                                                                      #
#                                                  ID                                                  #
#                                                                                                      #
########################################################################################################

## NOTE: ID is a subset of Group (if group is specified)

id_adj <- function(data, id = NULL){

  ID = NULL
  rm(list = c("ID"))

  # ID
  if(!is.null(id)){

      if(toupper(id) %in% colnames(data) == FALSE){

        stop('User-defined ID name does not match column name of supplied dataset\n')

      } else {

        col_idx <- grep(paste("\\b",toupper(id),"\\b", sep = ""), names(data) )
        colnames(data)[col_idx] <- "ID"

      }

  }else{

    if(!("ID" %in%  colnames(data)  )){
      # Create placeholder ID column for use with other functions / plots
      data <- data %>% dplyr::mutate(ID = 1)
    }

  }

  # Relocate to first column
  data <- data %>% dplyr::relocate(ID)

  # Convert to factor
  data$ID <- as.factor(data$ID)

  return(data)

}

