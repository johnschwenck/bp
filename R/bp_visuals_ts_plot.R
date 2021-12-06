#' Blood Pressure Time Series Plots
#'
#' @param data User-supplied data set containing blood pressure data. Must
#' contain a Systolic blood pressure (SBP), Diastolic blood pressure (DBP)
#' and an ID column. Data must also have either a DATE_TIME or DATE column,
#' unless an index column is specified for the x axis. An index column trumps
#' DATE_TIME and DATE if specified.
#'
#' @param index An optional user-specified column denoting x-axis values (other
#' than DATE_TIME or DATE columns). \code{index} argument must be a character denoting a
#' column in the supplied data. This argument is useful in the event the
#' data has no corresponding date/time value, but rather an index of values
#' such as (reading #1, #2, #3, etc. or office visit #1, #2, etc. as examples)
#' If NULL, the function will look for DATE_TIME or DATE columns to reference.
#' If \code{index} is character, it will be coerced to factor.
#'
#' @param subj Optional argument. Allows the user to specify and subset specific subjects
#' from the \code{ID} column of the supplied data set. The \code{subj} argument can be a single
#' value or a vector of elements. The input type should be character, but the function will
#' comply with integers so long as they are all present in the \code{ID} column of the data.
#'
#' @param first_hour Optional argument denoting a value corresponding to the first hour of the
#' x-axis for the hour plots. Only applicable to data sets that contain a DATE_TIME column.
#' It is often easier to visualize a BP time series not from 0 - 23 hours but rather an order
#' that begins or ends with waking up such as \code{first_hour = 6} which will sequence the
#' hours from 6am (6, 7, ..., 23, 0, ..., 4, 5). The default value for \code{first_hour} is
#' set at 0
#'
#' @param rotate_xlab An optional logical argument to rotate the x axis labels 90 degrees. The
#' default value is set to FALSE.
#'
#' @param wrap_var An optional character argument indicating a column by which to "wrap" the data.
#' This function utilizes ggplot2's \code{facet_wrap} function to split plots according to some
#' extraneous variable (such as gender, smoking status, awake/asleep, etc.).
#'
#' @param wrap_row An optional argument specifying how many rows to wrap the plots if \code{wrap_var}
#' is specified.
#'
#' @param wrap_col An optional argument specifying how many columnss to wrap the plots if \code{wrap_var}
#' is specified.
#'
#' @param method (ggplot2 plotting arguments) Smoothing method (function) to use. Default is NULL,
#' but also accepts a character vector "lm", "glm", "gam", "loess". NULL implies that the
#' smoothing method will be chosen automatically based on the size of the largest group.
#'
#' See \url{https://ggplot2.tidyverse.org/reference/geom_smooth.html} for more details.
#'
#' @param formula (ggplot2 plotting arguments) Formula to use in smoothing function. Default is NULL
#' implying y ~ x for fewer than 1,000 observations and y ~ x(x, bs = "cs") otherwise.
#'
#' See \url{https://ggplot2.tidyverse.org/reference/geom_smooth.html} for more details.
#'
#' @return If the data does not contain a DATE_TIME column, a single list will be
#' returned with the time-dependent plots for each subject ID. If the data does contain a DATE_TIME
#' column (and index is not specified), a list of two lists will be returned for each subject ID:
#' one corresponding to the time-dependent plots (according to the DATE_TIME values), and another
#' plot corresponding to the HOUR plots which show repeated measurements of BP values throughout
#' a 24-hour period. The index of the output therefore corresponds to whether there is only the
#' time-dependent plot type (the former situation) or there are both time-dependent and hourly
#' plot types (the latter situation).
#'
#'
#' @export
#'
#' @examples
#' # Pregnancy Data Set
#'
#' # bp_preg requires the use of the index argument since there are no DATE or
#' # DATE_TIME columns available
#' data_preg <- bp::bp_preg
#' data_preg$Time_Elapsed <- factor(data_preg$Time_Elapsed,
#'      levels = c("Booking", "0", "30", "60", "90", "120", "150", "180", "210", "240"))
#'
#' bp::bp_ts_plots(data_preg, index = 'time_elapsed', subj = 1:3)
#'
#'
#' # JHS Data Set
#'
#' # bp_jhs returns two lists since there is a DATE_TIME column: one for
#' # DATE_TIME and one for HOUR
#' data_jhs <- bp::process_data(bp::bp_jhs,
#'                                     sbp = 'sys.mmhg.',
#'                                     dbp = 'dias.mmhg.',
#'                                      hr = 'pulse.bpm.',
#'                               date_time = 'datetime')
#'
#' bp::bp_ts_plots(data_jhs)
#'
#'
#' # HYPNOS Data Set
#'
#' # bp_hypnos wraps the plots by the visit # since each subject was recorded over
#' # the course of two office visits
#' data_hypnos <- bp::process_data(bp::bp_hypnos,
#'                                           sbp = 'syst',
#'                                           dbp = 'diast',
#'                                     date_time = 'date.time')
#'
#' bp::bp_ts_plots(data_hypnos, wrap_var = 'visit', subj = '70435')
bp_ts_plots <- function(data, index = NULL, subj = NULL, first_hour = 0, rotate_xlab = FALSE, wrap_var = NULL, wrap_row = NULL, wrap_col = NULL, method = NULL, formula = NULL){

  # All data sets must have ID, SBP, DBP
  # Data must have either DATE or DATE_TIME, or else an 'index' column if not
  # Data sets may also include WAKE, GROUP, and VISIT if available

  ID = SBP = DBP = DATE_TIME = DATE = HOUR = NULL
  rm(list = c('ID', 'SBP', 'DBP', 'DATE_TIME', 'DATE', 'HOUR'))

  # Coerce data to dataframe
  data <- as.data.frame(data)

  # Capitalize all columns to avoid any spelling errors
  colnames(data) <- toupper(colnames(data))

  # Check that there are more than 1 BP value
  if(nrow(data) <= 1){
    stop('Data must contain more than one BP value / row')
  }

  # Ensure ID column is present
  if(!"ID" %in% colnames(data)){
    stop('No ID column found in data. Ensure that you use process_data first.')
  }

  # Subset subjects if subj argument is supplied
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

  # Create list of all unique subjects
  ids <- unique(data$ID)
  n_ids <- length(ids)
  id_tab <- cbind(ids, seq(1:n_ids))

  # Ensure that either SBP or DBP (or ideally both) are supplied
  if( ! any( c("SBP", "DBP") %in% colnames(data) ) ){
    stop('Neither SBP nor DBP columns were found in data. Ensure that you use process_data first.')
  }

  # Ensure that either DATE or DATE_TIME is supplied
  if( ! any( c("DATE_TIME", "DATE") %in% colnames(data)) & is.null(index) ){
    stop('Neither DATE nor DATE_TIME columns were found in data. Ensure that you use process_data first and specify an index column if necessary.')
  }


  ##### INDEX #####

  # Compatibility check for index argument to ensure that it is a valid column
  #    **NOTE** index trumps DATE or DATE_TIME in terms of the X-axis of the plot.
  #    Although redundant, index = 'DATE_TIME' should comply
  #    i.e. if supplied, ignore DATE / DATE_TIME

  # index is supplied and is not "DATE_TIME" or "DATE"
  if( (!is.null(index)) && (index != "DATE_TIME") && (index != "DATE") ){ # Need && not & for short-circuit evaluation


        # Capitalize because all data frame columns are capitalized
        index <- toupper(index)

        if( !index %in% colnames(data) ){
          stop('Supplied index argument must be a column within the supplied data. Index column not found.')
        }

        # Check Index input
        if( is.character( data[, grep(index, colnames(data))] ) ){

              # Convert to factor if character
              data[, grep(index, colnames(data))] <- as.factor( data[, grep(index, colnames(data))] )
              message('Converted supplied character index column to factor.')

        }


        # Check wrap_var input
        if( !is.null(wrap_var) ){

          # Capitalize because all data frame columns are capitalized
          wrap_var <- toupper(wrap_var)

            if( is.character( data[, grep(wrap_var, colnames(data))] ) ){

                # Convert to factor if character
                data[, grep(wrap_var, colnames(data))] <- as.factor( data[, grep(wrap_var, colnames(data))] )
                message('Converted supplied character wrap_var column to factor.')
            }
        }


        # Initialize list for plots
        p <- list()

        # Loop through IDs to create plot for each
        for(i in ids){

            tmp <- data %>% dplyr::filter(ID == i)

            # Remove rows that have NA for either SBP, DBP, or ID
            colnames(tmp) <- toupper(colnames(tmp))
            tmp <- tmp[ complete.cases( tmp[c('SBP', 'DBP', 'ID')] ), ]

            # Index plots
            p[[i]] <- ggplot2::ggplot(tmp, ggplot2::aes_string(x = index)) +

                           # Plot the SBP values in blue
                           ggplot2::geom_point(ggplot2::aes(y = SBP), col = 'blue') +

                           # If there is enough data, plot a smoothed non-parametric LOESS line for SBP
                           ggplot2::geom_smooth(ggplot2::aes(group = "", y = SBP), formula = formula, method = method, col = 'blue') +

                           # Plot the DBP values in red
                           ggplot2::geom_point(ggplot2::aes(y = DBP), col = 'red') +

                           # If there is enough data, plot a smoothed non-parametric LOESS line for DBP
                           ggplot2::geom_smooth(ggplot2::aes(group = "", y = DBP), formula = formula, method = method, col = 'red') +

                           # Rotate x axis writing if rotate_xlab = TRUE
                           {if (rotate_xlab == TRUE) ggplot2::theme(axis.text.x = ggplot2::element_text(angle=90)) } +

                           # X Axis Label based on whatever is selected for index
                           ggplot2::xlab(colnames(tmp)[grep(index, colnames(tmp))]) +

                           # Y Axis Label corresponds to Blood Pressure for both SBP and DBP
                           ggplot2::ylab("Blood Pressure (mmHg)") +

                           # Plot Title - Dynamic for multiple subjects if applicable
                           ggplot2::ggtitle( paste("BP Profile for Subject: ", i, sep = "") ) +

                           # Wrap by number of input variable
                           {if (!is.null(wrap_var)) ggplot2::facet_wrap(as.vector(paste("~", wrap_var)), scales = "free_x", nrow = wrap_row, ncol = wrap_col)} # wrap_var, wrap_row & wrap_col

        }

        # Remove empty elements from the list if applicable
        p[sapply(p, is.null)] <- NULL

        return(idx_plots = p)


  }else if( "DATE_TIME" %in% colnames(data) ){

      ##### DATE_TIME #####

      # If DATE_TIME supplied, make sure it is proper format

        # Check whether DATE_TIME column is POSIXct
        if(lubridate::is.POSIXct(data$DATE_TIME) == FALSE){
              # Convert to as.POSIXct if not
              data$DATE_TIME <- as.POSIXct(data$DATE_TIME)
        }

        # Check wrap_var input
        if( !is.null(wrap_var) ){

          # Capitalize because all data frame columns are capitalized
          wrap_var <- toupper(wrap_var)

          if( is.character( data[, grep(wrap_var, colnames(data))] ) ){

            # Convert to factor if character
            data[, grep(wrap_var, colnames(data))] <- as.factor( data[, grep(wrap_var, colnames(data))] )
            message('Converted supplied character wrap_var column to factor.')
          }
        }


        # Check that there is an HOUR column which corresponds to DATE_TIME and if not, create one
        if("HOUR" %in% colnames(data)){

              # HOUR column of wrong format
              if( identical( as.factor(lubridate::hour(data$DATE_TIME)), as.factor(data$HOUR)) == FALSE ){
                    warning('HOUR column in dataset does not correspond to the hours associated with the DATE_TIME column.\nOverwrote HOUR column with proper format.')
                    data$HOUR <- lubridate::hour(data$DATE_TIME)
              }

        }else{

          message('Created HOUR column from DATE_TIME column')

          # NO HOUR column present
          data$HOUR <- lubridate::hour(data$DATE_TIME)

        }

        # Sort data in increasing (chronological) order (oldest values first)
        data <- data %>% dplyr::arrange(DATE_TIME)


        # If first_hour is supplied in function argument
        if(!is.null(first_hour)){

              # Check that first_hour input is not <0 nor >23
              if(first_hour > 23 | first_hour < 0){
                stop('first_hour input must be between 0 - 23')
              }

              # Create index of hour values based on first_hour argument
              if(first_hour == 0){

                time_index <- seq(0,23)

              }else if(first_hour == 23){

                time_index <- c(23, seq(0,22))

              }else{
                remain <- 24 - first_hour
                last_hour <- 23 - remain
                time_index <- c( seq(first_hour, 23), seq(0, last_hour) )
              }

        }else{

              time_index <- seq(0, 23, 1)

        }

        # Order HOUR in increasing order
        data$HOUR <- factor(data$HOUR, levels = as.character(time_index), ordered = T)

        # Since dplyr's arrange function was used to order the data in chronological order,
        # I think this is unnecessary, but if I run into issues I will leave this here in case:
        #row.names(data) <- NULL


        # Initialize lists for plots (p for DATE_TIME, q for hour)
        p <- list() # For DATE_TIME plot
        q <- list() # For HOUR plot

        # Loop through IDs to create plot for each
        for(i in ids){

          tmp <- data %>% dplyr::filter(ID == i)

          # Remove rows that have NA for either SBP, DBP, or ID
          colnames(tmp) <- toupper(colnames(tmp))
          tmp <- tmp[ complete.cases( tmp[c('SBP', 'DBP', 'ID')] ), ]

          # DATE_TIME plots
          p[[i]] <- ggplot2::ggplot(tmp, ggplot2::aes(x = DATE_TIME)) +

                          # Plot the SBP values in blue
                          ggplot2::geom_point(ggplot2::aes(y = SBP), col = 'blue') +

                          # If there is enough data, plot a smoothed non-parametric LOESS line for SBP
                          ggplot2::geom_smooth(ggplot2::aes(group = "", y = SBP), formula = formula, method = method, col = 'blue') +

                          # Plot the DBP values in red
                          ggplot2::geom_point(ggplot2::aes(y = DBP), col = 'red') +

                          # If there is enough data, plot a smoothed non-parametric LOESS line for DBP
                          ggplot2::geom_smooth(ggplot2::aes(group = "", y = DBP), formula = formula, method = method, col = 'red') +
                          #ggplot2::geom_smooth(ggplot2::aes(x = index, y = DBP), method = method, col = 'red') +

                          # Rotate x axis writing if rotate_xlab = TRUE
                          {if (rotate_xlab == TRUE) ggplot2::theme(axis.text.x = ggplot2::element_text(angle=90)) } +

                          # X Axis Label based on whatever is selected for index
                          ggplot2::xlab(colnames(tmp)[grep("DATE_TIME", colnames(tmp))]) +

                          # Y Axis Label corresponds to Blood Pressure for both SBP and DBP
                          ggplot2::ylab("Blood Pressure (mmHg)") +

                          # Plot Title - Dynamic for multiple subjects if applicable
                          ggplot2::ggtitle( paste("BP Profile for Subject: ", i, sep = "") ) +

                          # Wrap by number of input variable
                          {if (!is.null(wrap_var)) ggplot2::facet_wrap(as.vector(paste("~", wrap_var)), scales = "free_x", nrow = wrap_row, ncol = wrap_col)} # wrap_var, wrap_row & wrap_col


          # HOUR plots
          q[[i]] <-   ggplot2::ggplot(tmp, ggplot2::aes(x = HOUR) ) +

                          # Plot the SBP values in blue
                          ggplot2::geom_point(ggplot2::aes(y = SBP), col = 'blue') +

                          # Plot a smooth non-parametric LOESS curve for SBP
                          ggplot2::geom_smooth(ggplot2::aes(group = "", y = SBP), formula = formula, method = method, col = 'blue')  +

                          # Plot the DBP values in red
                          ggplot2::geom_point(ggplot2::aes(y = DBP), col = 'red') +

                          # Plot a smooth non-parametric LOESS curve for DBP
                          ggplot2::geom_smooth(ggplot2::aes(group = "", y = DBP), formula = formula, method = method, col = 'red')  +

                          # Rotate x axis writing if rotate_xlab = TRUE
                          #{if (rotate_xlab == TRUE) ggplot2::theme(axis.text.x = ggplot2::element_text(angle=90)) } +

                          # X Axis Label based on whatever is selected for index
                          ggplot2::xlab(colnames(tmp)[grep("HOUR", colnames(tmp))]) +

                          # Y Axis Label corresponds to Blood Pressure for both SBP and DBP
                          ggplot2::ylab("Blood Pressure (mmHg)") +

                          # Plot Title - Dynamic for multiple subjects if applicable
                          ggplot2::ggtitle( paste("BP Profile for Subject: ", i, sep = "") ) #+

                          # Wrap by number of input variable
                          #{if (!is.null(wrap_var)) ggplot2::facet_wrap(as.vector(paste("~", wrap_var)), scales = "free_x", nrow = wrap_row, ncol = wrap_col)} # wrap_var, wrap_row & wrap_col



        }

        # Remove empty elements from the lists if applicable
        p[sapply(p, is.null)] <- NULL
        q[sapply(q, is.null)] <- NULL

#
#               if( (n_ids > 1) == TRUE ){
#                 plots <- q + ggplot2::facet_wrap(~ ID)
#               }else{
#                 plots <- q
#               }

        return(list( dt_plots = p, hour_plots = q) )


  }else if( "DATE" %in% colnames(data) ){

    ##### DATE #####

    # Check whether DATE column is Date format
    if(lubridate::is.Date(data$DATE) == FALSE){
          # Convert to as.Date if not
          data$DATE <- as.Date(data$DATE)
    }


    # Check wrap_var input
    if( !is.null(wrap_var) ){

      # Capitalize because all data frame columns are capitalized
      wrap_var <- toupper(wrap_var)

      if( is.character( data[, grep(wrap_var, colnames(data))] ) ){

        # Convert to factor if character
        data[, grep(wrap_var, colnames(data))] <- as.factor( data[, grep(wrap_var, colnames(data))] )
        message('Converted supplied character wrap_var column to factor.')
      }
    }

    # Sort data in increasing (chronological) order (oldest values first)
    data <- data %>% dplyr::arrange(DATE)


    # Initialize list for DATE plot
    p <- list() # For DATE plot


    # Loop through IDs to create plot for each
    for(i in ids){

          tmp <- data %>% dplyr::filter(ID == i)

          # Remove rows that have NA for either SBP, DBP, or ID
          colnames(tmp) <- toupper(colnames(tmp))
          tmp <- tmp[ complete.cases( tmp[c('SBP', 'DBP', 'ID')] ), ]

          # DATE_TIME plots
          p[[i]] <- ggplot2::ggplot(tmp, ggplot2::aes(x = DATE)) +

            # Plot the SBP values in blue
            ggplot2::geom_point(ggplot2::aes(y = SBP), col = 'blue') +

            # If there is enough data, plot a smoothed non-parametric LOESS line for SBP
            ggplot2::geom_smooth(ggplot2::aes(group = "", y = SBP), formula = formula, method = method, col = 'blue') +

            # Plot the DBP values in red
            ggplot2::geom_point(ggplot2::aes(y = DBP), col = 'red') +

            # If there is enough data, plot a smoothed non-parametric LOESS line for DBP
            ggplot2::geom_smooth(ggplot2::aes(group = "", y = DBP), formula = formula, method = method, col = 'red') +
            #ggplot2::geom_smooth(ggplot2::aes(x = index, y = DBP), method = method, col = 'red') +

            # Rotate x axis writing if rotate_xlab = TRUE
            {if (rotate_xlab == TRUE) ggplot2::theme(axis.text.x = ggplot2::element_text(angle=90)) } +

            # X Axis Label based on whatever is selected for index
            ggplot2::xlab(colnames(tmp)[grep("DATE", colnames(tmp))]) +

            # Y Axis Label corresponds to Blood Pressure for both SBP and DBP
            ggplot2::ylab("Blood Pressure (mmHg)") +

            # Plot Title - Dynamic for multiple subjects if applicable
            ggplot2::ggtitle( paste("BP Profile for Subject: ", i, sep = "") ) +

            # Wrap by number of input variable
            {if (!is.null(wrap_var)) ggplot2::facet_wrap(as.vector(paste("~", wrap_var)), scales = "free_x", nrow = wrap_row, ncol = wrap_col)} # wrap_var, wrap_row & wrap_col

    }

        return(date_plots = p)

  }else{

    stop('Could not generate time series plots. Check that all input is correct and that the data has been processed using process_data()')

  }


}
