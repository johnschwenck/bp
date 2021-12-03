#' Plot of Dipping Classifications
#'
#' @description
#' Graphical visualization of the dip_calc output for categories of dipping percentages.
#'
#' NOTE: Any reference to "sleep" in the bp package refers to an individual's nocturnal period;
#' "sleep" is used in an informal sense for intuitive purposes. Technically, from a clinical perspective,
#' indication of sleep is currently obtained through means of EEG or other highly specialized equipment.
#' For all intents and purposes, sleep in the context of this package refers to actigraphy-inferred
#' nocturnal periods of rest.
#'
#' @param data
#' User-supplied data set that must contain \code{SBP}, \code{DBP}, and either \code{DATE_TIME} or \code{WAKE}
#' columns in order to distinguish between sleep and awake
#'
#' In the event of non-ABPM data (i.e. a data set without a corresponding \code{WAKE} column), then a
#' \code{DATE_TIME} column \strong{must} be present in order to denote which times correspond to sleep and which
#' times correspond to awake.
#'
#' @param subj
#' Optional argument. Allows the user to specify and subset specific subjects
#' from the \code{ID} column of the supplied data set. The \code{subj} argument can be a single
#' value or a vector of elements. The input type should be character, but the function will
#' comply with integers so long as they are all present in the \code{ID} column of the data.
#'
#' @param dip_thresh
#' Default threshold for normal "Dipping" set to 0.10 (i.e. 10\%). This value represents the maximum
#' percentage that BP can fall during sleep and be characterized as "Normal" nocturnal decline (dipping).
#' Specifically, this category includes all dips between 0\% and this value.
#'
#' @param extreme_thresh
#' Default threshold for "Extreme Dipping" set to 0.20 (i.e. 20\%). This value represents the maximum
#' percentage that BP can fall during sleep and be characterized as "Extreme" nocturnal decline (dipping).
#' Specifically, this category includes all dips between the Normal dipping threshold and this value.
#'
#' NOTE: dip_thresh cannot exceed extreme_thresh.
#'
#' @param thresh_mult
#' Optional argument that serves as a multiplier by which to expand plot sizing for X and Y axis. Default
#' set to 2.
#'
#' @param sleep_start_end
#' Optional User-supplied manual override to adjust sleep interval indicating indicate start and end time
#' corresponding to the sleep interval of interest. Must only contain 2 values and must be 24-hour denoted integers
#'
#' Example: \code{sleep_start_end = c(22,5)} indicates a sleep period from 10pm - 5am.
#'
#' @references
#' Okhubo, T., Imai, Y., Tsuji, K., Nagai, K., Watanabe, N., Minami, J., Kato, J., Kikuchi, N., Nishiyama, A.,
#' Aihara, A., Sekino, M., Satoh, H., and Hisamichi, S. (1997). Relation Between Nocturnal Decline in Blood
#' Pressure and Mortality: The Ohasama Study,
#' \emph{American Journal of Hypertension} \strong{10(11)}, 1201--1207,
#' \doi{10.1016/S0895-7061(97)00274-4}.
#'
#' @return
#' A scatter plot of all dipping percentage values layered on top of the category plot outlined in
#' Okhubo et al. (1995). dip_thresh and extreme_thresh denote the cutoffs for the Normal and Extreme
#' dipping categories. Any dips below zero are denoted as Inverted (or Reverse) dipping.
#'
#' The default plot categories are as follows:
#' \itemize{
#'    \item{\emph{INV}: Inverted (Reverse) Dipper - no nocturnal decline (greater or equal to 0\%)}
#'    \item{\emph{ND}: Non-Dipper - a nocturnal decline between 0 - 10\%}
#'    \item{\emph{DIP}: Dipper - a nocturnal decline between 10\% and the extreme dipping \% (20\%)}
#'    \item{\emph{ED}: Extreme Dipper - a nocturnal decline exceeding 20\%}
#' }
#'
#' @export
#'
#' @examples
#' hypnos_proc <- process_data(bp::bp_hypnos,
#'                                sbp = "syst",
#'                                dbp = "DIAST",
#'                                date_time = "date.time",
#'                                id = "id",
#'                                wake = "wake",
#'                                visit = "visit")
#'
#' dip_class_plot(hypnos_proc)
dip_class_plot <- function(data, subj = NULL, dip_thresh = 0.1, extreme_thresh = 0.2, thresh_mult = 2, sleep_start_end = NULL){

  # Initialize variables for dplyr
  ID = dip_sys = dip_dias = VISIT = NULL
  rm(list = c("ID", "dip_sys", "dip_dias", "VISIT"))

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

  # Remove NA values from data
  data <- data %>% stats::na.omit()

  # Check compatibility of dip_thresh and extreme_thresh if not default (user-specified)
  if(dip_thresh != 0.10 | extreme_thresh != 0.20){

    # Identical is illogical, throw an error
    if(dip_thresh == extreme_thresh){
      stop('Threshold for "Dipper" status and "Extreme Dipper" status cannot be identical.
             \nextreme_thresh must be larger than dip_thresh and both must be positive values.')
    }

    # Cannot have negative thresholds
    if(dip_thresh < 0 | extreme_thresh < 0){

      stop('Both dip_thresh and extreme_thresh must be positive values')

      # Assuming positive values (i.e. not negative or zero)
    }else{

      # Ensure that extreme_thresh is larger than dip_thresh (if equal, error will be thrown from above)
      if(dip_thresh > extreme_thresh){
        stop('dip_thresh cannot exceed extreme_thresh and must both be positive values.')
      }

    }
  }

  # Run dip_calc to obtain output and extract systolic and diastolic results
  out <- dip_calc(data, sleep_start_end = sleep_start_end, dip_thresh = dip_thresh, extreme_thresh = extreme_thresh)[[2]]
  sbp_dip <- out['dip_sys']
  dbp_dip <- out['dip_dias']

  # +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ #

  # Maximum X axis value
  x_max <- max( (sbp_dip * 100) * thresh_mult , (extreme_thresh * 100) * thresh_mult )

  # Maximum Y axis value
  y_max <- max( (dbp_dip * 100) * thresh_mult , (extreme_thresh * 100) * thresh_mult )

  # Minimum X axis value
  x_min <- min( (sbp_dip * 100) * thresh_mult , -10 )

  # Minimum Y axis value
  y_min <- min( (dbp_dip * 100) * thresh_mult , -10 )

  # Vertical adjustment
  vjust <- 2

  # Horizontal adjustment
  hjust <- 0.35

  # +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ #


  # Create plot
  ggplot2::ggplot(out, ggplot2::aes(x = dip_sys*100, y = dip_dias*100)) +

    # Create initial background
    ggplot2::annotate("rect", xmin = x_min, xmax = x_max, ymin = y_min, ymax = y_max, fill = 'gray90') +

    # Add Extreme Dipping box
    ggplot2::annotate("rect", xmin = (extreme_thresh * 100), xmax = x_max, ymin = (extreme_thresh * 100), ymax = y_max, fill = 'gray70') +

    # Add dashed line for Normal Dipping %
    ggplot2::geom_segment(ggplot2::aes(x = extreme_thresh * 100, y = y_min, xend = extreme_thresh * 100, yend = extreme_thresh * 100), linetype = 'dashed') +
    ggplot2::geom_segment(ggplot2::aes(x = x_min, y = extreme_thresh * 100, xend = extreme_thresh * 100, yend = extreme_thresh * 100), linetype = 'dashed') +

    # Add Non-Dipping box
    ggplot2::annotate("rect", xmin = x_min, xmax = dip_thresh * 100, ymin = y_min, ymax = dip_thresh * 100, fill = 'gray50') +

    # Add Inverted-Dipping box
    ggplot2::annotate("rect", xmin = x_min, xmax = 0, ymin = y_min, ymax = 0, fill = 'gray30') +


        ### Add data values

    # If VISIT column present
        {if ( "VISIT" %in% toupper(colnames(data)) ) ggplot2::geom_point(size = 3, ggplot2::aes(col = ID, shape = as.factor(out$VISIT), stroke = 1.25)) } +
        {if ( "VISIT" %in% toupper(colnames(data)) ) ggplot2::labs(shape = "VISIT")} +

    # If VISIT column not present
        {if (!"VISIT" %in% toupper(colnames(data)) ) ggplot2::geom_point(size = 3, ggplot2::aes(col = ID, stroke = 1.25)) } +


    # Add plot title
    ggplot2::ggtitle('Dipping Category Plot', subtitle = 'Source: Ohkubo et al. (1997)') +

    # Add axis titles
    ggplot2::xlab('SBP Nocturnal Decline (%)') +
    ggplot2::ylab('DBP Nocturnal Decline (%)') +

    # Add label for Inverted Dippers
    ggplot2::geom_text(ggplot2::aes(x = x_min * 0.9 + 1, y = 0, label = 'INV'), color = 'white', hjust = hjust, vjust = vjust, size = 3) +

    # Add label for Non-Dippers
    ggplot2::geom_text(ggplot2::aes(x = x_min * 0.9 + 1, y = dip_thresh * 100, label = 'ND'), color = 'white', hjust = hjust, vjust = vjust, size = 3) +

    # Add label for Extreme Dippers
    ggplot2::geom_text(ggplot2::aes(x = x_max * 0.9 + 1, y = y_max, label = 'ED'), color = 'white', hjust = hjust, vjust = vjust, size = 3) +

    # Add label for (Normal) Dippers
    ggplot2::geom_text(ggplot2::aes(x = x_min * 0.9 + 1, y = y_max, label = 'DIP'), color = 'black', hjust = hjust, vjust = vjust, size = 3) +

    # Scale X & Y axes to include dips only
    ggplot2::scale_x_continuous(breaks = c(as.integer(x_min), 0, as.integer(dip_thresh * 100), as.integer(extreme_thresh * 100), as.integer(x_max) )) +
    ggplot2::scale_y_continuous(breaks = c(as.integer(y_min), 0, as.integer(dip_thresh * 100), as.integer(extreme_thresh * 100), as.integer(y_max) )) +

    # Classic Theme
    ggplot2::theme_classic()



}
