#' Blood Pressure Stage Scatter Plot
#'
#' @description The \code{bp_scatter} function serves to display all \code{SBP} and \code{DBP}
#' readings on a scatterplot based on which stage each recording lies, according to the
#' levels set by the American Heart Association (AHA) (although the lower / upper limits for
#' these stages can be adjusted in using the \code{sbp_stages_alt} and \code{dbp_stages_alt}
#' functions). There are six total stages which are the following:
#'
#' \itemize{
#'
#' \item \code{Low} - While low readings do not necessarily imply Hypotension, they are worth staying
#' alert for. According to the AHA, low blood pressure is any \code{SBP} reading below 100 or
#' \code{DBP} reading below 60 and is depicted in light blue in the scatter plot.
#'
#' \item \code{Normal} - Consistent \code{SBP} readings between 100 - 120 and \code{DBP} readings
#' between 60 - 80. This is the ideal stage where blood pressure is to be maintained at.
#' Normal BP is depicted in green in the scatter plot.
#'
#' \item \code{Elevated} - Consistent \code{SBP} readings between 120 - 130 and \code{DBP}
#' readings less than 80. Without intervention to control the condition, individuals are
#' likely to develop Hypertension. Elevated BP is depicted in yellow in the scatter plot.
#'
#' \item \code{Stage 1} - Consistent \code{SBP} readings between 130 - 140 or \code{DBP} readings
#' between 80 - 90. Stage 1 Hypertension will typically result in doctors prescribing
#' medication or lifestyle changes. Stage 1 BP is depicted in orange in the scatter plot.
#'
#' \item \code{Stage 2} - Consistent \code{SBP} readings between 140 - 180 or \code{DBP} readings
#' between 90 - 120. Stage 2 Hypertension will typically result in doctors prescribing both
#' medication and lifestyle changes. Stage 2 BP is depicted in dark red in the scatter plot.
#'
#' \item \code{Crisis} - A Hypertensive crisis is defined as a \code{SBP} reading exceeding 180 or a
#' \code{DBP} reading exceeding 120. This stage requires medical attention immediately.
#' Crisis is depicted in red in the scatter plot.
#' }
#'
#' Note: Because of the visual disparity between \code{DBP} readings for Normal and Elevated
#' (both are defined as \code{DBP} below 80), the \code{bp_scatter} plot splits the difference
#' and lists the \code{DBP} range for Normal to be from 60 - 80, Elevated from 80 - 85, and
#' Stage 1 from 85 - 90.
#'
#' @param data A processed dataframe resulting from the \code{process_data} function that
#' contains the \code{VISIT} (potentially, depending whether or not that information is
#' available), \code{SBP}, and \code{DBP} columns.
#'
#' @param sbp_stages_alt Optional argument that allows the user to supply their own set of
#' systolic blood pressure (SBP) stage thresholds. This parameter must be a vector containing
#' 7 integers that correspond to the lower and upper limits of each of the 6 stages. The
#' limits must be adjacent to one another as to not overlap (i.e. the upper limit for one
#' stage must be the lower limit for the next).
#'
#' For example, the alternative vector:
#' \code{sbp_stages_alt = c(90, 110, 120, 130, 140, 160, 200)} corresponds to a Low
#' stage from 90 - 110, a Normal stage from 110 - 120, and so forth for all 6 stages.
#'
#' @param dbp_stages_alt Optional argument that allows the user to supply their own set of
#' diastolic blood pressure (DBP) stage thresholds. This parameter must be a vector containing
#' 7 integers that correspond to the lower and upper limits of each of the 6 stages. The
#' limits must be adjacent to one another as to not overlap (i.e. the upper limit for one
#' stage must be the lower limit for the next).
#'
#' For example, the alternative vector:
#' \code{dbp_stages_alt = c(30, 70, 90, 100, 110, 120, 140)} corresponds to a Low stage from
#' 30 - 70, a Normal stage from 70 - 90, and so forth for all 6 stages.
#'
#' @param subj Optional argument. Allows the user to specify and subset specific subjects
#' from the \code{ID} column of the supplied data set. The \code{subj} argument can be a single
#' value or a vector of elements. The input type should be character, but the function will
#' comply with integers so long as they are all present in the \code{ID} column of the data.
#'
#'
#' @return A scatter plot graphic using the ggplot2 package overlaying each reading (represented as
#' points) onto a background that contains each stage
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
#' # An example of a multiple-subject data set with
#' # all points aggregated across subjects, split by visits:
#'
#' bp_scatter(hyp_proc)
#'
#'
#' # An example of a multiple-subject data set being filtered
#' # to one individual for one particular visit:
#'
#' hyp_proc %>% dplyr::group_by(ID, VISIT) %>% dplyr::filter(ID == 70417, VISIT == 1) %>% bp_scatter()
#'
#'
#' # An example of a single-subject data set without
#' # any VISIT variable present in the data:
#'
#' bp_scatter(jhs_proc)

bp_scatter <- function(data, sbp_stages_alt = NULL, dbp_stages_alt = NULL, subj = NULL){

  # Variables needed: SBP, DBP, possibly VISIT

  SBP = DBP = VISIT = ID = NULL
  rm(list = c("SBP", "DBP", "VISIT", "ID"))


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


  # Ensure that the necessary columns exist in data set
  if( all(c("SBP", "DBP") %in% names(data)) == FALSE){

    stop('One or more of the required variables are missing. \nEnsure that you have run the process_data() function first.')

  }


  ##############################################################

  # Compatibility Check for user-supplied stages if applicable
  sbp_breaks <- stage_check(sbp_stages_alt, dbp_stages_alt)[[1]]
  dbp_breaks <- stage_check(sbp_stages_alt, dbp_stages_alt)[[2]]

  ##############################################################


  # Scatterplot of bp stages

  scat1 <- ggplot(data, aes(DBP, SBP)) +

    # Give user option to adjust breaks
    scale_y_continuous(breaks = sbp_breaks) +
    scale_x_continuous(breaks = dbp_breaks) +

    # Y axis bars:
    annotate("rect", xmin = dbp_breaks[1], xmax = dbp_breaks[2],  ymin = sbp_breaks[1], ymax = sbp_breaks[2], fill = 'lightblue') +
    annotate("rect", xmin = dbp_breaks[1], xmax = dbp_breaks[3],  ymin = sbp_breaks[2], ymax = sbp_breaks[3], fill = 'darkgreen',  alpha = .5) +
    annotate("rect", xmin = dbp_breaks[1], xmax = dbp_breaks[4],  ymin = sbp_breaks[3], ymax = sbp_breaks[4], fill = 'yellow',     alpha = .5) +
    annotate("rect", xmin = dbp_breaks[1], xmax = dbp_breaks[5],  ymin = sbp_breaks[4], ymax = sbp_breaks[5], fill = 'orange',     alpha = .5) +
    annotate("rect", xmin = dbp_breaks[1], xmax = dbp_breaks[6],  ymin = sbp_breaks[5], ymax = sbp_breaks[6], fill = 'darkred',    alpha = .5) +
    annotate("rect", xmin = dbp_breaks[1], xmax = dbp_breaks[7],  ymin = sbp_breaks[6], ymax = sbp_breaks[7], fill = 'red',        alpha = .5) +

    # X axis bars
    annotate("rect", xmin = dbp_breaks[2], xmax = dbp_breaks[3], ymin = sbp_breaks[1], ymax = sbp_breaks[2], fill = 'darkgreen',  alpha = .5) +
    annotate("rect", xmin = dbp_breaks[3], xmax = dbp_breaks[4], ymin = sbp_breaks[1], ymax = sbp_breaks[3], fill = 'yellow',     alpha = .5) +
    annotate("rect", xmin = dbp_breaks[4], xmax = dbp_breaks[5], ymin = sbp_breaks[1], ymax = sbp_breaks[4], fill = 'orange',     alpha = .5) +
    annotate("rect", xmin = dbp_breaks[5], xmax = dbp_breaks[6], ymin = sbp_breaks[1], ymax = sbp_breaks[5], fill = 'darkred',    alpha = .5) +
    annotate("rect", xmin = dbp_breaks[6], xmax = dbp_breaks[7], ymin = sbp_breaks[1], ymax = sbp_breaks[6], fill = 'red',        alpha = .5) +

    # If VISIT column present
    {if( "VISIT" %in% names(data) & length(unique(data$VISIT)) > 1) geom_point(aes(color = factor(VISIT)), size = 1) } +
    {if( "VISIT" %in% names(data) & length(unique(data$VISIT)) > 1) scale_color_brewer(type = 'div', palette = 'Blues')} +
    {if( "VISIT" %in% names(data) & length(unique(data$VISIT)) > 1) guides(color=guide_legend(title="Visit")) } +

    # If VISIT column NOT present or # Visits = 1
    {if( !("VISIT" %in% names(data)) | length(unique(data$VISIT)) == 1 ) geom_point(color = 'blue', size = 1)} +

    geom_text(aes(x = dbp_breaks[1] + 5, y = sbp_breaks[2], label = 'Low'), color = 'black', hjust = .35, vjust = 2, size = 3) +
    geom_text(aes(x = dbp_breaks[1] + 5, y = sbp_breaks[3], label = 'Normal'), color = 'black', hjust = .35, vjust = 2, size = 3) +
    geom_text(aes(x = dbp_breaks[1] + 5, y = sbp_breaks[4], label = 'Elevated'), color = 'black', hjust = .35, vjust = 1.5, size = 3) +
    geom_text(aes(x = dbp_breaks[1] + 5, y = sbp_breaks[5], label = 'Stage 1'), color = 'black', hjust = .35, vjust = 1.5, size = 3) +
    geom_text(aes(x = dbp_breaks[1] + 5, y = sbp_breaks[6] - ( (sbp_breaks[6] - sbp_breaks[5]) / 2), label = 'Stage 2'), color = 'black', hjust = .35, vjust = 1.5, size = 3) +
    geom_text(aes(x = dbp_breaks[1] + 5, y = sbp_breaks[7], label = 'Crisis'), color = 'black', hjust = .35, vjust = 1.5, size = 3) +

    ggtitle('Scatterplot of BP Values', subtitle = 'Source: American Heart Association')


  return(scat1)

}

