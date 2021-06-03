#' Blood Pressure Report
#'
#' @description The \code{bp_report} function serves to aggregate various data visuals and metrics
#' pertaining to the supplied data set into a clean formatted report.
#'
#' @param data Required argument. A processed dataframe resulting from the \code{process_data}
#' function to properly format data set. In order for the \code{bp_report} function to work properly,
#' the following variables must be present and properly formatted:
#' \itemize{
#'
#' \item{\code{SBP}}
#' \item{\code{DBP}}
#' \item{\code{DATE_TIME}} - Used in the \code{process_data} function to create additional columns
#' that are needed for the \code{bp_report} function (SBP_Category, DBP_Category, Weekday, and
#' Time_of_Day.)
#' \item{\code{SBP_CATEGORY}} - Automatically calculated in the \code{process_data} function
#' \item{\code{DBP_CATEGORY}} - Automatically calculated in the \code{process_data} function
#' \item{\code{DAY_OF_WEEK}} - Automatically calculated in the \code{process_data} function
#' \item{\code{TIME_OF_DAY}} - Automatically calculated in the \code{process_data} function
#' \item{\code{ID}} - (If applicable) Used for separating out different individuals, if more than one
#' \item{\code{VISIT}} - (If applicable) Used for separating out an individuals' different visits,
#' if more than one
#'
#' }
#'
#' @param subj Optional argument. Allows the user to specify and subset specific subjects
#' from the \code{ID} column of the supplied data set. The \code{subj} argument can be a single
#' value or a vector of elements. The input type should be character, but the function will
#' comply with integers so long as they are all present in the \code{ID} column of the data.
#'
#' @param inc_low Optional
#'
#' @param inc_crisis Optional
#'
#' @param group_var Optional
#'
#' @param save_report A binary indicator (1 or 0) that determines whether or not to save the output.
#' The default is \code{save_report = 1} indicating that the report will be saved. Regardless of the
#' input, the report will still appear in the RStudio Plots pane.
#'
#' @param path Optional argument. A string corresponding to the respective file path by which the
#' report is to be saved. Do not include trailing slashes (i.e. ~/loc/) or the file name (i.e. ~/loc/testfile.pdf).
#' By default, if not \code{path} argument specified, will save at the current working directory.
#'
#' @param filename Optional argument. A string corresponding to the name of the report. The default is
#' "bp_report". The string cannot begin with a number or non-alphabetical character.
#' \cr
#' \cr Note: DO NOT include the file type extension (such as ".pdf" or ".png") at the end of the string;
#' the \code{bp_report} function will automatically join the name with the file type.
#'
#' @param width Optional argument. An numeric value corresponding to the width of the output document.
#' The default is set to 12 inches.
#'
#' @param height Optional argument. An numeric value corresponding to the height of the output document.
#' The default is set to 8.53 inches.
#'
#' @param filetype A string corresponding to he particular type of file that the report is to be saved as.
#' Although PDF is the default possible options include:
#' \itemize{
#'
#' \item{pdf} (default)
#' \item{png}
#' \item{jpeg}
#' \item{tiff}
#' \item{bmp}
#' \item{eps}
#' \item{ps}
#'
#' }
#'
#' @param units A character string corresponding to the unit of measurement that the width and height
#' correspond to in the exported output. The default is inches ("in"), but centimeters ("cm") and
#' millimeters ("mm") are also available.
#'
#' @param scale A multiplicative scaling factor for the report output.
#'
#' @return A report visual containing various blood pressure metrics and visuals and a saved report (if
#' indicated) on the specified path. If no path specified, the report will be saved at the current
#' working directory which can be checked using \code{getwd()}
#'
#' @export
#'
#' @examples
#'
#' data("bp_jhs")
#' data("bp_hypnos")
#' hyp_proc <- process_data(bp_hypnos,
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
#
#' # Single-subject Report
#' # Note: save_report set to 0 for illustrative purposes of the example, not to actually save
#' bp_report(jhs_proc, filetype = 'png', save_report = 0)
#'
#' # Multi-subject Report
#' # Note: save_report set to 0 for illustrative purposes of the example, not to actually save
#' ## Not Run
#' \dontrun{
#' bp_report(hyp_proc, group_var = 'VISIT', save_report = 0)
#' }
#'
bp_report <- function(data,
                      subj = NULL,
                      inc_low = TRUE,
                      inc_crisis = TRUE,
                      group_var = NULL,
                      save_report = 1,
                      path = NULL,
                      filename = "bp_report",
                      width = 11,
                      height = 8.5,
                      filetype = "pdf",
                      units = "in",
                      scale = 1.25){


  ######################################################################################

  ID = NULL
  rm(list = c('ID'))

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

  subjects <- unique(data$ID)
  n <- length(subjects)
  idx <- 1:n

  #####################################################
  # For "overview" of all subjects - aggregated plots #
  #####################################################

  grid::grid.newpage() # necessary?

  # Create Title graphic --> need to find a better way to make the title dynamic to adjust to different aspect ratios of the report
  report_subtitle <- grid::textGrob(paste("\nReport Generated: ", lubridate::today(),
                                          ifelse("DATE_TIME" %in% names(data),
  paste('                                                                                                           Data range: ', as.Date(min(data$DATE_TIME)), " to ", as.Date(max(data$DATE_TIME)), sep = ""), ""), sep = ""),
                                    gp = grid::gpar(fontsize = 15, fontface = 3L), hjust = 0.48)

  report_title <- grid::textGrob( ifelse(n > 1, "Blood Pressure Report - All Subjects", "Blood Pressure Report - Overview"),
                                  gp = grid::gpar(fontsize = 30),
                                  hjust = 0.45)


  margin <- grid::unit(0.5, "line")

  bptitle <- gridExtra::arrangeGrob(report_title, report_subtitle,
                                    heights = grid::unit.c(grid::grobHeight(report_title) + 1.2 * margin,
                                                           grid::unit(1,"null"),
                                                           grid::grobHeight(report_title) + margin)
                                    )

  # Run functions once to save time
  dow_tod_plots_all <- dow_tod_plots(data) # requires SBP, DBP, Weekday, Time_of_Day, SBP_Category, DBP_Category
  scat_all <- bp_scatter(data, inc_low = inc_low, inc_crisis = inc_crisis, group_var = group_var) # requires SBP, DBP, possibly VISIT
  hist_all <- bp_hist(data) # requires SBP, DBP, SBP_Category, DBP_Category


  # Scatterplot combine with legend
  #scat_all <- gridExtra::arrangeGrob(scat_all, hist_all[[4]], nrow = 2, heights = c(2, 0.15))

  # SBP / DBP frequencies
  all_1 <- gridExtra::arrangeGrob(hist_all[[2]], hist_all[[3]], nrow = 2)

  # ToD / DoW
  all_2 <- gridExtra::arrangeGrob(dow_tod_plots_all[[1]], dow_tod_plots_all[[2]], nrow = 1)

  # Layout of output
  lay <- rbind( c(1,  1,1,1,1,1,1,1,1, 1,  1,1,1,1,1,1,1,1, 1),
                c(NA, 2,2,2,2,2,2,2,2, NA, 3,3,3,3,3,3,3,3, NA),
                c(NA, 2,2,2,2,2,2,2,2, NA, 3,3,3,3,3,3,3,3, NA),
                c(NA, 2,2,2,2,2,2,2,2, NA, 3,3,3,3,3,3,3,3, NA),
                c(NA, 2,2,2,2,2,2,2,2, NA, 3,3,3,3,3,3,3,3, NA),
                c(NA, 2,2,2,2,2,2,2,2, NA, 4,4,4,4,4,4,4,4, NA),
                c(NA, 5,5,5,5,5,5,5,5, NA, 4,4,4,4,4,4,4,4, NA),
                c(NA, 5,5,5,5,5,5,5,5, NA, 4,4,4,4,4,4,4,4, NA),
                c(NA, 5,5,5,5,5,5,5,5, NA, 4,4,4,4,4,4,4,4, NA),
                c(NA, 5,5,5,5,5,5,5,5, NA, 4,4,4,4,4,4,4,4, NA),
                c(NA, NA,NA,NA,NA,NA,NA,NA,NA, NA, NA,NA,NA,NA,NA,NA,NA,NA, NA ))

  # Final Report
  final <- gridExtra::arrangeGrob(bptitle, scat_all, hist_all[[1]], all_1, all_2, layout_matrix = lay)



  # Determine whether to save output or not
  if(save_report == 0){

    message('Report not saved. To save, specify save_report = 1 in function argument.')

    gridExtra::grid.arrange(final)

  }else if(save_report == 1){

    # Verify that location (path) is valid and create proper path
    path <- path_check(path)
    out_filename <- paste(filename,".",filetype, sep = "")

    out_path <- file.path(path, out_filename)

    # Save final report
    ggplot2::ggsave(out_path, gridExtra::grid.arrange(final),
           width = width,
           height = height,
           units = units,
           scale = scale)

  }

  return(final)

}







