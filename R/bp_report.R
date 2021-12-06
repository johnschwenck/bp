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
#' @param inc_low Optional logical argument dictating whether or not to include the "Low" category for BP
#' classification column (and the supplementary SBP/DBP Category columns). Default set to TRUE.
#'
#' @param inc_crisis Optional logical argument dictating whether or not to include the "Crisis" category for BP
#' classification column (and the supplementary SBP/DBP Category columns). Default set to TRUE.
#'
#' @param group_var A categorical column of the input data set that the individual points are to
#' be grouped / separated by for a given plot. Cannot contain more than 10 levels (to avoid
#' overcrowding the plot). This is different from the \code{wrap_var} argument which segments
#' plots by category and cannot be used with the \code{process_data} function.
#'
#' @param save_report A logical value indicating whether to save the BP report output as a separate file.
#' The default is \code{TRUE} indicating that the report will be saved.
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
#' @param plot A logical value indicating whether to automatically produce the plot of bp_report, or suppress the output. The default value is TRUE. If false, the returned object is a grob that can be plotted using \code{\link{grid.arrange}}
#'
#' @return If \code{plot = TRUE}, the function produces a plot of BP report that contains scatterplot of BP values by stages (see \code{\link{bp_scatter}}), histograms of BP values by stages (see \code{\link{bp_hist}}) and frequency tables of BP values by stages and day of the week/time of the day (see \code{\link{dow_tod_plots}}). If \code{plot = FALSE}, the function returns the grob object that can be plotted later using \code{\link{grid.arrange}}. If \code{save_report = TRUE}, the report will be automatically saved at the current working directory (can be checked using \code{getwd()}) or at specified file path.
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
#'
#'\dontrun{
#' # Single-subject Report
#' # save_report = FALSE for illustrative purposes
#' # plot = TRUE will automatically generate a plot
#' bp_report(jhs_proc, save_report = FALSE, plot = TRUE)
#'
#' # Multi-subject Report
#' # save_report = FALSE for illustrative purposes
#' # plot = FALSE will suppress the plot output and return a grob object
#' out = bp_report(hyp_proc, group_var = 'VISIT', save_report = FALSE, plot = FALSE)
#' gridExtra::grid.arrange(out)
#' }
bp_report <- function(data,
                      subj = NULL,
                      inc_low = TRUE,
                      inc_crisis = TRUE,
                      group_var = NULL,
                      save_report = TRUE,
                      path = NULL,
                      filename = "bp_report",
                      width = 11,
                      height = 8.5,
                      filetype = "pdf",
                      units = "in",
                      scale = 1.25,
                      plot = TRUE){


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
        dplyr::filter(ID %in% subj)

    }
  }

  subjects <- unique(data$ID)
  n <- length(subjects)
  idx <- 1:n

  #####################################################
  # For "overview" of all subjects - aggregated plots #
  #####################################################

  # grid::grid.newpage() # necessary?

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
  dow_tod_plots_all <- dow_tod_plots(data, subj = subj) # requires SBP, DBP, Weekday, Time_of_Day, SBP_Category, DBP_Category
  scat_all <- bp_scatter(data, subj = subj, inc_low = inc_low, inc_crisis = inc_crisis, group_var = group_var) # requires SBP, DBP, possibly VISIT
  hist_all <- bp_hist(data, subj = subj) # requires SBP, DBP, SBP_Category, DBP_Category


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

  #out = gridExtra::grid.arrange(final)


  # Determine whether to save output or not
  if(save_report == FALSE){

    message('Report not saved to a file. To save, specify save_report = TRUE.')

    #return(out)
    #return(final)

  }else{

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
    #return(final)
  }

  if (plot == TRUE){
    gridExtra::grid.arrange(final)
  }else{
    return(final)
  }

}







