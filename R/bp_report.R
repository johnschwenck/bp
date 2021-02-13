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
#' @param inc_all An indicator to dictate whether or not an overview page containing the data for
#' all subjects (aggregated together) should be included in the report. By default, \code{inc_all = TRUE}.
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
#
#' # Single-subject Report
#' # Note: save_report set to 0 for illustrative purposes of the example, not to actually save
#' bp_report(jhs_proc, filetype = 'png', save_report = 0)
#'
#' # Multi-subject Report
#' # Note: save_report set to 0 for illustrative purposes of the example, not to actually save
#' ## Not Run
#' \dontrun{
#' bp_report(hyp_proc, save_report = 0)
#' }
#'
bp_report <- function(data,
                      subj = NULL,
                      inc_all = TRUE,
                      save_report = 1,
                      path = NULL,
                      filename = "bp_report",
                      width = 11,
                      height = 8.5,
                      filetype = "pdf",
                      units = "in",
                      scale = 1.25){


  ######################################################################################

  subjects <- unique(data$ID)
  n <- length(subjects)
  idx <- 1:n

  final <- list()

  #####################################################
  # For "overview" of all subjects - aggregated plots #
  #####################################################

  # Check whether user decides against including all subject (which will skip the if statement below)
  if(inc_all == TRUE){

    grid::grid.newpage() # necessary?

    # Create Title graphic
    report_subtitle <- grid::textGrob(paste("Report Generated: ", lubridate::today(),
                                            ifelse("DATE_TIME" %in% names(data),
    paste("                                                                                                             Data range: ",
                                                         as.Date(min(data$DATE_TIME)),
                                                         " to ",
                                                         as.Date(max(data$DATE_TIME)), sep = ""), ""), sep = ""),
                                      gp = grid::gpar(fontsize = 15, fontface = 3L), hjust = 0.25)

    report_title <- grid::textGrob( ifelse(n > 1, "Blood Pressure Report - All Subjects", "Blood Pressure Report - Overview"),
                                    gp = grid::gpar(fontsize = 30),
                                    hjust = -0.0)


    margin <- grid::unit(0.5, "line")

    bptitle <- gridExtra::arrangeGrob(report_subtitle, report_title,
                                      heights = grid::unit.c(grid::grobHeight(report_title) + 1.2 * margin,
                                                             grid::unit(1,"null"),
                                                             grid::grobHeight(report_subtitle) + margin)
                                      )

    # Run functions once to save time
    dow_tod_plots_all <- dow_tod_plots(data) # requires SBP, DBP, Weekday, Time_of_Day, SBP_Category, DBP_Category
    scat_all <- bp_scatter(data) # requires SBP, DBP, possibly VISIT
    hist_all <- bp_hist(data) # requires SBP, DBP, SBP_Category, DBP_Category

    # Right-side plots
    all_1 <- gridExtra::grid.arrange( gridExtra::arrangeGrob(hist_all[[1]], nrow = 1), # side by side histogram of SBP / DBP totals
                                   grid::linesGrob(y = unit(1.5, "lines"),
                                                   gp = grid::gpar(col = "black")),
                                   gridExtra::arrangeGrob(dow_tod_plots_all[[1]], dow_tod_plots_all[[2]], nrow = 1), # hist of SBP and DBP freqs
                                   nrow = 3, heights = c(1.5, 0.25, 1.5))

    all_2 <- gridExtra::grid.arrange(hist_all[[2]], hist_all[[3]], hist_all[[4]], ncol = 2, nrow = 2, layout_matrix = rbind( c(3,3), c(1,2) ), heights = c(.1, .8))

    all_3 <- gridExtra::grid.arrange(dow_tod_plots_all[[3]], dow_tod_plots_all[[4]], nrow = 1)

    out_all <- gridExtra::grid.arrange(bptitle, grid::nullGrob(),
                                       scat_all, all_1,
                                       all_2, all_3,
                                       nrow = 3, ncol = 2, heights = c(.5, 2, 1), widths = c(1, 1))

    final[[1]] <- out_all

    # j correspond to final output list, first element of list is overview
    j <- 2

  }else{

    # J correspond to final output list, first element of list is first subject because no overview page included
    j <- 1

  }


  ##############################################
  # If multiple subjects, print plots for each #
  ##############################################

  # Repeat for all subjects
  if( n > 1 ){


    # If user supplies a vector corresponding to a subset of multiple subjects (multi-subject only)
    if(!is.null(subj)){

      # check to ensure that supplied subject vector is compatible
      subject_subset_check(data, subj)

      if(length(subjects) > 1){

        idx <- match( subj , unique(data$ID) )

      }
    }


    for(i in idx){

      # Title graphic
      report_subtitle <- grid::textGrob(paste("Report Generated: ", lubridate::today(),
                                              ifelse("DATE_TIME" %in% names(data),
        paste("                                                                                                             Data range: ",
                                                           as.Date(min(data$DATE_TIME)),
                                                           " to ",
                                                           as.Date(max(data$DATE_TIME)), sep = ""), ""), sep = ""),
                                        gp = grid::gpar(fontsize = 15, fontface = 3L), hjust = 0.25)

      report_title <- grid::textGrob( paste("Blood Pressure Report - Subject ID: ", subjects[i], sep = ""),
                                      gp = grid::gpar(fontsize = 30),
                                      hjust = -0.0)



      margin <- grid::unit(0.5, "line")

      bptitle_tmp <- gridExtra::arrangeGrob(report_subtitle, report_title,
                                        heights = grid::unit.c(grid::grobHeight(report_title) + 1.2 * margin,
                                                               grid::unit(1,"null"),
                                                               grid::grobHeight(report_subtitle) + margin
                                                               ) )

      # Subset Data
      tmp_data <- data[which(data$ID == subjects[i]),]

      # Run functions once to save time
      dow_tod_plots_tmp <- dow_tod_plots(tmp_data) # requires SBP, DBP, Weekday, Time_of_Day, SBP_Category, DBP_Category
      scat_tmp <- bp_scatter(tmp_data) # requires SBP, DBP, possibly VISIT
      hist_tmp <- bp_hist(tmp_data) # requires SBP, DBP, SBP_Category, DBP_Category


      # Right-side plots
      tmp_1 <- gridExtra::grid.arrange( gridExtra::arrangeGrob(hist_tmp[[1]], nrow = 1), # side by side histogram of SBP / DBP totals
                                        grid::linesGrob(y = unit(1.5, "lines"),
                                                        gp = grid::gpar(col = "black")),
                                        gridExtra::arrangeGrob(dow_tod_plots_tmp[[1]], dow_tod_plots_tmp[[2]], nrow = 1), # hist of SBP and DBP freqs
                                        nrow = 3, heights = c(1.5, 0.25, 1.5))

      tmp_2 <- gridExtra::grid.arrange(hist_tmp[[2]], hist_tmp[[3]], hist_tmp[[4]],
                                       ncol = 2, nrow = 2, layout_matrix = rbind( c(3,3), c(1,2) ), heights = c(.1, .8))

      tmp_3 <- gridExtra::grid.arrange(dow_tod_plots_tmp[[3]], dow_tod_plots_tmp[[4]], nrow = 1)

      out_tmp <- gridExtra::grid.arrange(bptitle_tmp, grid::nullGrob(),
                                         scat_tmp, tmp_1,
                                         tmp_2, tmp_3,
                                         nrow = 3, ncol = 2, heights = c(.5, 2, 1), widths = c(1, 1))

      final[[j]] <- out_tmp

      j <- j + 1

    }


  }

  # Determine whether to save output or not
  if(save_report == 0){

    message('Report not saved. To save, specify save_report = 1 in function argument.')

  }else if(save_report == 1){

    # Verify that location (path) is valid and create proper path
    path <- path_check(path)
    out_filename <- paste(filename,".",filetype, sep = "")

    out_path <- file.path(path, out_filename)

    # Save final report
    ggsave(out_path, gridExtra::marrangeGrob(grobs = final, nrow = 1, ncol = 1),
           width = width,
           height = height,
           units = units,
           scale = scale)

  }

  return(final)

}







