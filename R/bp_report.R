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
#' \item{\code{SBP_Category}} - Automatically calculated in the \code{process_data} function
#' \item{\code{DBP_Category}} - Automatically calculated in the \code{process_data} function
#' \item{\code{Weekday}} - Automatically calculated in the \code{process_data} function
#' \item{\code{Time_of_Day}} - Automatically calculated in the \code{process_data} function
#' \item{\code{ID}} - (If applicable) Used for separating out different individuals, if more than one
#' \item{\code{VISIT}} - (If applicable) Used for separating out an individuals' different visits,
#' if more than one
#'
#' }
#'
#' @param filename A string corresponding to the name of the report. The default is
#' "bp_report". The string cannot begin with a number or non-alphabetical character.
#' \cr
#' \cr Note: DO NOT include the file type extension (such as ".pdf" or ".png") at the end of the string;
#' the \code{bp_report} function will automatically join the name with the file type.
#'
#' @param width An numeric value corresponding to the width of the output document.
#' The default is set to 12 inches.
#'
#' @param height An numeric value corresponding to the height of the output document.
#' The default is set to 8.53 inches.
#'
#' @param units A character string corresponding to the unit of measurement that the width and height
#' correspond to in the exported output. The default is inches ("in"), but centimeters ("cm") and
#' millimeters ("mm") are also available.
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
#' @param loc Test
#'
#' @param save_report A binary indicator (1 or 0) that determines whether or not to save the output.
#' The default is \code{save_report = 1} indicating that the report will be saved. Regardless of the
#' input, the report will still appear in the RStudio Plots pane.
#'
#' @return A report visual containing various blood pressure metrics and visuals and a saved report (if
#' indicated) on the specified path. If no path specified, the report will be saved at the current
#' working directory which can be checked using \code{getwd()}
#'
#' @export
#'
#' @examples
#' webshot::install_phantomjs()
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
#'
#' # Single-subject Report
#' # Note: save_report set to 0 for illustrative purposes of the example, not to actually save
#' bp_report(jhs_proc, filetype = 'png', save_report = 0)
#'
#' # Multi-subject Report
#' # Note: save_report set to 0 for illustrative purposes of the example, not to actually save
#' bp_report(hyp_proc, save_report = 0)
#'
bp_report <- function(data, filename = "bp_report", width = 11, height = 8.5, filetype = 'pdf', units = "in", loc = NULL, save_report = 1){


  ######################################################################################
  ######################################################################################

  # Create Title graphic
  report_title <- grid::textGrob("Blood Pressure Report", gp=grid::gpar(fontsize=30), hjust = -0.25)
  report_subtitle <- grid::textGrob(paste("            Report Generated: ", lubridate::today(),
                                          ifelse("DATE_TIME" %in% names(data),
                                                 paste("\nData ranging from: ", as.Date(min(data$DATE_TIME)), " - ", as.Date(max(data$DATE_TIME)), sep = ""), ""), sep = ""),
                                    gp=grid::gpar(fontsize=15, fontface=3L), hjust = -0.25)

  margin <- grid::unit(0.75, "line")

  bptitle <- gridExtra::arrangeGrob(report_title, report_subtitle,
                                    heights = grid::unit.c(grid::grobHeight(report_title) + 1.2*margin,
                                                           grid::grobHeight(report_subtitle) + margin,
                                                           grid::unit(1,"null")))
  grid::grid.newpage() # necessary?

  # Run functions once to save time
  dow_tod_plots_tmp <- dow_tod_plots(data) # requires SBP, DBP, Weekday, Time_of_Day, SBP_Category, DBP_Category
  scat1 <- bp_scatter(data) # requires SBP, DBP, possibly VISIT
  hist_tmp <- bp_hist(data) # requires SBP, DBP, SBP_Category, DBP_Category

  # Right-side plots
  p1 <- gridExtra::grid.arrange( gridExtra::arrangeGrob(hist_tmp[[1]], nrow = 1), # side by side histogram of SBP / DBP totals
                                 grid::linesGrob(y = unit(1.5, "lines"),
                                                 gp = grid::gpar(col = "black")),
                                 gridExtra::arrangeGrob(dow_tod_plots_tmp[[3]], dow_tod_plots_tmp[[4]], nrow = 1), # hist of SBp and DBP freqs
                                 nrow = 3, heights = c(1.5, 0.25, 1.5))

  p2 <- gridExtra::grid.arrange(hist_tmp[[2]], hist_tmp[[3]], hist_tmp[[4]], ncol = 2, nrow = 2, layout_matrix = rbind( c(3,3), c(1,2) ), heights = c(.1, .8))

  p3 <- gridExtra::grid.arrange(dow_tod_plots_tmp[[1]], dow_tod_plots_tmp[[2]], nrow = 1)

  final_1 <- gridExtra::grid.arrange(bptitle, grid::nullGrob(),
                                     scat1, p1,
                                     p2, p3,
                                     nrow = 3, ncol = 2, heights = c(.5, 2, 1), widths = c(1, 1))


  ######################################################################################
  ######################################################################################

  # # Determine how granular to calculate based on which columns are available
  # grps = c("ID", "VISIT")
  # grps = grps[which(grps %in% colnames(data) == TRUE)]
  #
  # if(length(grps) == 0){
  #
  #   message('No columns specified for ID, VISIT, or WAKE. All data values aggregated.')
  #
  # }
  #
  # data %>%
  #   dplyr::group_by_at(dplyr::vars(grps) ) %>%
  #   bp_report()

  # For multiple pages
  #final_report = ggpubr::ggarrange(final_1, final_2)

####################################################################################
  # # Save final report to PDF
  # if(save_report == 1){
  #
  #   # Compatibility checks for loc path
  #   if(!is.null(loc)){
  #
  #     # Check that loc path is valid
  #     if(!dir.exists(loc)){
  #       stop('Invalid filepath for loc argument.')
  #     }
  #
  #     # Check that the end of the loc path has either \\ or /  and if not, add it
  #     # if(grep("\\", loc)){
  #     #
  #     # }
  #
  #     # Check that naming convention is correct i.e. cant start with a number or character, etc
  #
  #     # Check whether there already exists a file in the loc directory and
  #     # if so, give user option to choose whether or not to override it
  #     # if( file.exists( paste(loc, filename, sep = "") ) ){
  #     #
  #     #   # user option to override
  #     #
  #     # }
  #
  #   }else{
  #     loc <- paste(getwd(), "\\",sep = "")
  #   }
  #
  #   # Save final report
  #   ggplot2::ggsave(grid::grid.draw(final_1), filename = paste(loc, filename, sep = ""), device = 'pdf', width = width, height = height)
  #
  # } else if(save_report == 0){
  #
  #   warning('Report not saved. To save, specify save_report = 1 in function argument.')
  #
  # } else{
  #
  #   warning('Invalid specification of save_report. save_report can only take values 0 or 1. \nReport not saved.')
  #
  # }

  ####################################################################################

  if(save_report == 0){

    warning('Report not saved. To save, specify save_report = 1 in function argument.')

  }else if(save_report == 1){

    # Save final report
    ggplot2::ggsave(grid::grid.draw(final_1), filename = paste(filename,".",filetype, sep = ""), device = filetype, width = width, height = height, units = units)


  }


  return(final_1)

}






