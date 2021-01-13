#' Blood Pressure Report
#'
#' @description Test
#'
#' @param data Test
#' @param filename Test
#' @param width Test
#' @param height Test
#' @param filetype Test
#' @param loc Test
#' @param save_report Test
#'
#' @return Test
#' @export
#'
#' @examples
#' # Test
bp_report <- function(data, filename = "bp_report.pdf", width = 12, height = 8.53, filetype = 'pdf', loc = NULL, save_report = 1){


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
  dow_tod_plots_tmp <- dow_tod_plots(data)
  scat1 <- bp_scatter(data)
  hist_tmp <- bp_hist(data)

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
    ggplot2::ggsave(grid::grid.draw(final_1), filename = filename, device = filetype, width = width, height = height)


  }


  return(final_1)

}






