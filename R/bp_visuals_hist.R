#' Blood Pressure Histograms
#'
#' @description The \code{bp_hist} function serves to display the frequencies of the \code{SBP}
#' and \code{DBP} readings. These histograms are formatted to complement the \code{bp_scatter}
#' function.
#'
#' @param data A processed dataframe resulting from the \code{process_data} function that
#' contains the \code{SBP}, \code{DBP}, \code{SBP_CATEGORY}, and \code{DBP_CATEGORY} columns.
#'
#' @param subj Optional argument. Allows the user to specify and subset specific subjects
#' from the \code{ID} column of the supplied data set. The \code{subj} argument can be a single
#' value or a vector of elements. The input type should be character, but the function will
#' comply with integers so long as they are all present in the \code{ID} column of the data.
#'
#' @param bins An integer specifying how many bins to use for histogram plots. This is a ggplot parameter; default value set to 30
#'
#' @param na.rm An logical value specifying whether or not to remove empty values from data frame.
#' This is a ggplot parameter; default value set to TRUE.
#'
#' @return A list containing four objects: three histogram visual graphics corresponding to
#' the SBP / DBP totals, SBP frequency, and DBP frequency, and a fourth list element
#' corresponding to a plot legend object for use in the \code{bp_report} function
#'
#' @export
#'
#' @examples
#' data("bp_jhs")
#' data("bp_hypnos")
#' hyp_proc <- process_data(bp_hypnos,
#'                          bp_type = 'abpm',
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
#' bp_hist(hyp_proc)
#' bp_hist(jhs_proc)
bp_hist <- function(data, subj = NULL, bins = 30, na.rm = TRUE){


  # Primary variables needed: SBP, DBP, SBP_Category, DBP_Category
  # Assumes bp_type = 'both', 'sbp', or 'dbp' and bp_tables parameters as defaults
  # Packages: ggplots2, cowplot

  # Initialize variables
  SBP = DBP = SBP_CATEGORY = DBP_CATEGORY = Category = n = ID = NULL
  rm(list = c("SBP", "DBP", "SBP_CATEGORY", "DBP_CATEGORY", "Category", "n", "ID"))


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


  # Ensure that the necessary columns exist in data set
  if( all(c("SBP", "DBP", "SBP_CATEGORY", "DBP_CATEGORY") %in% names(data)) == FALSE){

    stop('One or more of the required variables are missing. \nEnsure that you have run the process_data() function first.')

  }


  ################################################################################

  bpcols <- c("Low"      = "lightblue",
              "Normal"   = "darkgreen",
              "Elevated" = "yellow",
              "Stage 1"  = "orangered1",
              "Stage 2"  = "red",
              "IDH - S1" = "orange",
              "IDH - S2" = "red3",
              "ISH - S1" = "orange",
              "ISH - S2" = "red3",
              "Crisis"   = "darkred")


  ################################################################################


  # Run bp_tables function once to save time and refer to it later
  bp_table_data <- bp_tables(data)

  # Bar Chart of SBP & DBP Categories
  stages_all <- rbind(bp_table_data[[1]], bp_table_data[[2]])

  # BP Class colors according to what is available (Low and Crisis categories or not)
  chart_cols <- bpcols[names(bpcols) %in% unique(bp_table_data[[3]]$Category)]

  # BP Counts (All Stages - both SBP & DBP)
  hist1 <- ggplot(data = bp_table_data[[3]], aes(x = Category, y = n, fill = factor(Category)) ) +
    geom_bar(stat = 'identity', position = position_dodge(),  alpha = 0.5) +
    theme(legend.position = "top") +
    theme(legend.direction = "horizontal") +
    guides(fill = guide_legend(title = "Category: ", nrow = 1)) +
    scale_fill_manual(values = chart_cols, na.value = 'black') +

    geom_text(aes(label = n), vjust = -0.5, color = 'black', position = position_dodge(0.9), size = 3.5) +
    expand_limits(y = max(bp_table_data[[3]]$n) * 1.1) +

    theme_minimal() +
    theme(axis.title.x = element_blank()) +
    theme(axis.text.x = element_text(angle = 45, hjust = 0.75)) +
    ylab("Frequency") +

    ggtitle("# of Readings per BP Classification")

  hist_legend <- cowplot::get_legend(hist1)
  hist1 <- hist1 + theme(legend.position="none")



  # SBP Frequency

  sbp_cols <- bpcols[names(bpcols) %in% unique(data$SBP_CATEGORY)]

  hist2 <- ggplot(data = data, aes(x = SBP, fill = SBP_CATEGORY)) +
    geom_histogram(position = "identity", alpha = 0.65, color = 'gray57', bins = bins, na.rm = na.rm) +
    ggtitle("Frequency of SBP Readings") +
    ylab("Freq") +
    scale_fill_manual(values = sbp_cols, na.value = 'black') +
    #theme(legend.position = "top") + theme(legend.direction = "horizontal") + guides(fill = guide_legend(title = "Category: ", nrow = 1))
    theme(legend.position="none") +
    theme_minimal()


  # DBP Frequency

  dbp_cols <- bpcols[names(bpcols) %in% unique(data$DBP_CATEGORY)]

  hist3 <- ggplot(data = data, aes(x = DBP, fill = DBP_CATEGORY)) +
    geom_histogram(position = "identity", alpha = 0.65, color = 'gray57', bins = bins, na.rm = na.rm)+
    ggtitle("Frequency of DBP Readings") +
    ylab("Freq") +
    scale_fill_manual(values = dbp_cols, na.value = 'black') +
    theme(legend.position="none") +
    theme_minimal()


  out <- list(hist1, hist2, hist3, hist_legend)
  return(out)

}

