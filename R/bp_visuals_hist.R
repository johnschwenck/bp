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
#'
#' @return A list containing three histogram visual graphics corresponding to the SBP / DBP totals,
#' SBP frequency, and DBP frequency.
#'
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
#' bp_hist(hyp_proc)
#' bp_hist(jhs_proc)
bp_hist <- function(data, subj = NULL){


  # Primary variables needed: SBP, DBP, SBP_Category, DBP_Category
  # Assumes bp_type = 0 (both) and bp_tables parameters as defaults
  # Packages: ggplots2, cowplot

  SBP = DBP = SBP_CATEGORY = DBP_CATEGORY = Category = n = bp_type = ID = NULL
  rm(list = c("SBP", "DBP", "SBP_CATEGORY", "DBP_CATEGORY", "Category", "n", "bp_type", "ID"))


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
  if( all(c("SBP", "DBP", "SBP_CATEGORY", "DBP_CATEGORY") %in% names(data)) == FALSE){

    stop('One or more of the required variables are missing. \nEnsure that you have run the process_data() function first.')

  }


  # Alternative Histogram of SBP & DBP
  # hist_alt <- ggplot(data = data) +
  #   geom_histogram(aes(x = SBP, y = ..count.., fill = SBP_Category, color = SBP_Category), position = "identity", alpha = 0.5)+
  #   geom_histogram(aes(x = DBP, y = -..count.., fill = DBP_Category, color = DBP_Category), position = "identity", alpha = 0.5)+
  #   ggtitle("Frequency of BP Readings by BP Type", subtitle = "Neg Values: DBP | Pos Values: SBP")+
  #   labs(x = "BP", y ="# Readings (Neg Values: DBP | Pos Values: SBP)")+
  #   coord_flip()


  ################################################################################

  bpcols <- c("Low"      = "lightblue",
              "Normal"   = "#39ba25",
              "Elevated" = "#e3e029",
              "Stage 1"  = "#ff8c00",
              "Stage 2"  = "darkred",
              "Crisis"   = "red")


  # These are the same colors as the scatterplot for consistency, just in case
  # bpcols_alt <- c("Low"      = 'lightblue',
  #                 "Normal"   = 'lightgreen',
  #                 "Elevated" = 'yellow',
  #                 "Stage 1"  = 'orange',
  #                 "Stage 2"  = 'darkred',
  #                 "Crisis"   = 'red')


  ################################################################################


  # Run bp_tables function once to save time and refer to it later
  bp_table_data <- bp_tables(data)

  # Bar Chart of SBP & DBP Categories
  stages_all <- rbind(bp_table_data[[1]], bp_table_data[[2]])

  hist1 <- ggplot(data = stages_all, aes(x = Category, y = n, fill = bp_type)) +
    geom_bar(stat = "identity", position = position_dodge())+
    geom_text(aes(label=n), vjust= -.5, color="black",
              position = position_dodge(0.9), size=3.5)+
    expand_limits(y = max(stages_all$n) * 1.1) +
    scale_fill_brewer(palette="Paired")+
    theme_minimal() +
    theme(axis.title.x = element_blank()) +
    ylab("Frequency") +
    theme(legend.position='top',
          legend.justification='left',
          legend.direction='horizontal',
          legend.key.size = unit(0.5, "cm")) +
    guides(fill = guide_legend(title = "BP Type: ")) +
    ggtitle("# of Readings per Category by BP Type")



  # SBP Frequency
  hist2 <- ggplot(data = data, aes(x = SBP, fill = SBP_CATEGORY)) +
    geom_histogram(position = "identity", alpha = 0.65, color = 'gray57') +
    ggtitle("Frequency of SBP Readings") +
    ylab("Freq") +
    scale_fill_manual(values = bpcols) +
    theme(legend.position = "top") + theme(legend.direction = "horizontal") + guides(fill = guide_legend(title = "Category: ", nrow = 1))



  # DBP Frequency
  hist3 <- ggplot(data = data, aes(x = DBP, fill = DBP_CATEGORY)) +
    geom_histogram(position = "identity", alpha = 0.65, color = 'gray57')+
    ggtitle("Frequency of DBP Readings") +
    ylab("Freq") +
    scale_fill_manual(values = bpcols) +
    theme(legend.position="none")

  hist_legend <- cowplot::get_legend(hist2)

  hist2 <- hist2 + theme(legend.position="none")

  out <- list(hist1, hist2, hist3, hist_legend)
  return(out)

}

