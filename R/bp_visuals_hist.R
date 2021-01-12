bp_hist <- function(data){


  # Primary variables needed: SBP, DBP, SBP_Category, DBP_Category
  # Assumes bp_type = 0 (both) and bp_tables parameters as defaults
  # Packages: ggplots2, cowplot

  if( all(c("SBP", "DBP", "SBP_Category", "DBP_Category") %in% names(data)) == FALSE){

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
  hist2 <- ggplot(data = data, aes(x = SBP, fill = SBP_Category)) +
    geom_histogram(position = "identity", alpha = 0.65, color = 'gray57') +
    ggtitle("Frequency of SBP Readings") +
    ylab("Freq") +
    scale_fill_manual(values = bpcols) +
    theme(legend.position = "top") + theme(legend.direction = "horizontal") + guides(fill = guide_legend(title = "Category: ", nrow = 1))



  # DBP Frequency
  hist3 <- ggplot(data = data, aes(x = DBP, fill = DBP_Category)) +
    geom_histogram(position = "identity", alpha = 0.65, color = 'gray57')+
    ggtitle("Frequency of DBP Readings") +
    ylab("Freq") +
    scale_fill_manual(values = bpcols) +
    theme(legend.position="none")

  histlegend <- cowplot::get_legend(hist2)

  hist2 <- hist2 + theme(legend.position="none")

  out <- list(hist1, hist2, hist3, histlegend)
  return(out)

}

# Example
#bp_hist(data)
