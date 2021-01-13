bp_scatter <- function(data, sbp_stages_alt = NULL, dbp_stages_alt = NULL){

  # Variables needed: SBP, DBP, possibly VISIT

  SBP = DBP = VISIT = NULL
  rm(list = c(SBP, DBP, VISIT))

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

    ggtitle('Scatterplot of BP Values', subtitle = 'According to American Heart Association')


  return(scat1)

}


# Example
# data %>% dplyr::group_by(ID, VISIT) %>% dplyr::filter(ID == 70417, VISIT == 1) %>% bp_scatter()
