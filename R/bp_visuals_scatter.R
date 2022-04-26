
#' Blood Pressure Stage Scatter Plot
#'
#' @description Display all \code{SBP} and \code{DBP}
#' readings on a scatterplot with deliniation of BP according to the 8
#' mutually exclusive levels of Hypertension as in Lee et al (2020) (the default), or the
#' levels set by the American Heart Association (AHA).
#'
#'
#' @param data A processed dataframe resulting from the \code{process_data} function that
#' contains the \code{SBP}, and \code{DBP} columns, as well as (potentially) other information that can be used for grouping.
#'
#' @param plot_type String corresponding to the particular type of plot to be displayed. Default
#' plot (\code{"stages2020"}) sets the BP stages according to Lee et al (2020) with 8 mutually
#' exclusive categories. Two additional categories, "Low" or "Crisis", can be determined
#' through the \code{inc_low} or \code{inc_crisis} function arguments, respectively.
#' Setting \code{plot_type = "AHA"} will use the stages according to the guidelines
#' set forth by the American Heart Association (reference: \url{https://www.heart.org/en/health-topics/high-blood-pressure/understanding-blood-pressure-readings})
#'
#'
#' @param subj Optional argument. Allows the user to specify and subset specific subjects
#' from the \code{ID} column of the supplied data set. The \code{subj} argument can be a single
#' value or a vector of elements. The input type should be character, but the function will
#' comply with integers so long as they are all present in the \code{ID} column of the data.
#'
#' @param group_var A categorical column of the input data set that the individual points are to
#' be grouped / separated by for a given plot. Cannot contain more than 10 levels (to avoid
#' overcrowding the plot). This is different from the \code{wrap_var} argument which segments
#' plots by category. The default value is NULL (no grouping).
#'
#' @param wrap_var A categorical column of the input data set that the plots are to be segmented
#' by. If there are multiple levels such as time of day, or visit number, the output will include a
#' matrix with each plot corresponding to an individual level. This differs from the \code{group_var}
#' argument which separates data within the same plot. The default value is NULL (no wrapping).
#'
#' @param inc_low A TRUE / FALSE indicator of whether or not to include the "Low" (Hypotension)
#' category to the scatter plot. This is only activated in conjunction with \code{"stages2020"} plot type, and if TRUE is defined as SBP < 100 and DBP > 60. If FALSE, those values are displayed as "Normal". This argument is ignored with plot type \code{"AHA"}, where the "Low" stage (SBP < 100 and DBP < 60) is always displayed.
#'
#' @param inc_crisis A TRUE / FALSE indicator of whether or not to include the "Crisis" (Hypertensive)
#' category to the scatter plot. This is only activated in conjunction with \code{"stages2020"} plot type, and if TRUE is defined as SBP > 180 or DBP > 120. If FALSE, those values are displayed as either "ISH - S2", "S2" or "IDH - S2" stages (see details). This argument is ignored with plot type \code{"AHA"}, where the "Crisis" stage (SBP > 180 or DBP > 120) is always displayed.
#'
#' @return A scatter plot graphic using the ggplot2 package overlaying each reading (represented as
#' points) onto a background that contains each stage
#'
#' @details There are eight total stages according to Lee et al (2020) with the options
#' to include two additional categories for "Low" (Hypotension) and Hypertensive "Crisis". The
#' categories are as follows:
#'
#'\itemize{
#'
#' \item \code{Low} - (Optional) Legacy category for consistency with AHA stages. According to the AHA, low blood pressure is any reading with SBP < 100 and DBP < 60, and is depicted in light blue in the scatter plot. This is always displayed in \code{"AHA"} plot, and can be displayed in \code{"stages2020"} plot by setting \code{inc_low = TRUE}.
#'
#' \item \code{Normal} -  \code{SBP} readings less than 120 and \code{DBP} readings
#' less than 80. Reading within this range that either have SBP > 100 or DBP > 60 are also considered Normal by AHA. Normal BP is depicted in green in the scatter plot.
#'
#' \item \code{Elevated} - \code{SBP} readings between 120 - 129 and \code{DBP}
#' readings less than 80. Coincides with Elevated stage as defined by AHA. Without intervention to control the condition, individuals are likely to develop Hypertension. Elevated BP is depicted in yellow in the scatter plot.
#'
#' \item \code{Stage 1 - All (SDH)} - \code{SBP} readings between 130 - 139 and \code{DBP}
#' readings between 80 - 89. Stage 1 Hypertension will typically result in doctors prescribing
#' medication or lifestyle changes.  Stage 1 BP is depicted in dark orange in the scatter plot. These readings correspond to Stage 1 as defined by AHA.
#'
#' \item \code{Stage 1 - Isolated Diastolic Hypertension (IDH)} - \code{SBP} readings
#' less than 130, but \code{DBP} readings between 80 - 89. This alternative stage 1 level accounts
#' for unusually high diastolic readings, but fairly normal systolic readings and is depicted in
#' orange in the plot. These readings correspond to Stage 1 as defined by AHA.
#'
#' \item \code{Stage 1 - Isolated Systolic Hypertension (ISH)} - \code{SBP} readings
#' between 130 - 139, but \code{DBP} readings less than 80. This alternative stage 1 level accounts
#' for unusually high systolic readings, but fairly normal diastolic readings and is depicted in
#' orange in the plot. These readings correspond to Stage 1 as defined by AHA.
#'
#' \item \code{Stage 2 - All (SDH)} - \code{SBP} readings between 140 - 180 and \code{DBP} readings
#' between 90 - 120. Stage 2 Hypertension will typically result in doctors prescribing both
#' medication and lifestyle changes. Stage 2 BP is depicted in bright red in the scatter plot. These readings correspond to Stage 2 as defined by AHA.
#'
#' \item \code{Stage 2 - Isolated Diastolic Hypertension (IDH)} - \code{SBP} readings
#' less than or equal to 140, but \code{DBP} readings greater than or equal to 90. This alternative
#' stage 2 level accounts for unusually high diastolic readings, but fairly normal systolic readings
#' and is depicted in red. These readings correspond to Stage 2 as defined by AHA.
#'
#' \item \code{Stage 2 - Isolated Systolic Hypertension (IDH)} - \code{SBP} readings
#' greater than or equal to 140, but \code{DBP} readings less or equal to 90. This alternative
#' stage 2 level accounts for unusually high systolic readings, but fairly normal diastolic readings
#' and is depicted in red. These readings correspond to Stage 2 as defined by AHA.
#'
#' \item \code{Crisis} - (Optional) Legacy category for consistency with AHA stages. According to the AHA, hypertensive crisis is defined as a \code{SBP} reading exceeding 180 or a
#' \code{DBP} reading exceeding 120. This stage requires medical attention immediately.
#' Crisis is depicted in red in the scatter plot. This is always displayed in \code{"AHA"} plot, and can be displayed in \code{"stages2020"} plot by setting \code{inc_crisis = TRUE}.
#'
#' }
#'
#'
#' @references
#' Lee H, Yano Y, Cho SMJ, Park JH, Park S, Lloyd-Jones DM, Kim HC. Cardiovascular risk of isolated
#' systolic or diastolic hypertension in young adults. \emph{Circulation}. 2020; 141:1778–1786.
#' \doi{10.1161/CIRCULATIONAHA.119.044838}
#'
#' Unger, T., Borghi, C., Charchar, F., Khan, N. A., Poulter, N. R., Prabhakaran, D., ... & Schutte,
#' A. E. (2020). 2020 International Society of Hypertension global hypertension practice guidelines.
#' \emph{Hypertension}, 75(6), 1334-1357.
#' \doi{10.1161/HYPERTENSIONAHA.120.15026}
#'
#' @export
#'
#' @examples
#' data("bp_jhs")
#' data("bp_hypnos")
#' data("bp_ghana")
#' hypnos_proc <- process_data(bp_hypnos,
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
#' # HYPNOS Data
#' bp_scatter(hypnos_proc,
#'            inc_crisis = TRUE,
#'            inc_low = TRUE,
#'            group_var = "wake",
#'            wrap_var = "day_of_week")
#'
#' # JHS Data
#' bp_scatter(jhs_proc,
#'            plot_type = "AHA",
#'            group_var = "time_of_day")
#'
#' # Ghana Data Set
#' #(Note that column names are of proper naming convention so no processing needed)
#' bp_scatter(bp::bp_ghana, inc_crisis = TRUE, inc_low = FALSE, group_var = "TIME_ELAPSED")
bp_scatter <- function(data,
                           plot_type = c("stages2020", "AHA"),
                           subj = NULL,
                           group_var = NULL,
                           wrap_var = NULL,
                           inc_crisis = TRUE,
                           inc_low = TRUE){

  # Set global variables
  SBP = DBP = VISIT = ID = NULL
  rm(list = c("SBP", "DBP", "VISIT", "ID"))

  # Force to dataframe
  data <- as.data.frame(data)

  # Upper case all column names
  colnames(data) <- toupper(colnames(data))

  # Coerce user input to upper case and match with either 8 stage thresholds or the AHA guidelines
  #plot_type = toupper(plot_type)
  plot_type = match.arg(plot_type)

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

  # Remove NA / NULL values
  data <- data[complete.cases(data[ , c('SBP', 'DBP')]), ]

  # Grouping Variable for WITHIN plots
  if(!is.null(group_var)){

    group_var <- toupper( group_var )

    if( (group_var %in% toupper( colnames(data) ) ) == FALSE){
      stop('group_var not found in data set. Ensure that spelling matches column name in data set.')
    }

    if( length( unique( data[[group_var]] ) ) > 11 ){
      stop('group_var must be categorical with no more than 11 groups')
    }

    #print("Grouping Variable: ", group_var, sep = "")

  }

  # Wrap variable for facet_wrap plots (multi-plots by group)
  if(!is.null(wrap_var)){

    wrap_var <- toupper( wrap_var )

    if( (wrap_var %in% toupper( colnames(data) ) ) == FALSE){
      stop('wrap_var not found in data set. Ensure that spelling matches column name in data set.')
    }

    #print("Wrapping Variable: ", wrap_var, sep = "")
  }

  # Ensure that the necessary columns exist in data set
  if( all(c("SBP", "DBP") %in% names(data)) == FALSE){
    stop('One or more of the required variables are missing. \nEnsure that you have run the process_data() function first.')
  }

  # 8 stage thresholds according to Lee et al 2020
  if(plot_type == "stages2020"){

    # Initialize IDH - S1, Elevated, and ISH - S1 as they never change
    xlim_breaks <- c(80, 90)
    ylim_breaks <- c(120, 130, 140)

    # precalculate minDBP and minSBP values
    minDBP = min(data$DBP, na.rm = TRUE)
    minSBP = min(data$SBP, na.rm = TRUE)

    # Check whether user wants to include a 'Low (Hypotension)' category
    if( inc_low == TRUE ){

      low_x_lim <- c( floor(min(25, minDBP - 10)), 60)
      low_y_lim <- c( floor(min(80, minSBP - 10)), 100)

      norm_x_lim <- c(60, 80)
      norm_y_lim <- c(100, 120)

      xlim_breaks <- c(low_x_lim, xlim_breaks)
      ylim_breaks <- c(low_y_lim, ylim_breaks)

    }else{

      xlim_breaks <- c( floor(min(25, minDBP - 10)), xlim_breaks )
      ylim_breaks <- c( floor(min(80, minSBP - 10)), ylim_breaks )

      norm_x_lim <- c( floor(min(25, minDBP - 10)) , 80)
      norm_y_lim <- c( floor(min(80, minSBP - 10)) , 120)

    }


    # Check whether user wants to include a hypertensive 'Crisis' category
    if( inc_crisis == TRUE ){

      crisis_x_lim <- c(120, max(140, max(data$DBP, na.rm = TRUE) + 10) )
      crisis_y_lim <- c(180, max(200, max(data$SBP, na.rm = TRUE) + 10) )

      s2_x_lim <- c(90, 120)
      s2_y_lim <- c(140, 180)

      xlim_breaks <- c( xlim_breaks, crisis_x_lim)
      xlim_breaks <- ceiling(xlim_breaks)

      ylim_breaks <- c( ylim_breaks, crisis_y_lim)
      ylim_breaks <- ceiling(ylim_breaks)

    }else{

      xlim_breaks <- c(xlim_breaks, max(120, max(data$DBP, na.rm = TRUE) + 10) )
      xlim_breaks <- ceiling(xlim_breaks)

      ylim_breaks <- c(ylim_breaks, max(140, max(data$SBP, na.rm = TRUE) + 10) )
      ylim_breaks <- ceiling(ylim_breaks)

      s2_x_lim <- c( xlim_breaks[length(xlim_breaks)-1], xlim_breaks[length(xlim_breaks)] )
      s2_y_lim <- c( ylim_breaks[length(ylim_breaks)-1], ylim_breaks[length(ylim_breaks)] )

    }


    # Calculate length of breaks to prestore
    xlim_breaks_length = length(xlim_breaks)
    ylim_breaks_length = length(ylim_breaks)

    scat <- ggplot(data, aes(DBP, SBP)) +

      # Give user option to adjust breaks
      scale_y_continuous(breaks = ylim_breaks) +
      scale_x_continuous(breaks = xlim_breaks) +


            ## Categories

            # Low (Hypotension) & Normal Categories
            {if( inc_low == FALSE ) annotate("rect", xmin = xlim_breaks[1],
                                                     xmax = xlim_breaks[2],
                                                     ymin = ylim_breaks[1],
                                                     ymax = ylim_breaks[2],
                                                     fill = 'darkgreen',    alpha = .5)} +

            {if( inc_low == TRUE ) annotate("rect", xmin = xlim_breaks[1],
                                                    xmax = xlim_breaks[2],
                                                    ymin = ylim_breaks[1],
                                                    ymax = ylim_breaks[2],
                                                    fill = 'lightblue')} +

            {if( inc_low == TRUE ) annotate("rect", xmin = xlim_breaks[1],
                                                    xmax = xlim_breaks[3],
                                                    ymin = ylim_breaks[2],
                                                    ymax = ylim_breaks[3],
                                                    fill = 'darkgreen',    alpha = .5)} +

            {if( inc_low == TRUE ) annotate("rect", xmin = xlim_breaks[2],
                                                    xmax = xlim_breaks[3],
                                                    ymin = ylim_breaks[1],
                                                    ymax = ylim_breaks[2],
                                                    fill = 'darkgreen',    alpha = .5)} +


              # Elevated
              annotate("rect", xmin = xlim_breaks[1], xmax = 80,  ymin = 120, ymax = 130, fill = 'yellow',     alpha = .5) +

              # Stage 1 - All
              annotate("rect", xmin = 80, xmax = 90,  ymin = 130, ymax = 140, fill = 'orangered1',     alpha = .67) +
              # Stage 1 - ISH
              annotate("rect", xmin = xlim_breaks[1], xmax = 80,  ymin = 130, ymax = 140, fill = 'orange',    alpha = .5) +
              # Stage 1 - IDH
              annotate("rect", xmin = 80, xmax = 90,  ymin = ylim_breaks[1], ymax = 130, fill = 'orange',    alpha = .5) +


          ################################
          #        INCLUDE CRISIS        #
          ################################

            # Crisis - All
            {if( inc_crisis == TRUE )
              annotate("rect", xmin = xlim_breaks[xlim_breaks_length - 1],
                               xmax = xlim_breaks[xlim_breaks_length],
                               ymin = ylim_breaks[ylim_breaks_length - 1],
                               ymax = ylim_breaks[ylim_breaks_length],
                               fill = 'darkred',    alpha = .5) }+

            # Crisis - Dias
            {if( inc_crisis == TRUE )
              annotate("rect", xmin = xlim_breaks[xlim_breaks_length - 1],
                               xmax = xlim_breaks[xlim_breaks_length],
                               ymin = ylim_breaks[1],
                               ymax = ylim_breaks[ylim_breaks_length - 1],
                               fill = 'darkred',    alpha = .5) }+

            # Crisis - Sys
            {if( inc_crisis == TRUE )
              annotate("rect", xmin = xlim_breaks[1],
                               xmax = xlim_breaks[xlim_breaks_length - 1],
                               ymin = ylim_breaks[ylim_breaks_length - 1],
                               ymax = ylim_breaks[ylim_breaks_length],
                               fill = 'darkred',    alpha = .5) }+

            # Stage 2 - All
            {if( inc_crisis == TRUE )
              annotate("rect", xmin = xlim_breaks[xlim_breaks_length - 2],
                               xmax = xlim_breaks[xlim_breaks_length - 1],
                               ymin = ylim_breaks[ylim_breaks_length - 2],
                               ymax = ylim_breaks[ylim_breaks_length - 1],
                               fill = 'red',    alpha = .5)} +

            # Stage 2 - Isolated Diatolic
            {if( inc_crisis == TRUE )
              annotate("rect", xmin = xlim_breaks[xlim_breaks_length - 2],
                               xmax = xlim_breaks[xlim_breaks_length - 1],
                               ymin = ylim_breaks[1],
                               ymax = ylim_breaks[ylim_breaks_length - 2],
                               fill = 'red3',    alpha = .55)} +
            # Stage 2 - Isolated Systolic
            {if( inc_crisis == TRUE )
              annotate("rect", xmin = xlim_breaks[1],
                               xmax = xlim_breaks[xlim_breaks_length - 2],
                               ymin = ylim_breaks[ylim_breaks_length - 2],
                               ymax = ylim_breaks[ylim_breaks_length - 1],
                               fill = 'red3',    alpha = .55)} +


            ################################
            #        EXCLUDE CRISIS        #
            ################################

              # Stage 2 - All
              {if( inc_crisis == FALSE )
                annotate("rect", xmin = xlim_breaks[xlim_breaks_length- 1],
                                 xmax = xlim_breaks[xlim_breaks_length],
                                 ymin = ylim_breaks[ylim_breaks_length - 1],
                                 ymax = ylim_breaks[ylim_breaks_length],
                                 fill = 'red3',    alpha = .55)} +

              # Stage 2 - Isolated Diastolic
              {if( inc_crisis == FALSE )
                annotate("rect", xmin = xlim_breaks[xlim_breaks_length - 1],
                                 xmax = xlim_breaks[xlim_breaks_length],
                                 ymin = ylim_breaks[1],
                                 ymax = ylim_breaks[ylim_breaks_length - 1],
                                 fill = 'red',    alpha = .45)} +

              # Stage 2 - Isolated Systolic
              {if( inc_crisis == FALSE )
                annotate("rect", xmin = xlim_breaks[1],
                                 xmax = xlim_breaks[xlim_breaks_length - 1],
                                 ymin = ylim_breaks[ylim_breaks_length - 1],
                                 ymax = ylim_breaks[ylim_breaks_length],
                                 fill = 'red',    alpha = .45)} +


      # If group_var column NOT present or # groups in group_vars = 1
      {if( is.null(group_var) | length( group_var %in% names(data) ) == 0 ) geom_point(color = 'blue', size = 1)} +

      # If group_var column present
      {if( !is.null(group_var) & length( group_var %in% names(data) ) >= 1 ) geom_point(aes(color = factor(get(group_var))), size = 1) } +
      {if( !is.null(group_var) & length( group_var %in% names(data) ) >= 1 ) scale_color_brewer(type = 'div', palette = 'Paired', na.translate = FALSE)} +
      {if( !is.null(group_var) & length( group_var %in% names(data) ) >= 1 ) guides(color=guide_legend(title=group_var)) } +

      # If wrap_var column present
      {if( !is.null(wrap_var) & length( wrap_var %in% names(data) ) >= 1 ) facet_wrap( as.formula(paste("~", wrap_var)) )} +


      ### Stage labels

      # Low (Hypotension)
      {if(inc_low == TRUE) geom_text(aes(x = min(xlim_breaks) + 5, y = low_y_lim[2], label = 'Low'), color = 'black', hjust = .35, vjust = 2, size = 3, check_overlap = TRUE) }+

      # Normal
      geom_text(aes(x = min(xlim_breaks) + 5, y = norm_y_lim[2], label = 'Normal'), color = 'black', hjust = .35, vjust = 2, size = 3, check_overlap = TRUE) +

      # Elevated
      geom_text(aes(x = min(xlim_breaks) + 5, y = 130, label = 'Elevated'), color = 'black', hjust = .35, vjust = 1.5, size = 3, check_overlap = TRUE) +

      # SDH - Stage 1 - All
      geom_text(aes(x = 90, y = 140, label = 'S1'), color = 'black', hjust = 1.5, vjust = 1.5, size = 3, check_overlap = TRUE) +

      # SDH - Stage 2 - All
      geom_text(aes(x = s2_x_lim[2] - 5, y = s2_y_lim[2] - 5, label = 'S2'), color = 'black', hjust = 1.5, vjust = 1.5, size = 3, check_overlap = TRUE) +

      # ******************************************************************************************************************************** #

      # ISH - Stage 1
      geom_text(aes(x = min(xlim_breaks) + 5, y = 140, label = 'ISH - S1'), color = 'black', hjust = .35, vjust = 1.5, size = 3, check_overlap = TRUE) +

      # ISH - Stage 2
      geom_text(aes(x = min(xlim_breaks) + 5, y = s2_y_lim[2] - 5, label = 'ISH - S2'), color = 'black', hjust = .35, vjust = 1.5, size = 3, check_overlap = TRUE) +

      # ******************************************************************************************************************************** #

      # IDH - Stage 1
      geom_text(aes(x = 90, y = 90, label = 'IDH\n S1'), color = 'black', hjust = 1.25, size = 3, check_overlap = TRUE) +

      # IDH - Stage 2
      geom_text(aes(x = s2_x_lim[2] - 5, y = 90, label = 'IDH\n S2'), color = 'black', hjust = 1.25, size = 3, check_overlap = TRUE) +

      # Crisis
      {if(inc_crisis == TRUE) geom_text(aes(x = xlim_breaks[xlim_breaks_length], y = ylim_breaks[ylim_breaks_length], label = 'Crisis'), color = 'black', hjust = 1.25, vjust = 1.5, size = 3, check_overlap = TRUE) }+

      # Main Title & Subtitle
      ggtitle('Scatterplot of BP Values', subtitle = 'Source: Lee et al (2020)')




  }else{

    ##############################################################
    # Compatibility Check for user-supplied stages if applicable

    #sbp_breaks <- stage_check(sbp_stages_alt, dbp_stages_alt)[[1]] # deprecated
    #dbp_breaks <- stage_check(sbp_stages_alt, dbp_stages_alt)[[2]] # deprecated

    sbp_breaks <- c(80,100,120,130,140,180,200)
    dbp_breaks <- c(25,60,80,85,90,120,140)

    ##############################################################


    # Scatterplot of bp stages

    scat <- ggplot(data, aes(DBP, SBP)) +

      # Give user option to adjust breaks
      scale_y_continuous(breaks = sbp_breaks) +
      scale_x_continuous(breaks = dbp_breaks) +

      # Y axis bars:
      annotate("rect", xmin = dbp_breaks[1], xmax = dbp_breaks[2],  ymin = sbp_breaks[1], ymax = sbp_breaks[2], fill = 'lightblue') +
      annotate("rect", xmin = dbp_breaks[1], xmax = dbp_breaks[3],  ymin = sbp_breaks[2], ymax = sbp_breaks[3], fill = 'darkgreen',  alpha = .5) +
      annotate("rect", xmin = dbp_breaks[1], xmax = dbp_breaks[3],  ymin = sbp_breaks[3], ymax = sbp_breaks[4], fill = 'yellow',     alpha = .5) +
      annotate("rect", xmin = dbp_breaks[1], xmax = dbp_breaks[5],  ymin = sbp_breaks[4], ymax = sbp_breaks[5], fill = 'orange',     alpha = .5) +
      annotate("rect", xmin = dbp_breaks[1], xmax = dbp_breaks[6],  ymin = sbp_breaks[5], ymax = sbp_breaks[6], fill = 'red3',    alpha = .5) +
      annotate("rect", xmin = dbp_breaks[1], xmax = dbp_breaks[7],  ymin = sbp_breaks[6], ymax = sbp_breaks[7], fill = 'darkred',        alpha = .5) +

      # X axis bars
      annotate("rect", xmin = dbp_breaks[2], xmax = dbp_breaks[3], ymin = sbp_breaks[1], ymax = sbp_breaks[2], fill = 'darkgreen',  alpha = .5) +
      # annotate("rect", xmin = dbp_breaks[3], xmax = dbp_breaks[3], ymin = sbp_breaks[1], ymax = sbp_breaks[3], fill = 'yellow',     alpha = .5) +
      annotate("rect", xmin = dbp_breaks[3], xmax = dbp_breaks[5], ymin = sbp_breaks[1], ymax = sbp_breaks[4], fill = 'orange',     alpha = .5) +
      annotate("rect", xmin = dbp_breaks[5], xmax = dbp_breaks[6], ymin = sbp_breaks[1], ymax = sbp_breaks[5], fill = 'red3',    alpha = .5) +
      annotate("rect", xmin = dbp_breaks[6], xmax = dbp_breaks[7], ymin = sbp_breaks[1], ymax = sbp_breaks[6], fill = 'darkred',        alpha = .5) +

      # Add stage labels
      geom_text(aes(x = dbp_breaks[1] + 5, y = sbp_breaks[2], label = 'Low'), color = 'black', hjust = .35, vjust = 2, size = 3, check_overlap = TRUE) +
      geom_text(aes(x = dbp_breaks[1] + 5, y = sbp_breaks[3], label = 'Normal'), color = 'black', hjust = .35, vjust = 2, size = 3, check_overlap = TRUE) +
      geom_text(aes(x = dbp_breaks[1] + 5, y = sbp_breaks[4], label = 'Elevated'), color = 'black', hjust = .35, vjust = 1.5, size = 3, check_overlap = TRUE) +
      geom_text(aes(x = dbp_breaks[1] + 5, y = sbp_breaks[5], label = 'Stage 1'), color = 'black', hjust = .35, vjust = 1.5, size = 3, check_overlap = TRUE) +
      geom_text(aes(x = dbp_breaks[1] + 5, y = sbp_breaks[6] - ( (sbp_breaks[6] - sbp_breaks[5]) / 2), label = 'Stage 2'), color = 'black', hjust = .35, vjust = 1.5, size = 3, check_overlap = TRUE) +
      geom_text(aes(x = dbp_breaks[1] + 5, y = sbp_breaks[7], label = 'Crisis'), color = 'black', hjust = .35, vjust = 1.5, size = 3, check_overlap = TRUE) +

      # Add title
      ggtitle('Scatterplot of BP Values', subtitle = 'Source: American Heart Association') +

      # If group_var column NOT present or # groups in group_vars = 1
      {if( is.null(group_var) | length( group_var %in% names(data) ) == 0 ) geom_point(color = 'blue', size = 1)} +

      # If group_var column present
      {if( !is.null(group_var) & length( group_var %in% names(data) ) >= 1 ) geom_point(aes(color = factor(get(group_var))), size = 1) } +
      {if( !is.null(group_var) & length( group_var %in% names(data) ) >= 1 ) scale_color_brewer(type = 'div', palette = 'Paired', na.translate = FALSE)} +
      {if( !is.null(group_var) & length( group_var %in% names(data) ) >= 1 ) guides(color=guide_legend(title=group_var)) }

    # If wrap_var column present
    {if( !is.null(wrap_var) & length( wrap_var %in% names(data) ) >= 1 ) facet_wrap( as.formula(paste("~", wrap_var)) )}

  }


  return(scat)

}
