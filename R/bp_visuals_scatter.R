

#' Blood Pressure Stage Scatter Plot
#'
#' @description Display all \code{SBP} and \code{DBP}
#' readings on a scatterplot with deliniation of BP according to the 8
#' mutually exclusive levels of Hypertension as in Lee et al (2020) (the default), or the
#' levels set by the American Heart Association (AHA).
#'
#' @inheritParams process_data
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
#'
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
#' systolic or diastolic hypertension in young adults. \emph{Circulation}. 2020; 141:1778-1786.
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
                       inc_low = TRUE,
                       bp_cutoffs = list( c(100, 120, 130, 140, 180), c(60, 80, 80, 90, 120)),
                       bp_type = c("hbpm", "abpm", "ap")){

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
  bp_type <- tolower(bp_type)
  bp_type <- toupper( match.arg(bp_type) )

  # Fix this in the long term
  if(plot_type == "stages2020"){
    plot_type = "Lee_2020"
  }

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

  ### BEGIN PLOTS

  # For plots
  source_dsc <- ifelse(plot_type == "Lee_2020", 'Source: Lee et al (2020)', 'Source: American Heart Association')

  # Need bp_type, guidelines (plot_type), and bp_cutoffs
  stage_lookup = stage_lookup_v1(bp_type = bp_type, guidelines = plot_type, bp_cutoffs = bp_cutoffs,
                                 inc_low = inc_low, inc_crisis = inc_crisis)

  # Color Map
  color_map = data.frame(
    Stages = c("Low", "Normal", "Elevated", "Stage 1", "ISH - S1", "IDH - S1", "Stage 2", "ISH - S2", "IDH - S2", "Crisis"),
    colors_map = c('lightblue', 'darkgreen', 'yellow', 'orangered1', 'orange', 'orange', 'red', 'red3', 'red3', 'darkred'),
    row_order = c(1:10),
    text_x = c(0, 0, 0,
               stage_lookup$dbp_UL[which(stage_lookup$Stages %in% "Stage 1")-1],
               0,
               stage_lookup$dbp_UL[which(stage_lookup$Stages %in% "Stage 1")-1],
               stage_lookup$dbp_UL[which(stage_lookup$Stages %in% "Stage 2")-1],
               0,
               stage_lookup$dbp_UL[which(stage_lookup$Stages %in% "Stage 2")-1],
               ifelse("Crisis" %in% stage_lookup$Stages, stage_lookup$dbp_UL[which(stage_lookup$Stages %in% "Crisis")]-15, NA )
    ),

    text_y = c(ifelse("Low" %in% stage_lookup$Stages, stage_lookup$sbp_UL[which(stage_lookup$Stages %in% "Low")], NA ),
               stage_lookup$sbp_UL[which(stage_lookup$Stages %in% "Normal")],
               stage_lookup$sbp_UL[which(stage_lookup$Stages %in% "Elevated")],
               stage_lookup$sbp_UL[which(stage_lookup$Stages %in% "Stage 1")],
               stage_lookup$sbp_UL[which(stage_lookup$Stages %in% "Stage 1")],
               0,
               stage_lookup$sbp_UL[which(stage_lookup$Stages %in% "Stage 2")],
               stage_lookup$sbp_UL[which(stage_lookup$Stages %in% "Stage 2")],
               0,
               ifelse("Crisis" %in% stage_lookup$Stages, stage_lookup$sbp_UL[which(stage_lookup$Stages %in% "Crisis")], NA )
    )
  )

  # Filter based only on what stages are included in stage_lookup
  color_map = color_map[ color_map[ , 1] %in% stage_lookup$Stages, ]
  row.names(color_map) <- NULL
  color_map$row_order <- as.integer(row.names(color_map))

  # Add colors to stage_lookup table
  stage_lookup = merge(stage_lookup, color_map, by = 'Stages')
  stage_lookup = stage_lookup[order(stage_lookup$row_order),]
  row.names(stage_lookup) <- NULL


  # Plot limits for SBP/DBP
  sbp_breaks = c(0, sort(unique(stage_lookup$sbp_UL)) )
  dbp_breaks =  c(0, sort(unique(stage_lookup$dbp_UL)) )

  sbp_breaks_len = length(sbp_breaks)
  dbp_breaks_len = length(dbp_breaks)

  # Create scatterplot
  scat <- ggplot(data, aes(DBP, SBP)) +

    # Give user option to adjust breaks
    scale_y_continuous(breaks = sbp_breaks) +
    scale_x_continuous(breaks = dbp_breaks)

  # Add stage sections
  for(i in nrow(stage_lookup):1){
    scat <- scat + annotate("rect",
                            xmin = stage_lookup$dbp_LL[i],
                            xmax = stage_lookup$dbp_UL[i],
                            ymin = stage_lookup$sbp_LL[i],
                            ymax = stage_lookup$sbp_UL[i],
                            fill = stage_lookup$colors_map[i],
                            alpha = 0.5)
  }

  if(plot_type == "AHA"){

    # Fill vertically for Normal, Stage 1, Stage 2, and Crisis
    for(i in which(stage_lookup$Stages %in% c("Normal", "Stage 1", "Stage 2") )){
      scat <- scat + annotate("rect",
                              xmin = stage_lookup$dbp_LL[i],
                              xmax = stage_lookup$dbp_UL[i],
                              ymin = 0,
                              ymax = stage_lookup$sbp_LL[i],
                              fill = stage_lookup$colors_map[i],
                              alpha = 0.5)

      scat <- scat + annotate("rect",
                              xmin = 0,
                              xmax = stage_lookup$dbp_LL[i],
                              ymin = stage_lookup$sbp_LL[i],
                              ymax = stage_lookup$sbp_UL[i],
                              fill = stage_lookup$colors_map[i],
                              alpha = 0.5)
    }

  }else if(plot_type == "Lee_2020"){

    # Adjust Normal
    stage_name = "Normal"

    scat <- scat + annotate("rect",
                            xmin = stage_lookup[ which(stage_lookup$Stages %in% stage_name), ]$dbp_LL,
                            xmax = stage_lookup[ which(stage_lookup$Stages %in% stage_name), ]$dbp_UL,
                            ymin = 0,
                            ymax = stage_lookup[ which(stage_lookup$Stages %in% stage_name), ]$sbp_LL,
                            fill = stage_lookup[ which(stage_lookup$Stages %in% stage_name), ]$colors_map,
                            alpha = 0.5)

    scat <- scat + annotate("rect",
                            xmin = 0,
                            xmax = stage_lookup[ which(stage_lookup$Stages %in% stage_name), ]$dbp_LL,
                            ymin = stage_lookup[ which(stage_lookup$Stages %in% stage_name), ]$sbp_LL,
                            ymax = stage_lookup[ which(stage_lookup$Stages %in% stage_name), ]$sbp_UL,
                            fill = stage_lookup[ which(stage_lookup$Stages %in% stage_name), ]$colors_map,
                            alpha = 0.5)
  }

  if( (inc_crisis == TRUE) & ("Crisis" %in% stage_lookup$Stages) ){

    stage_name = "Crisis"

    scat <- scat + annotate("rect",
                            xmin = stage_lookup[ which(stage_lookup$Stages %in% stage_name), ]$dbp_LL,
                            xmax = stage_lookup[ which(stage_lookup$Stages %in% stage_name), ]$dbp_UL,
                            ymin = 0,
                            ymax = stage_lookup[ which(stage_lookup$Stages %in% stage_name), ]$sbp_LL,
                            fill = stage_lookup[ which(stage_lookup$Stages %in% stage_name), ]$colors_map,
                            alpha = 0.5)

    scat <- scat + annotate("rect",
                            xmin = 0,
                            xmax = stage_lookup[ which(stage_lookup$Stages %in% stage_name), ]$dbp_LL,
                            ymin = stage_lookup[ which(stage_lookup$Stages %in% stage_name), ]$sbp_LL,
                            ymax = stage_lookup[ which(stage_lookup$Stages %in% stage_name), ]$sbp_UL,
                            fill = stage_lookup[ which(stage_lookup$Stages %in% stage_name), ]$colors_map,
                            alpha = 0.5)

  }

  # Add Labels:

  if(plot_type == "Lee_2020"){

    for(i in 1:nrow(stage_lookup)){

      scat <- scat + geom_text(x = stage_lookup$text_x[i] + 5, y = stage_lookup$text_y[i],
                               label = stage_lookup$Stages[i], color = 'black',
                               hjust = .35, vjust = 2, size = 2.5, check_overlap = TRUE)
    }


  }else if(plot_type == "AHA"){

    for(i in 1:nrow(stage_lookup)){

      scat <- scat + geom_text(x = stage_lookup$dbp_LL[1] + 5, y = stage_lookup$sbp_UL[i],
                               label = stage_lookup$Stages[i], color = 'black',
                               hjust = .35, vjust = 2, size = 2.5, check_overlap = TRUE)
    }

  }

  # Add Data:

  # If group_var column NOT present or # groups in group_vars = 1
  scat <- scat + {if( is.null(group_var) | length( group_var %in% names(data) ) == 0 ) geom_point(color = 'blue', size = 1)} +

    # If group_var column present
    {if( !is.null(group_var) & length( group_var %in% names(data) ) >= 1 ) geom_point(aes(color = factor(get(group_var))), size = 1) } +
    {if( !is.null(group_var) & length( group_var %in% names(data) ) >= 1 ) scale_color_brewer(type = 'div', palette = 'Paired', na.translate = FALSE)} +
    {if( !is.null(group_var) & length( group_var %in% names(data) ) >= 1 ) guides(color=guide_legend(title=group_var)) } +

    # If wrap_var column present
    {if( !is.null(wrap_var) & length( wrap_var %in% names(data) ) >= 1 ) facet_wrap( as.formula(paste("~", wrap_var)) )} +

    # Add title
    ggtitle('Scatterplot of BP Values', subtitle = source_dsc) +

    # If group_var column NOT present or # groups in group_vars = 1
    {if( is.null(group_var) | length( group_var %in% names(data) ) == 0 ) geom_point(color = 'blue', size = 1)} +

    # If group_var column present
    {if( !is.null(group_var) & length( group_var %in% names(data) ) >= 1 ) geom_point(aes(color = factor(get(group_var))), size = 1) } +
    {if( !is.null(group_var) & length( group_var %in% names(data) ) >= 1 ) scale_color_brewer(type = 'div', palette = 'Paired', na.translate = FALSE)} +
    {if( !is.null(group_var) & length( group_var %in% names(data) ) >= 1 ) guides(color=guide_legend(title=group_var)) }

  # If wrap_var column present
  {if( !is.null(wrap_var) & length( wrap_var %in% names(data) ) >= 1 ) facet_wrap( as.formula(paste("~", wrap_var)) )}

  return(scat)

}
