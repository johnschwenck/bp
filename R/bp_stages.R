#' Alternative Blood Pressure Stages
#'
#' Adds SBP_Category and DBP_Category columns to supplied dataframe.
#'
#' Supplied dataframe must adhere to the unified format using the \code{process_data} function.
#'
#' @param data User-supplied dataset containing blood pressure data. Must
#' contain data for Systolic blood pressure and Diastolic blood pressure at a
#' minimum.
#'
#' @param sbp column name corresponding to systolic blood pressure (SBP)
#'
#' @param dbp column name corresponding to diastolic blood pressure (DBP)
#'
#' @param inc_low A TRUE / FALSE indicator of whether or not to include the "Low" (Hypotension)
#' category to the scatter plot. The range for Hypotension is set from a minimum of 25 for DBP or 80
#' for SBP, or the corresponding minimum value for either category from the data until 60 for DBP and
#' 100 for SBP.
#'
#' @param inc_crisis A TRUE / FALSE indicator of whether or not to include the Hypertensive "Crisis"
#' category to the scatter plot. The range for crisis is any value above 180 for SBP or above 120 for
#' DBP.
#'
#' @param data_screen Default to TRUE. data_screens for extreme values in the data for both \code{SBP} and \code{DBP}
#' according to Omboni, et al (1995) paper - Calculation of Trough:Peak Ratio of Antihypertensive Treatment
#' from Ambulatory Blood Pressure: Methodological Aspects
#'
#' @param SUL Systolic Upper Limit (SUL). If \code{data_screen = TRUE}, then \code{SUL} sets the upper limit by which
#' to exclude any \code{SBP} values that exceed this threshold. The default is set to 240 per Omboni, et al (1995)
#' paper - Calculation of Trough:Peak Ratio of Antihypertensive Treatment from Ambulatory Blood Pressure:
#' Methodological Aspects
#'
#' @param SLL Systolic Lower Limit (SLL). If \code{data_screen = TRUE}, then \code{SLL} sets the lower limit by which
#' to exclude any \code{SBP} values that fall below this threshold. The default is set to 50 per Omboni, et al (1995)
#' paper - Calculation of Trough:Peak Ratio of Antihypertensive Treatment from Ambulatory Blood Pressure:
#' Methodological Aspects
#'
#' @param DUL Diastolic Upper Limit (DUL). If \code{data_screen = TRUE}, then \code{DUL} sets the upper limit by which
#' to exclude any \code{DBP} values that exceed this threshold. The default is set to 140 per Omboni, et al (1995)
#' paper - Calculation of Trough:Peak Ratio of Antihypertensive Treatment from Ambulatory Blood Pressure:
#' Methodological Aspects
#'
#' @param DLL Diastolic Lower Limit (DLL). If \code{data_screen = TRUE}, then \code{DLL} sets the lower limit by which
#' to exclude any \code{DBP} values that fall below this threshold. The default is set to 40 per Omboni, et al (1995)
#' paper - Calculation of Trough:Peak Ratio of Antihypertensive Treatment from Ambulatory Blood Pressure:
#' Methodological Aspects
#'
#' @return A dataframe with additional columns corresponding to the stages of high blood pressure and the
#' supplementary SBP / DBP categories
#'
#' @references
#' Omboni, S., Parati, G*., Zanchetti, A., Mancia, G. Calculation of trough: peak ratio of
#' antihypertensive treatment from ambulatory blood pressure: methodological aspects
#' \emph{Journal of Hypertension}. October 1995 - Volume 13 - Issue 10 - p 1105-1112
#' \doi{10.1097/00004872-199510000-00005}
#'
#' @export
#'
#' @examples
#' # Load bp_hypnos
#' data(bp_hypnos)
#'
#' bp_stages(bp_hypnos, sbp = "syst", dbp = "diast")
#'
#'
#' # Load bp_jhs data
#' data(bp_jhs)
#'
#' bp_stages(bp_jhs, sbp = "sys.mmhg.", dbp = "dias.mmhg.")
#'
bp_stages <- function(data, sbp, dbp, inc_low = TRUE, inc_crisis = TRUE, data_screen = TRUE, SUL = 240, SLL = 50, DUL = 140, DLL = 40){

  # BP Categories / Stages
  # Only require SBP and DBP

  SBP = DBP = BP_CLASS = HOUR = DATE_TIME = . = NULL
  rm(list = c("SBP", "DBP", "BP_CLASS", "HOUR", "DATE_TIME", "."))

  # Convert all column names to upper case for consistency
  colnames(data) <- toupper(colnames(data))

  # Adjust SBP
  data <- sbp_adj(data = data, sbp = sbp, data_screen = data_screen, SUL = SUL, SLL = SLL)

  # Adjust DBP
  data <- dbp_adj(data = data, dbp = dbp, data_screen = data_screen, DUL = DUL, DLL = DLL)

  # Compatibility Check for user-supplied stages if applicable
  #sbp_stages <- stage_check(sbp_stages_alt, dbp_stages_alt)[[1]]
  #dbp_stages <- stage_check(sbp_stages_alt, dbp_stages_alt)[[2]]




  # Initialize IDH - S1, Elevated, and ISH - S1 as they never change
  xlim_breaks <- c(80, 90)
  ylim_breaks <- c(120, 130, 140)

  opts <- "NONE" # include neither low nor crisis category

  # Check whether user wants to include a 'Low (Hypotension)' category
  if( inc_low == TRUE ){

    low_x_lim <- c( floor(min(25, min(data$DBP, na.rm = TRUE) - 10)), 60)
    low_y_lim <- c( floor(min(80, min(data$SBP, na.rm = TRUE) - 10)), 100)

    norm_x_lim <- c(60, 80)
    norm_y_lim <- c(100, 120)

    xlim_breaks <- c(low_x_lim, xlim_breaks)
    ylim_breaks <- c(low_y_lim, ylim_breaks)

    opts <- "LOW" # include only low category

  }else{

    xlim_breaks <- c( floor(min(25, min(data$DBP, na.rm = TRUE) - 10)), xlim_breaks )
    ylim_breaks <- c( floor(min(80, min(data$SBP, na.rm = TRUE) - 10)), ylim_breaks )

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

    if(opts == "LOW"){
      opts <- "LOW_CRISIS" # include both low and crisis categories
    }else{
      opts <- "CRISIS" # include only crisis category
    }

  }else{

    xlim_breaks <- c(xlim_breaks, max(120, max(data$DBP, na.rm = TRUE) + 10) )
    xlim_breaks <- ceiling(xlim_breaks)

    ylim_breaks <- c(ylim_breaks, max(140, max(data$SBP, na.rm = TRUE) + 10) )
    ylim_breaks <- ceiling(ylim_breaks)

  }




  # Categorize data by stage
  data <- data %>%
    dplyr::mutate( BP_CLASS = dplyr::case_when(

      # Include neither Low nor Crisis categories
      opts == "NONE" ~ dplyr::case_when(

            # Original categories
             SBP < ylim_breaks[2] & DBP < xlim_breaks[2] ~ "Normal",
            (SBP >= ylim_breaks[2] & SBP < ylim_breaks[3]) & (DBP < xlim_breaks[2]) ~ "Elevated",
            (SBP >= ylim_breaks[3] & SBP < ylim_breaks[4]) & (DBP >= xlim_breaks[2] & DBP < xlim_breaks[3]) ~ "Stage 1",
            (SBP >= ylim_breaks[4]) & (DBP >= xlim_breaks[3]) ~ "Stage 2",

            # Isolated categories

            # Stage 1
            (SBP >= ylim_breaks[3] & SBP < ylim_breaks[4]) & (DBP < xlim_breaks[2]) ~ "ISH - S1",
            (SBP < ylim_breaks[3]) & (DBP >= xlim_breaks[2] & DBP < xlim_breaks[3]) ~ "IDH - S1",

            # Stage 2
            (SBP >= ylim_breaks[4]) & (DBP < xlim_breaks[3]) ~ "ISH - S2",
            (SBP < ylim_breaks[4]) & (DBP >= xlim_breaks[3]) ~ "IDH - S2"

      ),


      # Include Low only
      opts == "LOW" ~ dplyr::case_when(

            # Original categories
             SBP < ylim_breaks[2] & DBP < xlim_breaks[2] ~ "Low",

            # All possibilities of normal
            (SBP >= ylim_breaks[2] & SBP < ylim_breaks[3]) & (DBP >= xlim_breaks[2] & DBP < xlim_breaks[3]) |
            (SBP >= ylim_breaks[2] & SBP < ylim_breaks[3]) & (DBP < xlim_breaks[2]) |
            (SBP < ylim_breaks[2]) & (DBP >= xlim_breaks[2] & DBP < xlim_breaks[3]) ~ "Normal",

            (SBP >= ylim_breaks[3] & SBP < ylim_breaks[4]) & (DBP < xlim_breaks[3]) ~ "Elevated",
            (SBP >= ylim_breaks[4] & SBP < ylim_breaks[5]) & (DBP >= xlim_breaks[3] & DBP < xlim_breaks[4]) ~ "Stage 1",
            (SBP >= ylim_breaks[5]) & (DBP >= xlim_breaks[4]) ~ "Stage 2",

            # Isolated categories

            # Stage 1
            (SBP >= ylim_breaks[4] & SBP < ylim_breaks[5]) & (DBP < xlim_breaks[3]) ~ "ISH - S1",
            (SBP < ylim_breaks[4]) & (DBP >= xlim_breaks[3] & DBP < xlim_breaks[4]) ~ "IDH - S1",

            # Stage 2
            (SBP >= ylim_breaks[5]) & (DBP < xlim_breaks[4]) ~ "ISH - S2",
            (SBP < ylim_breaks[5]) & (DBP >= xlim_breaks[4]) ~ "IDH - S2"

      ),


      # Include Crisis only
      opts == "CRISIS" ~ dplyr::case_when(

            # Original categories
             SBP < ylim_breaks[2] & DBP < xlim_breaks[2] ~ "Normal",
            (SBP >= ylim_breaks[2] & SBP < ylim_breaks[3]) & (DBP < xlim_breaks[2]) ~ "Elevated",
            (SBP >= ylim_breaks[3] & SBP < ylim_breaks[4]) & (DBP >= xlim_breaks[2] & DBP < xlim_breaks[3]) ~ "Stage 1",
            (SBP >= ylim_breaks[4] & SBP < ylim_breaks[5]) & (DBP >= xlim_breaks[3] & DBP < xlim_breaks[4]) ~ "Stage 2",

            # Isolated categories

            # Stage 1
            (SBP >= ylim_breaks[3] & SBP < ylim_breaks[4]) & (DBP < xlim_breaks[2]) ~ "ISH - S1",
            (SBP < ylim_breaks[3]) & (DBP >= xlim_breaks[2] & DBP < xlim_breaks[3]) ~ "IDH - S1",

            # Stage 2
            (SBP >= ylim_breaks[4] & SBP < ylim_breaks[5]) & (DBP < xlim_breaks[3]) ~ "ISH - S2",
            (SBP < ylim_breaks[4]) & (DBP >= xlim_breaks[3] & DBP < xlim_breaks[4]) ~ "IDH - S2",

            SBP >= ylim_breaks[5] | DBP >= xlim_breaks[4] ~ "Crisis"

      ),


      # Include both Low and Crisis
      opts == "LOW_CRISIS" ~ dplyr::case_when(

            # Original categories

            # Low
            SBP < ylim_breaks[2] & DBP < xlim_breaks[2] ~ "Low",

            # All possibilities of normal
            (SBP >= ylim_breaks[2]  &  SBP <  ylim_breaks[3]) & (DBP >= xlim_breaks[2] & DBP < xlim_breaks[3]) |
            (SBP >= ylim_breaks[2]  &  SBP <  ylim_breaks[3]) & (DBP <  xlim_breaks[2]) |
            (SBP <  ylim_breaks[2]) & (DBP >= xlim_breaks[2]  &  DBP <  xlim_breaks[3]) ~ "Normal",

            (SBP >= ylim_breaks[3] & SBP < ylim_breaks[4]) & (DBP <  xlim_breaks[3]) ~ "Elevated",
            (SBP >= ylim_breaks[4] & SBP < ylim_breaks[5]) & (DBP >= xlim_breaks[3] & DBP < xlim_breaks[4]) ~ "Stage 1",
            (SBP >= ylim_breaks[5] & SBP < ylim_breaks[6]) & (DBP >= xlim_breaks[4] & DBP < xlim_breaks[5]) ~ "Stage 2",

            # Isolated categories

            # Stage 1
            (SBP >= ylim_breaks[4]  & SBP  <  ylim_breaks[5]) & (DBP < xlim_breaks[3]) ~ "ISH - S1",
            (SBP <  ylim_breaks[4]) & (DBP >= xlim_breaks[3]  &  DBP < xlim_breaks[4]) ~ "IDH - S1",

            # Stage 2
            (SBP >= ylim_breaks[5]  &  SBP  < ylim_breaks[6]) & (DBP < xlim_breaks[4]) ~ "ISH - S2",
            (SBP <  ylim_breaks[5]) & (DBP >= xlim_breaks[4]  & DBP  < xlim_breaks[5]) ~ "IDH - S2",

            SBP >= ylim_breaks[6] | DBP >= xlim_breaks[5] ~ "Crisis"

      ),

      TRUE ~ "ERROR"

    )) %>%

    # Move BP_CLASS column to front after DBP
    dplyr::relocate(BP_CLASS, .after = DBP)



    # data %>%
    #   dplyr::mutate(
    #       BP_CLASS = dplyr::case_when(
    #         opts == "NONE" ~ factor(BP_CLASS, ordered = TRUE, levels = c("Normal", "Elevated", "Stage 1", "IDH - S1", "ISH - S1", "Stage 2", "IDH - S2", "ISH - S2")),
    #         opts == "LOW" ~ factor(BP_CLASS, ordered = TRUE, levels = c("Low", "Normal", "Elevated", "Stage 1", "IDH - S1", "ISH - S1", "Stage 2", "IDH - S2", "ISH - S2")),
    #         opts == "CRISIS" ~ factor(BP_CLASS, ordered = TRUE, levels = c("Normal", "Elevated", "Stage 1", "IDH - S1", "ISH - S1", "Stage 2", "IDH - S2", "ISH - S2", "Crisis")),
    #         opts == "LOW_CRISIS" ~ factor(BP_CLASS, ordered = TRUE, levels = low_cris_lev[which(low_cris_lev %in% unique(tmp$BP_CLASS) == TRUE)] )
    #   ))



    all_stage_options <- c("Low", "Normal", "Elevated", "Stage 1", "IDH - S1", "ISH - S1", "Stage 2", "IDH - S2", "ISH - S2", "Crisis")
    data$BP_CLASS <- factor(data$BP_CLASS, ordered = TRUE, levels = all_stage_options[which(all_stage_options %in% unique(data$BP_CLASS) == TRUE)] )



  # Old code for SBP/DBP categories

    # SBP Category - Not a 2-to-1 mapping like BP_CLASS, but serves to isolate where most of the values for SBP fall
    data <- data %>% dplyr::mutate(SBP_CATEGORY = dplyr::case_when(

                    opts == "NONE" ~ dplyr::case_when(SBP < ylim_breaks[2] ~ "Normal",
                                                      SBP >=  ylim_breaks[2] & SBP < ylim_breaks[3] ~ "Elevated",
                                                      SBP >=  ylim_breaks[3] & SBP < ylim_breaks[4] ~ "Stage 1",
                                                      SBP >=  ylim_breaks[4] & SBP < ylim_breaks[5] ~ "Stage 2"),

                    opts == "LOW" ~ dplyr::case_when(SBP < ylim_breaks[2] ~ "Low",
                                                     SBP >=  ylim_breaks[2] & SBP < ylim_breaks[3] ~ "Normal",
                                                     SBP >=  ylim_breaks[3] & SBP < ylim_breaks[4] ~ "Elevated",
                                                     SBP >=  ylim_breaks[4] & SBP < ylim_breaks[5] ~ "Stage 1",
                                                     SBP >=  ylim_breaks[5] & SBP < ylim_breaks[6] ~ "Stage 2"),

                    opts == "CRISIS" ~ dplyr::case_when(SBP < ylim_breaks[2] ~ "Normal",
                                                        SBP >=  ylim_breaks[2] & SBP < ylim_breaks[3] ~ "Elevated",
                                                        SBP >=  ylim_breaks[3] & SBP < ylim_breaks[4] ~ "Stage 1",
                                                        SBP >=  ylim_breaks[4] & SBP < ylim_breaks[5] ~ "Stage 2",
                                                        SBP >=  ylim_breaks[5] & SBP < ylim_breaks[6] ~ "Crisis"),

                    opts == "LOW_CRISIS" ~ dplyr::case_when(SBP < ylim_breaks[2] ~ "Low",
                                                            SBP >=  ylim_breaks[2] & SBP < ylim_breaks[3] ~ "Normal",
                                                            SBP >=  ylim_breaks[3] & SBP < ylim_breaks[4] ~ "Elevated",
                                                            SBP >=  ylim_breaks[4] & SBP < ylim_breaks[5] ~ "Stage 1",
                                                            SBP >=  ylim_breaks[5] & SBP < ylim_breaks[6] ~ "Stage 2",
                                                            SBP >=  ylim_breaks[6] & SBP < ylim_breaks[7] ~ "Crisis"),

                    TRUE ~ "ERROR"

    ))

    data$SBP_CATEGORY <- factor(data$SBP_CATEGORY, ordered = TRUE, levels = all_stage_options[which(all_stage_options %in% unique(data$SBP_CATEGORY) == TRUE)] )



    # DBP Category - Not a 2-to-1 mapping like BP_CLASS, but serves to isolate where most of the values for DBP fall
    data <- data %>% dplyr::mutate(DBP_CATEGORY = dplyr::case_when(

                    opts == "NONE" ~ dplyr::case_when(DBP < xlim_breaks[2] ~ "Normal",
                                                      DBP >=  xlim_breaks[2] & DBP < xlim_breaks[3] ~ "Stage 1",
                                                      DBP >=  xlim_breaks[3] & DBP < xlim_breaks[4] ~ "Stage 2"),

                    opts == "LOW" ~ dplyr::case_when(DBP < xlim_breaks[2] ~ "Low",
                                                     DBP >=  xlim_breaks[2] & DBP < xlim_breaks[3] ~ "Normal",
                                                     DBP >=  xlim_breaks[3] & DBP < xlim_breaks[4] ~ "Stage 1",
                                                     DBP >=  xlim_breaks[4] & DBP < xlim_breaks[5] ~ "Stage 2"),

                    opts == "CRISIS" ~ dplyr::case_when(DBP < xlim_breaks[2] ~ "Normal",
                                                        DBP >=  xlim_breaks[2] & DBP < xlim_breaks[3] ~ "Stage 1",
                                                        DBP >=  xlim_breaks[3] & DBP < xlim_breaks[4] ~ "Stage 2",
                                                        DBP >=  xlim_breaks[4] & DBP < xlim_breaks[5] ~ "Crisis"),

                    opts == "LOW_CRISIS" ~ dplyr::case_when(DBP < xlim_breaks[2] ~ "Low",
                                                            DBP >=  xlim_breaks[2] & DBP < xlim_breaks[3] ~ "Normal",
                                                            DBP >=  xlim_breaks[3] & DBP < xlim_breaks[4] ~ "Stage 1",
                                                            DBP >=  xlim_breaks[4] & DBP < xlim_breaks[5] ~ "Stage 2",
                                                            DBP >=  xlim_breaks[5] & DBP < xlim_breaks[6] ~ "Crisis"),

                    TRUE ~ "ERROR"

    ))

    data$DBP_CATEGORY <- factor(data$DBP_CATEGORY, ordered = TRUE, levels = all_stage_options[which(all_stage_options %in% unique(data$DBP_CATEGORY) == TRUE)] )


  return(data)
}

