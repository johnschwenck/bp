#' Alternative Blood Pressure Stages
#'
#' Adds SBP_Category and DBP_Category columns to supplied dataframe.
#'
#' Supplied dataframe must adhere to the unified format using the \code{process_data} function.
#'
#' @param data User-supplied data
#'
#' @param sbp column name corresponding to systolic blood pressure (SBP)
#'
#' @param dbp column name corresponding to diastolic blood pressure (DBP)
#'
#' @param sbp_stages_alt (Optional) Determines the lower and upper limits for each stage
#' of systolic blood pressure (\code{SBP}). If supplied, the input must be a vector
#' containing 7 integers that correspond to the limits of each stage. The default vector would be
#' given by \code{c(80, 100, 120, 130, 140, 180, 200)} where:
#' \itemize{
#'
#'    \item Low - default: < 100 (specifically 80 - 100)
#'    \item Normal - default: 100 - 120
#'    \item Elevated - default: 120 - 130
#'    \item Stage 1 - default: 130 - 140
#'    \item Stage 2 - default: 140 - 180
#'    \item Crisis - default: > 180 (specifically 180 - 200)
#'
#' }
#'
#'
#' @param dbp_stages_alt (Optional) Determines the lower and upper limits for each stage
#' of diastolic blood pressure (\code{DBP}). If supplied, the input must be a vector
#' containing 7 integers that correspond to the limits of each stage. The default vector would be
#' given by \code{c(25, 60, 80, 85, 90, 120, 140)} where:
#' \itemize{
#'
#'    \item Low - default: < 60 (specifically 25 - 60)
#'    \item Normal - default: 60 - 80
#'    \item Elevated - default: 80 - 85
#'    \item Stage 1 - default: 85 - 90
#'    \item Stage 2 - default: 90 - 120
#'    \item Crisis - default: > 120 (specifically 120 - 140)
#'
#' }
#'
#' @param screen Default to TRUE. Screens for extreme values in the data for both \code{SBP} and \code{DBP}
#' according to Omboni, et al (1995) paper - Calculation of Trough:Peak Ratio of Antihypertensive Treatment
#' from Ambulatory Blood Pressure: Methodological Aspects
#'
#'
#' @return A dataframe with two additional columns corresponding to \code{SBP_Category} and \code{DBP_Category}
#'
#' @export
#'
#' @examples
#' # Load bp_hypnos
#' data(bp_hypnos)
#'
#' bp_stages(bp_hypnos,
#'             sbp = "syst",
#'             dbp = "diast",
#'             sbp_stages_alt = c(80, 100, 120, 130, 140, 170, 200),
#'             dbp_stages_alt = c(25, 60, 80, 85, 90, 120, 140)
#'             )
#'
#'
#' # Load bp_jhs data
#' data(bp_jhs)
#'
#' bp_stages(bp_jhs,
#'             sbp = "sys.mmhg.",
#'             dbp = "dias.mmhg.",
#'             sbp_stages_alt = c(80, 100, 120, 130, 160, 170, 200),
#'             )
#'
bp_stages <- function(data, sbp, dbp, sbp_stages_alt = NULL, dbp_stages_alt = NULL, screen = FALSE){

  # BP Categories / Stages
  # Only require SBP and DBP

  SBP = DBP = SBP_Category = DBP_Category = . = NULL
  rm(list = c("SBP", "DBP", "SBP_Category", "DBP_Category", "."))

  # Adjust SBP
  data <- sbp_adj(data = data, sbp = sbp, screen = screen)

  # Adjust DBP
  data <- dbp_adj(data = data, dbp = dbp, screen = screen)

  # Compatibility Check for user-supplied stages if applicable
  sbp_stages <- stage_check(sbp_stages_alt, dbp_stages_alt)[[1]]
  dbp_stages <- stage_check(sbp_stages_alt, dbp_stages_alt)[[2]]

  data <- data %>%
    dplyr::mutate(SBP_Category = dplyr::case_when(SBP <= sbp_stages[2] ~ "Low",
                                                  SBP > sbp_stages[2] & SBP <= sbp_stages[3] ~ "Normal",
                                                  SBP > sbp_stages[3] & SBP <= sbp_stages[4] ~ "Elevated",
                                                  SBP > sbp_stages[4] & SBP <= sbp_stages[5] ~ "Stage 1",
                                                  SBP > sbp_stages[5] & SBP <= sbp_stages[6] ~ "Stage 2",
                                                  SBP > sbp_stages[6] ~ "Crisis"),
                  SBP_Category = factor(SBP_Category, levels = c("Low", "Normal", "Elevated", "Stage 1", "Stage 2", "Crisis")),

                  DBP_Category = dplyr::case_when(DBP <= dbp_stages[2] ~ "Low",
                                                  DBP > dbp_stages[2]  & DBP <= dbp_stages[3] ~ "Normal",
                                                  DBP > dbp_stages[3]  & DBP <= dbp_stages[4] ~ "Elevated",
                                                  DBP > dbp_stages[4]  & DBP <= dbp_stages[5] ~ "Stage 1",
                                                  DBP > dbp_stages[5] & DBP <= dbp_stages[6] ~ "Stage 2",
                                                  DBP > dbp_stages[6] ~ "Crisis"),
                  DBP_Category = factor(DBP_Category, levels = c("Low", "Normal", "Elevated", "Stage 1", "Stage 2", "Crisis")) )

  data <- data %>%
    dplyr::relocate(SBP_Category, .after = SBP) %>%
    dplyr::relocate(DBP_Category, .after = DBP)


  return(data)
}

