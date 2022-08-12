
# Compatibility Checks for user-defined bp_cutoffs function input
cutoff_input_check <- function(SUL, DUL, bp_cutoffs){

  # Check SUL, DUL limits

  if(SUL < max(bp_cutoffs[[1]]) ){
    stop('SUL less than highest bp_cutoff value for SBP within bp_stages function. To adjust highest SBP limit, set guidelines = "Custom" and adjust bp_cutoffs argument')
  }

  if(DUL < max(bp_cutoffs[[2]]) ){
    stop('DUL less than highest bp_cutoff value for DBP within bp_stages function. To adjust highest DBP limit, set guidelines = "Custom" and adjust bp_cutoffs argument')
  }

  if( any( c(SUL, DUL, bp_cutoffs[[1]], bp_cutoffs[[2]]) < 0) ){
    stop('Cannot have negative values for SLL, SUL, DLL, DUL, or elements of bp_cutoffs list')
  }

  # Check compatibility of supplied bp_cutoffs threshold values

  # Length check
  if ( (length( bp_cutoffs[[1]] ) != 5) | (length( bp_cutoffs[[2]] ) != 5) ){
    stop("Five threshold values should be supplied to first element of bp_cutoffs, and five treshold values should be supplied to second element")
  }
  # Numerical value check
  if ( (!is.numeric( bp_cutoffs[[1]] )) | (!is.numeric( bp_cutoffs[[2]] )) ){
    stop("Numeric values should be supplied as thresholds to bp_cutoffs elements")
  }
  # Range check
  if ((min(bp_cutoffs[[1]]) < 0)|(min(bp_cutoffs[[2]]) < 0) |(max(bp_cutoffs[[1]]) > SUL) | (max(bp_cutoffs[[2]]) > DUL)){
    stop("Thresholds for bp_cutoffs elements must be between [SLL, SUL] and [DLL, DUL] range, respectively")
  }
  # Sorting check
  if ( (!identical( bp_cutoffs[[1]], sort( bp_cutoffs[[1]] ) )) | (!identical(bp_cutoffs[[2]], sort( bp_cutoffs[[2]] ) )) ){
    stop("Thresholds for bp_cutoffs elements must be supplied from smallest to largest")
  }
  #
  if( ( all(bp_cutoffs[[1]] == cummax(bp_cutoffs[[1]])) & all(bp_cutoffs[[2]] == cummax(bp_cutoffs[[2]])) ) == FALSE){
    stop('bp_cutoffs elements must be increasing vectors of integers.')
  }

  return(bp_cutoffs)
}



# Create documentation once cutoff format issue is resolved
# stage_lookup creates the lookup table for comparing input data to

stage_lookup_v1 <- function(bp_type = c("OBP", "HBPM", "ABPM"),
                            guidelines = c("Lee_2020", "AHA", "Custom"),
                            bp_cutoffs = list( c(100, 120, 130, 140, 180), c(60, 80, 80, 90, 120)),
                            SUL = 240, DUL = 140, inc_low = FALSE, inc_crisis = TRUE){


  # Match guidelines & bp_type specified in function arguments
  guidelines = match.arg(guidelines)
  bp_type = match.arg(bp_type)
  #print(guidelines)



  # Default Lee_2020 stages for reference in compatibility checks
  default_stages = c( "Low", "Normal", "Elevated", "Stage 1", "ISH - S1", "IDH - S1", "Stage 2", "ISH - S2", "IDH - S2", "Crisis" )
  default_cutoffs = list( c(100, 120, 130, 140, 180), c(60, 80, 80, 90, 120))
  default_sbp_dbp_ops = c("|", "&", "&", "&", "&", "&", "&", "&", "&", "|")


  # Lee 2020 cutoffs are fixed according to paper, cannot change

  if( (guidelines == "Lee_2020" ) & (identical(bp_cutoffs, default_cutoffs) == FALSE ) ){

    warning('bp_cutoffs input ignored since guidelines set to "Lee_2020". To set custom bp_cutoffs, set guidelines = "Custom"')

  }


  # bp_cutoff Adjustment - Return output based on guidelines chosen above:

  # Custom implies a custom input vector list for bp_cutoffs whereas Lee/AHA implies pre-determined inputs based on bp_type
  # i.e. bp_type only matters for AHA guidelines --> Lee is fixed, Custom is assumed to be pre-specified to the associated bp_type

  if(guidelines == "Custom"){

    # Different thresholds, possibly different stages

    if( identical(bp_cutoffs, default_cutoffs) == TRUE ){
      message('guidelines = "Custom", but bp_cutoffs unchanged (set to default values). Proceeding with Lee 2020 guidelines.')
    }

    bp_cutoffs = cutoff_input_check(SUL, DUL, bp_cutoffs)

  }else{


    # If not custom, then either Lee or AHA
    # Lee ==> AHA

    if(guidelines == "Lee_2020"){
      bp_cutoffs = default_cutoffs #list( c(100, 120, 130, 140, 180), c(60, 80, 80, 90, 120))


    }else if(guidelines == "AHA"){

      # if bp_cutoffs differ from default_cutoffs it implies that the user changed the bp_cutoffs input argument
      if( identical(bp_cutoffs, default_cutoffs) == FALSE ){

        # Run checks for bp_cutoff supplied by user
        bp_cutoffs = cutoff_input_check(SUL, DUL, bp_cutoffs)

        message('AHA guidelines specified, but bp_cutoffs changed by user.
bp_cutoffs will override AHA guidelines for the respective bp_type.
If this is a mistake, leave bp_cutoffs to default values and keep guidelines = "AHA".')

      }else{
        # User did not change the bp_cutoffs argument --> use the cutoffs specified by AHA
        if(bp_type == "OBP"){
          bp_cutoffs = list( c(100, 120, 130, 140, 160), c(60, 80, 80, 90, 100))

        }else if(bp_type == "HBPM"){
          bp_cutoffs = list( c(100, 120, 130, 135, 145), c(60, 80, 80, 85, 90))

        }else if(bp_type == "ABPM"){
          bp_cutoffs = list( c(100, 115, 125, 130, 145), c(60, 75, 75, 80, 90))
        }
      }
    }
  }

  # specify each of the 4 LL/UL vectors using user input
  sbp_LL  = c( 0, bp_cutoffs[[1]][1], bp_cutoffs[[1]][2], bp_cutoffs[[1]][3], bp_cutoffs[[1]][3], 0, bp_cutoffs[[1]][4], bp_cutoffs[[1]][4], 0, bp_cutoffs[[1]][5] )
  sbp_UL = c( bp_cutoffs[[1]][1], bp_cutoffs[[1]][2], bp_cutoffs[[1]][3], bp_cutoffs[[1]][4], bp_cutoffs[[1]][4], bp_cutoffs[[1]][3], bp_cutoffs[[1]][5], bp_cutoffs[[1]][5], bp_cutoffs[[1]][4], SUL )
  dbp_LL =  c( 0, bp_cutoffs[[2]][1], 0, bp_cutoffs[[2]][3], 0, bp_cutoffs[[2]][3],   bp_cutoffs[[2]][4], 0, bp_cutoffs[[2]][4], bp_cutoffs[[2]][5] )
  dbp_UL = c( bp_cutoffs[[2]][1], bp_cutoffs[[2]][2], bp_cutoffs[[2]][3], bp_cutoffs[[2]][4], bp_cutoffs[[2]][3], bp_cutoffs[[2]][4], bp_cutoffs[[2]][5], bp_cutoffs[[2]][4], bp_cutoffs[[2]][5], DUL )

  # Output including all 10 stages from Lee et al. 2020
  out = data.frame(default_stages, sbp_LL, sbp_UL, default_sbp_dbp_ops, dbp_LL, dbp_UL)

  colnames(out)[which( colnames(out) %in% "default_stages" )] <- "Stages"
  colnames(out)[which( colnames(out) %in% "default_sbp_dbp_ops" )] <- "Operation"

  if(guidelines == "AHA"){

    # out = out[ out[ , 1] %in% default_stages[c(1,2,3,4,7,10)] , ]
    out = out[ out[ , 1] %in% default_stages[ which( default_stages %in% c("Low","Normal","Elevated","Stage 1","Stage 2","Crisis") )] , ]
    row.names(out) <- NULL

    out[ out[ , 1] %in% default_stages[ which( default_stages %in% c("Stage 1", "Stage 2") )] , ]$Operation <- c("|", "|")

  }


  if(inc_low == FALSE){

    out = out[ out[ , 1] %in% default_stages[!(default_stages %in% "Low")] , ]
    row.names(out) <- NULL

    # Reset LL of SBP / DBP to 0
    out$sbp_LL[1] <- 0
    out$dbp_LL[1] <- 0

  }

  if(inc_crisis == FALSE){

    out = out[ out[ , 1] %in% default_stages[!(default_stages %in% "Crisis")] , ]
    row.names(out) <- NULL

    # Reset UL of SBP / DBP to SUL / DUL:

    if(guidelines == "Lee_2020"){

      # Change S2 and ISH - S2
      out$sbp_UL[ length(out$sbp_UL) - 2 ] <- SUL
      out$sbp_UL[ length(out$sbp_UL) - 1 ] <- SUL
      out$dbp_UL[ length(out$dbp_UL) - 2 ] <- DUL
      out$dbp_UL[ length(out$dbp_UL) ]     <- DUL

    }else if(guidelines == "AHA"){
      # Change S2 SUL / DUL
      out$sbp_UL[ length(out$sbp_UL) ] <- SUL
      out$dbp_UL[ length(out$dbp_UL) ] <- DUL
    }


  }

  # print(out)

  return(out)

}



# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ #


#' Blood Pressure Stage Classification
#'
#' Adds BP_CLASS, SBP_Category, and DBP_Category columns to supplied dataframe.
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
#' @param bp_type A string designation for the blood pressure type of the underlying SBP / DBP data. \code{bp_type}
#' may be one of the following: "HBPM", "ABPM", or "OBP" where HBPM corresponds to home blood pressure
#' monitoring, ABPM corresponds to ambulatory blood pressure monitoring, and OBP corresponds to office
#' blood pressure data. By default, \code{bp_type} is set to "HBPM". \code{bp_type} only impacts \code{bp_stages}
#' if \code{guidelines = "AHA"}, for which the cutoffs for each blood pressure stage will be adjusted
#' according to its respective blood pressure type. If guidelines argument set to either "Lee" or
#' "Custom", the \code{bp_type} will have no effect since the Lee guidelines are pre-determined from a
#' recent academic paper and Custom implies that the user is already aware of the bp_type.
#'
#' @param guidelines A string designation for the health / medical guidelines to follow when mapping BP
#' readings to a respective BP stage. \code{guidelines} can take on either "Lee_2020" corresponding to an
#' academic paper by Lee et al (2020) (see references for function), "AHA" corresponding to published
#' guidelines by the American Heart Association, or "Custom" which the user sets their own BP cutoffs.
#'
#' @param bp_cutoffs A list containing two vectors corresponding to SBP and DBP cutoffs, respectively.
#' The vectors both contain 5 integer values that each represent an upper limit cutoff for each SBP / DBP
#' stage. If \code{guidelines = "Lee_2020"}, this argument is ignored; if \code{guidelines = "AHA"} and
#' if the \code{bp_cutoffs} is manually adjusted (i.e. not the default list), then the manually adjusted
#' list will override the default AHA guidelines. The default AHA guidelines depend on the bp_type and will
#' adjust automatically based on the specification of \cde{bp_type}. If \code{guidelines = "Custom"},
#' the \code{bp_cutoffs} argument dictates the cutoffs for the respective stages as expected.
#'
#' The SBP vector (100, 120, 130, 140, 180) corresponds to the upper limits for the following stages:
#' Low (0-100), Normal (100-120), Elevated (120-130), Stage 1 Hypertension (130-140), Stage 2 Hypertension
#' (140-180) according to the American Heart Association (AHA). When utilizing Lee et al (2020) guidelines,
#' the following additional stages will be automatically derived and included: Isolated Systolic Hypertension
#' for Stage 1 (ISH - S1) (130-140), Isolated Diastolic Hypertension for Stage 1 (IDH - S1) (0-130), ISH - S2
#' (140-180), and IDH - S2 (0-140).
#'
#' The DBP vector (60, 80, 80, 90, 120) corresponds to the upper limits for the following stages:
#' Low (0-60), Normal (60-80), Elevated (0-80), Stage 1 Hypertension (80-90), Stage 2 Hypertension
#' (90-120) according to the American Heart Association (AHA). When utilizing Lee et al (2020) guidelines,
#' the following additional stages will be automatically derived and included: Isolated Systolic Hypertension
#' for Stage 1 (ISH - S1) (0-80), Isolated Diastolic Hypertension for Stage 1 (IDH - S1) (80-90), ISH - S2
#' (0-90), and IDH - S2 (90-120).
#'
#' NOTE: The upper limit of the "Elevated" category repeats in the DBP vector and matches that of Normal.
#' This because according to most guidelines, there is no distinction between DBP cutoffs for Normal and
#' Elevated - these stages are discerned by SBP, not DBP. Should the user decide to manually change these
#' \code{bp_cutoffs} values, the stages will reflect accordingly.
#'
#' Any manual adjustment to these SBP / DBP vectors by the user will be assumed to be "Custom" and will
#' classify readings accordingly. If no manual adjustments made, stage classification will conform to either
#' "Lee_2020" or "AHA" protocol (where Lee is pre-determined and AHA is determined by bp_type).
#'
#' Any SBP reading below 100 or DBP reading below 60 is considered Hypotension ("Low").
#' Any SBP reading above 180 or DBP reading above 120 is considered a Crisis.
#'
#' If \code{inc_low = FALSE}, although an upper limit value is still required in the SBP vector, the "Low"
#' stage will be omitted in the final output. Similarly, if \code{inc_crisis = FALSE}, then the "Crisis"
#' category will be omitted from the final output.
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
#' @param adj_sbp_dbp Logical indicator to dictate whether or not to run helper functions that adjust / process
#' SBP & DBP columns in supplied data set. Default set to: \code{adj_sbp_dbp = TRUE}
#'
#' @return A dataframe with additional columns corresponding to the stages of high blood pressure and the
#' supplementary SBP / DBP categories
#'
#' @references
#' Lee H, Yano Y, Cho SMJ, Park JH, Park S, Lloyd-Jones DM, et al. Cardiovascular risk of isolated systolic
#' or diastolic hypertension in young adults. \emph{Circulation}. 2020;141(22):1778–1786.
#' \doi{0.1161/CIRCULATIONAHA.119.044838}
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
bp_stages <- function(data, sbp, dbp, bp_type = c("HBPM", "ABPM", "OBP"), inc_low = FALSE, inc_crisis = TRUE, data_screen = TRUE,
                      SUL = 240, SLL = 50, DUL = 140, DLL = 40, adj_sbp_dbp = TRUE,
                      guidelines = c("Lee_2020", "AHA", "Custom"),
                      bp_cutoffs = list( c(100, 120, 130, 140, 180), c(60, 80, 80, 90, 120)) ){

  SBP = DBP = BP_CLASS = . = NULL
  rm(list = c('SBP', 'DBP', 'BP_CLASS', '.'))

  # Set bp_type and guidelines
  bp_type = match.arg(bp_type)
  guidelines = match.arg(guidelines)


  # Convert all column names to upper case for consistency
  colnames(data) <- toupper(colnames(data))

  # SBP / DBP Adjustments
  if(adj_sbp_dbp == TRUE){

    # Adjust SBP
    data <- sbp_adj(data = data, sbp = sbp, data_screen = data_screen, SUL = SUL, SLL = SLL)

    # Adjust DBP
    data <- dbp_adj(data = data, dbp = dbp, data_screen = data_screen, DUL = DUL, DLL = DLL)

  }


  # If all SBP/DBP values are NA (due to data screening), change BP_CLASS to NA and avoid all future computation to avoid min/max warning
  if(all(is.na(data$SBP)) == TRUE | all(is.na(data$DBP)) == TRUE){

    # BP_CLASS is NA if all of either SBP or DBP are NA
    data$BP_CLASS <- NA

    # Move BP_CLASS column to front after DBP
    data <- data %>% dplyr::relocate(BP_CLASS, .after = DBP)

    # Throw warning to inform user that all SBP or DBP values were screened out due to data_screen argument
    warning('All SBP values or all DBP values are NA. \n\nThis is most likely due to data being filtered out by data_screen argument in process_data(). \nEnsure that upper and lower limits for SBP/DBP are correct.')

  }


  # Initiate lookup table for stage cutoffs
  bp_lookup <- stage_lookup_v1(bp_type = bp_type, guidelines = guidelines, bp_cutoffs = bp_cutoffs,
                               SUL = SUL, DUL = DUL, inc_low = inc_low, inc_crisis = inc_crisis)

  stages <- bp_lookup$Stages

  # Initiate BP_CLASS column in data
  if( "BP_CLASS" %in% colnames(data) ){

    colnames(data)[colnames(data) == "BP_CLASS"] <- "BP_CLASS_OLD"
  }

  data$BP_CLASS <- NA
  data$SBP_CATEGORY <- NA
  data$DBP_CATEGORY <- NA


  ### BP_CLASS Column

  # Loop through conditions, match BP stage
  for ( i in 1:(length( unique(bp_lookup$Stages) ) ) ){
    if( eval(parse(text = paste( "nrow(data[ which( ( (data$SBP >= bp_lookup$sbp_LL[",i,"]) & (data$SBP < bp_lookup$sbp_UL[",i,"])", bp_lookup$Operation[i], "( (data$DBP >= bp_lookup$dbp_LL[",i,"]) & (data$DBP < bp_lookup$dbp_UL[",i,"]) ) )), ]) != 0 " ) )) ){
      data[ which( eval(parse(text = paste( "( (data$SBP >= bp_lookup$sbp_LL[",i,"]) & (data$SBP < bp_lookup$sbp_UL[",i,"])", bp_lookup$Operation[i], "( (data$DBP >= bp_lookup$dbp_LL[",i,"]) & (data$DBP < bp_lookup$dbp_UL[",i,"]) ) )")))), ]$BP_CLASS <- stages[i]
    }else{
      next
    }
  }

  # Re-factor BP_CLASS column in order of hypertension stages
  data$BP_CLASS <- factor(data$BP_CLASS, ordered = TRUE, levels = stages[which(stages %in% unique(data$BP_CLASS) == TRUE)] )

  # Move BP_CLASS column to front after DBP
  data <- data %>% dplyr::relocate(BP_CLASS, .after = DBP)


  ### SBP_CATEGORY / DBP_CATEGORY for plots

  AHA_lookup <- stage_lookup_v1(bp_type = "OBP", guidelines = "AHA",
                                SUL = SUL, DUL = DUL, inc_low = inc_low, inc_crisis = inc_crisis)
  AHA_stages <- AHA_lookup$Stages

  # SBP_Category
  for ( i in 1:(length( unique(AHA_lookup$Stages) ) ) ){
    if( eval(parse(text = paste( "nrow(data[ which( ( (data$SBP >= AHA_lookup$sbp_LL[",i,"]) & (data$SBP < AHA_lookup$sbp_UL[",i,"]) ) ), ]) != 0 " ) )) ){
      data[ which( eval(parse(text = paste( "( (data$SBP >= AHA_lookup$sbp_LL[",i,"]) & (data$SBP < AHA_lookup$sbp_UL[",i,"]) )") )) ), ]$SBP_CATEGORY <- AHA_stages[i]
    }
  }

  # DBP_Category
  for ( i in which(AHA_lookup$Stages %in% c("Normal", "Stage 1", "Stage 2", "Crisis")) ){
    if( eval(parse(text = paste( "nrow(data[ which( ( (data$DBP >= AHA_lookup$dbp_LL[",i,"]) & (data$DBP < AHA_lookup$dbp_UL[",i,"]) ) ), ]) != 0 " ) )) ){
      data[ which( eval(parse(text = paste( "( (data$DBP >= AHA_lookup$dbp_LL[",i,"]) & (data$DBP < AHA_lookup$dbp_UL[",i,"]) )") )) ), ]$DBP_CATEGORY <- AHA_stages[i]
    }
  }


  return(data)

}


