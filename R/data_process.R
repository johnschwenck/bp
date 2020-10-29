#' Data Pre-processor
#'
#' @param data (Required) User-supplied data set
#' @param sbp (Required) Column index of Systolic Blood Pressure (SBP) values
#' @param dbp (Required) Column index of Diastolic Blood Pressure (DBP) values
#' @param hr (Optional) Column index of Heart Rate (HR) values
#' @param pp (optional) Column index of Pulse Pressure (PP) values
#' @param map (optional) Column index of Mean Arterial Pressure (MAP) values
#' @param rpp (optional) Column index of Rate Pressure Product (RPP) values - Note that RPP can be directly calculated from HR and SBP values if not originally supplied
#'
#' @return
#' @export
#'
#' @examples
data_process <- function(data, sbp = 1, dbp = 2, hr = NULL, pp = NULL, map = NULL, rpp = NULL){
  # If only two columns in dataset (both columns must be sbp and dbp):
  if(ncol(data) <= 2){

    sbp_col <- as.numeric(readline('Does the first column correspond to Systolic Blood Pressure (SBP)? 1: Yes | 2: No   '))
    if(sbp_col != 1 & sbp_col != 2){
      stop('Invalid input: Enter 1 for Yes or 2 for No')
    }

    if(sbp != 1){

      data <- data[ , c( sbp, 1)]

    }
  }else if(sbp != 1){ # For more than 2 columns in dataset

    data <- data[ , c( sbp, 1:(sbp-1), (sbp+1):ncol(data) )]

    if(dbp != 2){
      data <- data[ , c( 1, dbp, 2:(dbp-1), (sbp+1):ncol(data) )]
    }
  }

}
