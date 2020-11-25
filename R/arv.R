#' Average Real Variability (ARV)
#'
#' @description
#'
#' @return
#' @export
#'
#' @examples
#' data <- hypnos_data
#' data <- process_data(data, sbp = "SYST", dbp = "DIAST", bp_datetime = "date.time", id = "id", wake = "wake", visit = "visit", hr = "hr", pp ="pp", map = "map", rpp = "rpp")
#'
#' arv(data)
arv <- function(data){

  # check for missing values
  if(nrow(data) != length(complete.cases(data$SBP))){
    warning('Missing values found in data set. Removing for calculation')
    # remove NA values
  }

  # Determine how granular to calculate based on which columns are available
  grps = c("ID", "VISIT", "WAKE")
  grps = grps[which(grps %in% colnames(data) == TRUE)]

  # Group data by whichever of the three above variables are present in the data
  out <- data %>%
    group_by_at(vars(grps) ) %>%
    #filter(ID == 70417, VISIT == 1, WAKE == 0) %>% # subset for manual calculation test

    # ARV Calculation
    summarise(ARV = sum( abs( (SBP - lag(SBP))[2:length(SBP - lag(SBP))] ) ) / (n() - 1))

  return(out)
}





