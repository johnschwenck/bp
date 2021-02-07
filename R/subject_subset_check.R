subject_subset_check <- function(data, subj = NULL){

  subj <- as.integer(subj)
  data$ID <- as.integer(data$ID)

  if(!is.vector(subj)){
    stop('subj argument must be a vector corresponding to the selected individuals to analyze')
  }

  if(typeof(subj) != typeof(data$ID)){
    stop('subj and ID column must be of the same type (vector)')
  }

  if(all(subj %in% data$ID) == FALSE){
    stop('One or more of the supplied subject IDs are not present in the supplied data')
  }

  return(TRUE)
}
# subject_subset_check(data, subj)
