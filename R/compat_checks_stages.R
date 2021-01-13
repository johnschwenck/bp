
stage_check <- function(sbp_stages, dbp_stages){


  # Compatibility checks for SBP
  if(is.null(sbp_stages)){

    sbp_stages <- c(80,100,120,130,140,180,200)

  }else{

    sbp_stages <- as.integer(as.vector(sbp_stages))

    if(length(sbp_stages) != 7){
      stop('Invalid sbp_stages vector supplied. \nEnsure there are 7 values corresponding to the high / low thresholds of each stage.')
    }

    if(all(sbp_stages == cummax(sbp_stages)) == FALSE){
      stop('sbp_stages must be an increasing vector of integers.')
    }

    if(max(sbp_stages) > 300){
      warning('Highest systolic blood pressure threshold value exceeds 300. Ensure that this is intentional.')
    }

    if(min(sbp_stages) < 60){
      warning('Lowest systolic blood pressure threshold value is less than 60. Ensure that this is intentional.')
    }

  }


  # Compatibility checks for DBP
  if(is.null(dbp_stages)){

    dbp_stages <- c(25,60,80,85,90,120,140)

  }else{

    dbp_stages <- as.integer(as.vector(dbp_stages))

    if(length(dbp_stages) != 7){
      stop('Invalid dbp_stages vector supplied. \nEnsure there are 7 values corresponding to the low / high thresholds of each stage.')
    }

    if(all(dbp_stages == cummax(dbp_stages)) == FALSE){
      stop('dbp_stages vector must be monotonically increasing.')
    }

    if(max(dbp_stages) > 200){
      warning('Highest diastolic blood pressure threshold value exceeds 200. Ensure that this is intentional.')
    }

    if(min(dbp_stages) < 25){
      warning('Lowest diastolic blood pressure threshold value is less than 25. Ensure that this is intentional.')
    }

  }



  return(list(sbp_stages, dbp_stages))

}
