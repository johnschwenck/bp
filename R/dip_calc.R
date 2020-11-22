# Calculate the drop in BP during sleep to determine dipping pattern

# To Do: - If Awake indicator column is specified in dataset, calculate percentages according to indicator.
#              If not, default to Awake: 6am - 11pm | Asleep: 11pm - 6am |
#        - Give user ability to choose time frame interval if no awake indicator column provided
#        X User-defined dipping threshold, default to 10%
#        - Screening criteria  for {SBP > 250 | SBP < 70} and {DBP < 45 | DBP > 150} and {HR < 40 | HR > 200} according to Holt-Lunstad, Jones, and Birmingham (2009) paper


# Calculate the percent difference between two successive groups. In this case: Awake vs Asleep
pct = function(X_vec) {
  return(X_vec[1]/X_vec[2])
}

dip <- data %>%
  group_by(ID, VISIT, WAKE) %>%
  summarise(Avg_BP = mean(SBP))

dip_thresh <- 0.10
dip_pct <- dip %>%
  summarise(dip = -(1 - pct(Avg_BP)) ) %>%
  mutate(out = ifelse(dip <= -dip_thresh, "dipper",
                      ifelse(dip > 0, "reverse", "non-dipper")))

list(dip, dip_pct)

dip_calc <- function(data, dip_thresh = .10, sleep_int = NULL){

  # Check to see if there is a column indicating Awake vs Asleep
  if(is.null(data$WAKE)){ # No Sleep / Awake indicator column provided

    if(is.null(data$DATE_TIME)){ # No Date_Time column provided, unable to calculate dipping metrics # No Wake, No Date_time

      stop('Either sleep/awake indicator column or date_time column is required to calculate the dipping metrics.')

    }else if(is.null(sleep_int)){ # If no user-specified time interval, default to 11pm - 6am according to paper # No wake, Yes Date_time, No sleep_int

      sleep_int = c(23, 0, 1, 2, 3, 4, 5, 6)
      awake_int <- (seq(1:24)-1)[!(seq(1:24)-1) %in% sleep_int]

      data$WAKE <- ifelse(lubridate::hour(data$DATE_TIME) %in% awake_int == TRUE, 1, 0)

    }else{ # No Wake, Yes Date_Time, Yes sleep_int

          if(!is.vector(sleep_int)){ # Not vector

            stop('sleep_int must be a vector specifying the sleep interval. For example: sleep_int = c(22,5) to indicate sleep period from 22:00 to 05:00')

          }else if( any( !is.numeric(sleep_int) | is.na(sleep_int) ) == TRUE){ # Yes vector, No numeric

            stop('One or more of the elements of the supplied sleep_int vector are not numeric and/or NA')

          }

          # Coerce decimal values to integers
          if( all(sleep_int == floor(sleep_int)) == FALSE ){

            warning('Coerced floating point values within sleep_int vector to integer')
            sleep_int <- as.integer(sleep_int)

          }

          awake_int <- (seq(1:24)-1)[!(seq(1:24)-1) %in% sleep_int]

          data$WAKE <- ifelse(lubridate::hour(data$DATE_TIME) %in% awake_int == TRUE, 1, 0)

      }

  }else{ # WAKE Column provided

      x

  }

}


