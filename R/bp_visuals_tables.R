#' Blood Pressure Tables in Graphical Format
#'
#' @param data A processed dataframe resulting from the \code{process_data} function that
#' contains the \code{SBP}, \code{DBP}, \code{DAY_OF_WEEK}, \code{Time_of_Day}, \code{SBP_Category},
#' and \code{DBP_Category} columns.
#'
#' @param subj Optional argument. Allows the user to specify and subset specific subjects
#' from the \code{ID} column of the supplied data set. The \code{subj} argument can be a single
#' value or a vector of elements. The input type should be character, but the function will
#' comply with integers so long as they are all present in the \code{ID} column of the data.
#'
#' @return A list of four `gtables` that correspond to the tables for Day of Week and
#' Time of Day broken down by both \code{SBP} and \code{DBP}.
#'
#' @export
#'
#' @examples
#'
#' data("hypnos_data")
#' hyp_proc <- process_data(hypnos_data,
#'                          sbp = "syst",
#'                          dbp = "DIAST",
#'                          bp_datetime = "date.time",
#'                          id = "id",
#'                          wake = "wake",
#'                          visit = "visit",
#'                          hr = "hr",
#'                          map = "map",
#'                          rpp = "rpp",
#'                          pp = "pp",
#'                          ToD_int = c(5, 13, 18, 23))
#'
#' rm(hypnos_data)
#'
#' dow_tod_plots_tmp <- dow_tod_plots(hyp_proc)
#' grid::grid.draw(
#'    gridExtra::grid.arrange(dow_tod_plots_tmp[[1]], dow_tod_plots_tmp[[2]],
#'                            dow_tod_plots_tmp[[3]],dow_tod_plots_tmp[[4]],
#'                            nrow = 2, ncol = 2)
#'                            )
dow_tod_plots <- function(data, subj = NULL){


  # Primary variables needed:    SBP, DBP, DAY_OF_WEEK, Time_of_Day
  # Secondary variables needed:  SBP_Category (bp_tables), DBP_Category (bp_tables)

  SBP = DBP = DAY_OF_WEEK = ID = Time_of_Day = Sun = Mon = Tue = Wed = Thu = Fri = Sat = Total = Morning = Afternoon = Evening = Night = . = NULL
  rm(list = c("SBP", "DBP", "DAY_OF_WEEK", "ID", "Time_of_Day",
              "Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Total",
              "Morning", "Afternoon", "Evening", "Night", "."))


  # If user supplies a vector corresponding to a subset of multiple subjects (multi-subject only)
  if(!is.null(subj)){

    # check to ensure that supplied subject vector is compatible
    subject_subset_check(data, subj)

    if(length(unique(data$ID)) > 1){

      # Filter data based on subset of subjects
      data <- data %>%
        dplyr::filter(ID == subj)

    }

  }


  ###################
  # SBP Day of Week #
  ###################

  # Extract necessary data
  sbp_dow_data <- bp_tables(data)['SBP_by_Day_of_Week'][[1]]

  # Convert row names to column --> tibble package
  sbp_dow_data <- tibble::rownames_to_column(sbp_dow_data, var = "BP Stage")

  # Add column totals --> dplyr for bind_rows
  # See https://github.com/r-lib/tidyselect/issues/201 for issue with "where" in dplyr's across function
  # https://dplyr.tidyverse.org/reference/across.html - dplyr documentation of how where is used within across
  # https://stackoverflow.com/questions/40251801/how-to-use-utilsglobalvariables - alternative temporary fix using
  #                                                                                               global variables
  sbp_dow_data <- sbp_dow_data %>%
    dplyr::bind_rows(dplyr::summarise(.,
                               dplyr::across(tidyselect::vars_select_helpers$where(is.numeric), sum),
                               dplyr::across(tidyselect::vars_select_helpers$where(is.character), ~"Total")))


  # Stages
  sbp_stage <- gridExtra::tableGrob(sbp_dow_data["BP Stage"],
                        theme = gridExtra::ttheme_default(
                          colhead = list(bg_params=list(fill="white", col="grey90")),
                          base_size = 7, padding = unit(c(4,3),"mm")),
                        rows = NULL)

  sbp_dow_table <- gridExtra::gtable_cbind(sbp_stage)

  # Loop over all days of the week
  for(dfcol in names(sbp_dow_data)[2:(length(names(sbp_dow_data)) - 1)]){

    cols_day <- grDevices::colorRampPalette(c("#FFE0B2", "#FFA000", "#E65100"))(nrow(sbp_dow_data))[rank(sbp_dow_data[[dfcol]][1:6])]
    day_dat <- as.data.frame(sbp_dow_data[dfcol][1:6,])
    colnames(day_dat) <- dfcol
    day <- gridExtra::tableGrob(day_dat,
                     theme = gridExtra::ttheme_default(
                     core=list(bg_params = list(fill=cols_day)),
                     colhead = list(bg_params=list(fill="white", col="grey90")),
                     base_size = 7, padding = unit(c(4,3),"mm")),
                     rows = NULL)
    day_tot <- as.data.frame(sbp_dow_data[dfcol][7,])
    colnames(day_tot) <- dfcol

    day_tot_grob <- gridExtra::tableGrob(day_tot, rows = NULL, cols = NULL,
                              theme = gridExtra::ttheme_default(base_size = 7, padding = unit(c(4,3),"mm")))

    day <- gridExtra::gtable_rbind(day, day_tot_grob)

    sbp_dow_table <- gridExtra::gtable_cbind(sbp_dow_table, day)
  }


  # Totals
  sbptotals <- gridExtra::tableGrob(sbp_dow_data["Total"],
                      theme = gridExtra::ttheme_default(
                        colhead = list(bg_params=list(fill="white", col="grey90")),
                        base_size = 7, padding = unit(c(4,3),"mm")),
                      rows = NULL)

  sbp_dow_table <- gridExtra::gtable_cbind(sbp_dow_table, sbptotals)


  sbp_dow_table <- gtable::gtable_add_grob(sbp_dow_table,
                                    grobs = grid::rectGrob(gp = grid::gpar(fill = NA, lwd = 2)),
                                    t = 2,
                                    b = nrow(sbp_dow_table)-1,
                                    l = 2,
                                    r = ncol(sbp_dow_table)-1
                                  )

  sbp_dow_table <- gtable::gtable_add_grob(sbp_dow_table,
                                      grobs = grid::rectGrob(gp = grid::gpar(fill = NA, lwd = 1)),
                                      t = 1,
                                      b = nrow(sbp_dow_table),
                                      l = 1,
                                      r = ncol(sbp_dow_table)
                                  )


  sbptitle <- grid::textGrob("SBP by Day of the Week", gp = grid::gpar(fontsize = 15))
  padding <-ggplot2::unit(7, "mm")

  sbp_dow_table <- gtable::gtable_add_rows(
    sbp_dow_table,
    heights = grid::grobHeight(sbptitle) + padding,
    pos = 0)

  sbp_dow_table <- gtable::gtable_add_grob(
    sbp_dow_table,
    sbptitle,
    1, 1, 1, ncol(sbp_dow_table))





  ###################
  # DBP Day of Week #
  ###################


  # Extract necessary data
  dbp_dow_data <- bp_tables(data)['DBP_by_Day_of_Week'][[1]]

  # Convert row names to column --> tibble package
  dbp_dow_data <- tibble::rownames_to_column(dbp_dow_data, var = "BP Stage")

  # Add column totals --> dplyr for bind_rows
  # See https://github.com/r-lib/tidyselect/issues/201 for issue with "where" in dplyr's across function
  # https://dplyr.tidyverse.org/reference/across.html - dplyr documentation of how where is used within across
  # https://stackoverflow.com/questions/40251801/how-to-use-utilsglobalvariables - alternative temporary fix using
  #                                                                                               global variables
  dbp_dow_data <- dbp_dow_data %>%
    dplyr::bind_rows(dplyr::summarise(.,
                               dplyr::across(tidyselect::vars_select_helpers$where(is.numeric), sum),
                               dplyr::across(tidyselect::vars_select_helpers$where(is.character), ~"Total")))


  # Stages
  dbp_stage <- gridExtra::tableGrob(dbp_dow_data["BP Stage"],
                        theme = gridExtra::ttheme_default(
                          colhead = list(bg_params=list(fill="white", col="grey90")),
                          base_size = 7, padding = unit(c(4,3),"mm")),
                        rows = NULL)

  dbp_dow_table <- gridExtra::gtable_cbind(dbp_stage)

  # Loop over all days of the week
  for(dfcol in names(dbp_dow_data)[2:(length(names(dbp_dow_data)) - 1)]){

    cols_day <- grDevices::colorRampPalette(c("#FFE0B2", "#FFA000", "#E65100"))(nrow(dbp_dow_data))[rank(dbp_dow_data[[dfcol]][1:6])]
    day_dat <- as.data.frame(dbp_dow_data[dfcol][1:6,])
    colnames(day_dat) <- dfcol
    day <- gridExtra::tableGrob(day_dat,
                     theme = gridExtra::ttheme_default(
                       core=list(bg_params = list(fill=cols_day)),
                       colhead = list(bg_params=list(fill="white", col="grey90")),
                       base_size = 7, padding = unit(c(4,3),"mm")),
                     rows = NULL)
    day_tot <- as.data.frame(dbp_dow_data[dfcol][7,])
    colnames(day_tot) <- dfcol

    day_tot_grob <- gridExtra::tableGrob(day_tot, rows = NULL, cols = NULL,
                              theme = gridExtra::ttheme_default(base_size = 7, padding = unit(c(4,3),"mm")))

    day <- gridExtra::gtable_rbind(day, day_tot_grob)

    dbp_dow_table <- gridExtra::gtable_cbind(dbp_dow_table, day)
  }


  # Totals
  dbptotals <- gridExtra::tableGrob(dbp_dow_data["Total"],
                      theme = gridExtra::ttheme_default(
                        colhead = list(bg_params=list(fill="white", col="grey90")),
                        base_size = 7, padding = unit(c(4,3),"mm")),
                      rows = NULL)

  dbp_dow_table <- gridExtra::gtable_cbind(dbp_dow_table, dbptotals)


  dbp_dow_table <- gtable::gtable_add_grob(dbp_dow_table,
                                   grobs = grid::rectGrob(gp = grid::gpar(fill = NA, lwd = 2)),
                                   t = 2,
                                   b = nrow(dbp_dow_table)-1,
                                   l = 2,
                                   r = ncol(dbp_dow_table)-1
  )

  dbp_dow_table <- gtable::gtable_add_grob(dbp_dow_table,
                                   grobs = grid::rectGrob(gp = grid::gpar(fill = NA, lwd = 1)),
                                   t = 1,
                                   b = nrow(dbp_dow_table),
                                   l = 1,
                                   r = ncol(dbp_dow_table)
  )


  dbptitle <- grid::textGrob("DBP by Day of the Week", gp = grid::gpar(fontsize = 15))
  padding <- ggplot2::unit(7, "mm")

  dbp_dow_table <- gtable::gtable_add_rows(
    dbp_dow_table,
    heights = grid::grobHeight(dbptitle) + padding,
    pos = 0)

  dbp_dow_table <- gtable::gtable_add_grob(
    dbp_dow_table,
    dbptitle,
    1, 1, 1, ncol(dbp_dow_table))





  ###################
  # SBP Time of Day #
  ###################

  # Extract necessary data
  sbp_tod_data <- bp_tables(data)['SBP_by_Time_of_Day'][[1]]

  # Convert row names to column --> tibble package
  sbp_tod_data <- tibble::rownames_to_column(sbp_tod_data, var = "BP Stage")

  # Add column totals --> dplyr for bind_rows
  # See https://github.com/r-lib/tidyselect/issues/201 for issue with "where" in dplyr's across function
  # https://dplyr.tidyverse.org/reference/across.html - dplyr documentation of how where is used within across
  # https://stackoverflow.com/questions/40251801/how-to-use-utilsglobalvariables - alternative temporary fix using
  #                                                                                               global variables
  sbp_tod_data <- sbp_tod_data %>%
    dplyr::bind_rows(dplyr::summarise(.,
                               dplyr::across(tidyselect::vars_select_helpers$where(is.numeric), sum),
                               dplyr::across(tidyselect::vars_select_helpers$where(is.character), ~"Total")))


  # Stages
  sbp_stage <- gridExtra::tableGrob(sbp_tod_data["BP Stage"],
                         theme = gridExtra::ttheme_default(
                           colhead = list(bg_params=list(fill="white", col="grey90")),
                           base_size = 7, padding = unit(c(4,3),"mm")),
                         rows = NULL)

  sbp_tod_table <- gridExtra::gtable_cbind(sbp_stage)

  # Loop over all times of the day
  for(dfcol in names(sbp_tod_data)[2:(length(names(sbp_tod_data)) - 1)]){

    #cols_tm <- colorRampPalette(c("#FFE0B2", "#FFA000", "#E65100"))(nrow(sbp_tod_data))[rank(sbp_tod_data[[dfcol]][1:6])]
    cols_tm <- grDevices::colorRampPalette(c("#DDDDFF", "#AABBFF", "#3366EE"))(nrow(sbp_tod_data))[rank(sbp_tod_data[[dfcol]][1:6])]

    tm_dat <- as.data.frame(sbp_tod_data[dfcol][1:6,])
    colnames(tm_dat) <- dfcol
    tm <- gridExtra::tableGrob(tm_dat,
                     theme = gridExtra::ttheme_default(
                       core=list(bg_params = list(fill=cols_tm)),
                       colhead = list(bg_params=list(fill="white", col="grey90")),
                       base_size = 7, padding = unit(c(4,3),"mm")),
                     rows = NULL)
    tm_tot <- as.data.frame(sbp_tod_data[dfcol][7,])
    colnames(tm_tot) <- dfcol

    tm_tot_grob <- gridExtra::tableGrob(tm_tot, rows = NULL, cols = NULL,
                             theme = gridExtra::ttheme_default(base_size = 7, padding = unit(c(4,3),"mm")))

    tm <- gridExtra::gtable_rbind(tm, tm_tot_grob)

    sbp_tod_table <- gridExtra::gtable_cbind(sbp_tod_table, tm)
  }


  # Totals
  sbptotals <- gridExtra::tableGrob(sbp_tod_data["Total"],
                         theme = gridExtra::ttheme_default(
                           colhead = list(bg_params=list(fill="white", col="grey90")),
                           base_size = 7, padding = unit(c(4,3),"mm")),
                         rows = NULL)

  sbp_tod_table <- gridExtra::gtable_cbind(sbp_tod_table, sbptotals)


  sbp_tod_table <- gtable::gtable_add_grob(sbp_tod_table,
                                   grobs = grid::rectGrob(gp = grid::gpar(fill = NA, lwd = 2)),
                                   t = 2,
                                   b = nrow(sbp_tod_table)-1,
                                   l = 2,
                                   r = ncol(sbp_tod_table)-1
  )

  sbp_tod_table <- gtable::gtable_add_grob(sbp_tod_table,
                                   grobs = grid::rectGrob(gp = grid::gpar(fill = NA, lwd = 1)),
                                   t = 1,
                                   b = nrow(sbp_tod_table),
                                   l = 1,
                                   r = ncol(sbp_tod_table)
  )


  sbptitle <- grid::textGrob("SBP by Time of Day", gp = grid::gpar(fontsize = 15))
  padding <- ggplot2::unit(7, "mm")

  sbp_tod_table <- gtable::gtable_add_rows(
    sbp_tod_table,
    heights = grid::grobHeight(sbptitle) + padding,
    pos = 0)

  sbp_tod_table <- gtable::gtable_add_grob(
    sbp_tod_table,
    sbptitle,
    1, 1, 1, ncol(sbp_tod_table))






  ###################
  # DBP Time of Day #
  ###################

  # Extract necessary data
  dbp_tod_data <- bp_tables(data)['DBP_by_Time_of_Day'][[1]]

  # Convert row names to column --> tibble package
  dbp_tod_data <- tibble::rownames_to_column(dbp_tod_data, var = "BP Stage")

  # Add column totals --> dplyr for bind_rows
  # See https://github.com/r-lib/tidyselect/issues/201 for issue with "where" in dplyr's across function
  # https://dplyr.tidyverse.org/reference/across.html - dplyr documentation of how where is used within across
  # https://stackoverflow.com/questions/40251801/how-to-use-utilsglobalvariables - alternative temporary fix using
  #                                                                                               global variables
  dbp_tod_data <- dbp_tod_data %>%
    dplyr::bind_rows(dplyr::summarise(.,
                               dplyr::across(tidyselect::vars_select_helpers$where(is.numeric), sum),
                               dplyr::across(tidyselect::vars_select_helpers$where(is.character), ~"Total")))


  # Stages
  dbp_stage <- gridExtra::tableGrob(dbp_tod_data["BP Stage"],
                         theme = gridExtra::ttheme_default(
                           colhead = list(bg_params=list(fill="white", col="grey90")),
                           base_size = 7, padding = unit(c(4,3),"mm")),
                         rows = NULL)

  dbp_tod_table <- gridExtra::gtable_cbind(dbp_stage)

  # Loop over all times of the day
  for(dfcol in names(dbp_tod_data)[2:(length(names(dbp_tod_data)) - 1)]){

    #cols_tm <- colorRampPalette(c("#FFE0B2", "#FFA000", "#E65100"))(nrow(sbp_tod_data))[rank(sbp_tod_data[[dfcol]][1:6])]
    cols_tm <- grDevices::colorRampPalette(c("#DDDDFF", "#AABBFF", "#3366EE"))(nrow(dbp_tod_data))[rank(dbp_tod_data[[dfcol]][1:6])]

    tm_dat <- as.data.frame(dbp_tod_data[dfcol][1:6,])
    colnames(tm_dat) <- dfcol
    tm <- gridExtra::tableGrob(tm_dat,
                    theme = gridExtra::ttheme_default(
                      core=list(bg_params = list(fill=cols_tm)),
                      colhead = list(bg_params=list(fill="white", col="grey90")),
                      base_size = 7, padding = unit(c(4,3),"mm")),
                    rows = NULL)
    tm_tot <- as.data.frame(dbp_tod_data[dfcol][7,])
    colnames(tm_tot) <- dfcol

    tm_tot_grob <- gridExtra::tableGrob(tm_tot, rows = NULL, cols = NULL,
                             theme = gridExtra::ttheme_default(base_size = 7, padding = unit(c(4,3),"mm")))

    tm <- gridExtra::gtable_rbind(tm, tm_tot_grob)

    dbp_tod_table <- gridExtra::gtable_cbind(dbp_tod_table, tm)
  }


  # Totals
  dbptotals <- gridExtra::tableGrob(dbp_tod_data["Total"],
                         theme = gridExtra::ttheme_default(
                           colhead = list(bg_params=list(fill="white", col="grey90")),
                           base_size = 7, padding = unit(c(4,3),"mm")),
                         rows = NULL)

  dbp_tod_table <- gridExtra::gtable_cbind(dbp_tod_table, dbptotals)


  dbp_tod_table <- gtable::gtable_add_grob(dbp_tod_table,
                                   grobs = grid::rectGrob(gp = grid::gpar(fill = NA, lwd = 2)),
                                   t = 2,
                                   b = nrow(dbp_tod_table)-1,
                                   l = 2,
                                   r = ncol(dbp_tod_table)-1
  )

  dbp_tod_table <- gtable::gtable_add_grob(dbp_tod_table,
                                   grobs = grid::rectGrob(gp = grid::gpar(fill = NA, lwd = 1)),
                                   t = 1,
                                   b = nrow(dbp_tod_table),
                                   l = 1,
                                   r = ncol(dbp_tod_table)
  )


  dbptitle <- grid::textGrob("DBP by Time of Day", gp = grid::gpar(fontsize = 15))
  padding <- ggplot2::unit(7, "mm")

  dbp_tod_table <- gtable::gtable_add_rows(
    dbp_tod_table,
    heights = grid::grobHeight(dbptitle) + padding,
    pos = 0)

  dbp_tod_table <- gtable::gtable_add_grob(
    dbp_tod_table,
    dbptitle,
    1, 1, 1, ncol(dbp_tod_table))




  ## Return all table visuals in a list

  out <- list(sbp_dow_table, dbp_dow_table, sbp_tod_table, dbp_tod_table)
  return(out)

}

