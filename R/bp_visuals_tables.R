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
#' data("bp_hypnos")
#' hyp_proc <- process_data(bp_hypnos,
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
#' rm(bp_hypnos)
#'
#' dow_tod_plots_out <- dow_tod_plots(hyp_proc)
#' grid::grid.draw(
#'    gridExtra::grid.arrange(dow_tod_plots_out[[1]], dow_tod_plots_out[[2]], ncol = 2)
#'  )
#'
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
        dplyr::filter(ID %in% subj)

    }
  }

  # This part is inefficient as don't need all tables and may not need to call at all if date is missing, but it works. Possibly come to this later
  table_data <- bp_tables(data)

  ft_size = 6

  ## Day of Week ##
  if( ("DAY_OF_WEEK" %in% names(data)) == TRUE ){
      tables_dow <- table_data$CLASS_Day_of_Week

      tables_dow <- tibble::rownames_to_column(table_data$CLASS_Day_of_Week, var = "BP Stage")
      tables_dow <- tables_dow %>%
      dplyr::bind_rows(dplyr::summarise(.,
                                        dplyr::across(tidyselect::vars_select_helpers$where(is.numeric), sum),
                                        dplyr::across(tidyselect::vars_select_helpers$where(is.character), ~"Total")))

        dow <- gridExtra::tableGrob(tables_dow, rows = NULL, theme = gridExtra::ttheme_default(base_size = ft_size))
        dow <- gtable::gtable_add_grob(dow,
                                       grobs = grid::rectGrob(gp = grid::gpar(fill = NA, lwd = 2)),
                                       t = 2, b = nrow(dow)-1, l = 1, r = ncol(dow))
        dow <- gtable::gtable_add_grob(dow,
                                       grobs = grid::rectGrob(gp = grid::gpar(fill = NA, lwd = 2)),
                                       t = 1, l = 1, r = ncol(dow))
        dow <- gtable::gtable_add_grob(dow,
                                       grobs = grid::rectGrob(gp = grid::gpar(fill = NA, lwd = 2)),
                                       t = nrow(dow), b = nrow(dow), l = 1, r = ncol(dow))
        dow <- gtable::gtable_add_grob(dow,
                                       grobs = grid::rectGrob(gp = grid::gpar(fill = NA, lwd = 2)),
                                       t = 2, b = nrow(dow), l = ncol(dow), r = ncol(dow))
        dow <- gtable::gtable_add_grob(dow,
                                       grobs = grid::rectGrob(gp = grid::gpar(fill = NA, lwd = 2)),
                                       t = 2, b = nrow(dow), l = 2, r = ncol(dow)-1)

        # Add title for plot
        title_dow <- grid::textGrob("BP by Day of Week", gp = grid::gpar(fontsize = 15))
        padding <- grid::unit(7, "mm")

        table_dow <- gtable::gtable_add_rows(
                                dow,
                                heights = grid::grobHeight(title_dow) + padding,
                                pos = 0)

        dow <- gtable::gtable_add_grob(
                                table_dow,
                                title_dow,
                                1, 1, 1, ncol(table_dow))

    # display plot
    dow_out <- gridExtra::arrangeGrob(dow)
  }else{
    # Empty grob
    dow_out <- grid::grid.rect(gp = grid::gpar(col = NA))
  }


  ## Time of Day ##
  if( ("TIME_OF_DAY" %in% names(data)) == TRUE ){
    tables_tod <- table_data$CLASS_Time_of_Day

    tables_tod <- tibble::rownames_to_column(tables_tod, var = "BP Stage")

    tables_tod <- tables_tod %>%
        dplyr::bind_rows(dplyr::summarise(.,
                                        dplyr::across(tidyselect::vars_select_helpers$where(is.numeric), sum),
                                        dplyr::across(tidyselect::vars_select_helpers$where(is.character), ~"Total")))

        tod <- gridExtra::tableGrob(tables_tod, rows = NULL, theme = gridExtra::ttheme_default(base_size = ft_size))
        tod <- gtable::gtable_add_grob(tod,
                                       grobs = grid::rectGrob(gp = grid::gpar(fill = NA, lwd = 2)),
                                       t = 2, b = nrow(tod)-1, l = 1, r = ncol(tod))
        tod <- gtable::gtable_add_grob(tod,
                                       grobs = grid::rectGrob(gp = grid::gpar(fill = NA, lwd = 2)),
                                       t = 1, l = 1, r = ncol(tod))
        tod <- gtable::gtable_add_grob(tod,
                                       grobs = grid::rectGrob(gp = grid::gpar(fill = NA, lwd = 2)),
                                       t = nrow(tod), b = nrow(tod), l = 1, r = ncol(tod))
        tod <- gtable::gtable_add_grob(tod,
                                       grobs = grid::rectGrob(gp = grid::gpar(fill = NA, lwd = 2)),
                                       t = 2, b = nrow(tod), l = ncol(tod), r = ncol(tod))
        tod <- gtable::gtable_add_grob(tod,
                                       grobs = grid::rectGrob(gp = grid::gpar(fill = NA, lwd = 2)),
                                       t = 2, b = nrow(tod), l = 2, r = ncol(tod)-1)

        # Add title for plot
        title_tod <- grid::textGrob("BP by Time of Day", gp = grid::gpar(fontsize = 15))
        padding <- grid::unit(7, "mm")

        table_tod <- gtable::gtable_add_rows(
                              tod,
                              heights = grid::grobHeight(title_tod) + padding,
                              pos = 0)

        tod <- gtable::gtable_add_grob(
                              table_tod,
                              title_tod,
                              1, 1, 1, ncol(table_tod))

    # display plot
    tod_out <- gridExtra::arrangeGrob(tod)
  }else{
    # Empty grob
    tod_out <- grid::grid.rect(gp = grid::gpar(col = NA))
  }

  return(list(dow = dow_out, tod = tod_out))
}







































dow_tod_plots_old <- function(data, subj = NULL){


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



  table_build <- function(data){

    # Load all tables one time into sbp_dow_data object
    sbp_dow_data <- bp_tables(data)

    table_id <- c('SBP_by_Day_of_Week', 'DBP_by_Day_of_Week', 'SBP_by_Time_of_Day', 'DBP_by_Time_of_Day')
    title_names <- c("SBP by Day of the Week", "DBP by Day of the Week", "SBP by Time of the Day", "DBP by Time of the Day")

    out_tables <- list()

    for(i in table_id){

      # Clear plot area for initial / next iteration
      grid::grid.newpage()

      # Convert row names of table to column --> tibble package
      loop_data <- tibble::rownames_to_column(sbp_dow_data[i][[1]], var = "BP Stage")

      # Add column totals --> dplyr for bind_rows
      # See https://github.com/r-lib/tidyselect/issues/201 for issue with "where" in dplyr's across function
      # https://dplyr.tidyverse.org/reference/across.html - dplyr documentation of how where is used within across
      # https://stackoverflow.com/questions/40251801/how-to-use-utilsglobalvariables - alternative temporary fix using
      #                                                                                               global variables
      loop_data <- loop_data %>%
        dplyr::bind_rows(dplyr::summarise(.,
                                          dplyr::across(tidyselect::vars_select_helpers$where(is.numeric), sum),
                                          dplyr::across(tidyselect::vars_select_helpers$where(is.character), ~"Total")))

      # Format BP Stage column
      stage_col <- gridExtra::tableGrob(loop_data["BP Stage"],
                                        theme = gridExtra::ttheme_default(
                                          colhead = list(bg_params=list(fill="white", col="grey90")),
                                          base_size = 7, padding = grid::unit(c(4,3),"mm")),
                                        rows = NULL)

      loop_table <- gridExtra::gtable_cbind(stage_col)


      # Loop over each of the columns other than total and row name to format properly
      for(dfcol in names(loop_data)[2: (length(names(loop_data)) - 1) ]){

        # Color palette for either day of the week or time of day
        color_pal = ifelse( table_id %in% c('SBP_by_Day_of_Week' , 'SBP_by_Day_of_Week') ,
                            grDevices::colorRampPalette(c("#FFE0B2", "#FFA000", "#E65100"))(nrow(loop_data))[rank(loop_data[[dfcol]][1:6])],
                            grDevices::colorRampPalette(c("#DDDDFF", "#AABBFF", "#3366EE"))(nrow(loop_data))[rank(loop_data[[dfcol]][1:6])])

        # Isolate the column data for each of the dfcol days / times of day
        col_data <- as.data.frame(loop_data[dfcol][1:6,])

        # Change colname to dfcol name (i.e. Sun, Mon, etc)
        colnames(col_data) <- dfcol


        col_grob <- gridExtra::tableGrob(col_data,
                                         theme = gridExtra::ttheme_default(
                                           core=list(bg_params = list(fill = color_pal )),
                                           colhead = list(bg_params=list(fill="white", col="grey90")),
                                           base_size = 7, padding = grid::unit(c(4,3),"mm")),
                                         rows = NULL)


        col_total <- as.data.frame(loop_data[dfcol][7,])
        colnames(col_total) <- dfcol

        col_total_grob <- gridExtra::tableGrob(col_total, rows = NULL, cols = NULL,
                                               theme = gridExtra::ttheme_default(base_size = 7, padding = grid::unit(c(4,3),"mm")))

        col_grob <- gridExtra::gtable_rbind(col_grob, col_total_grob)

        loop_table <- gridExtra::gtable_cbind(loop_table, col_grob)

      }


      # Total column formatting
      grobtotals <- gridExtra::tableGrob(loop_data["Total"],
                                         theme = gridExtra::ttheme_default(
                                           colhead = list(bg_params=list(fill="white", col="grey90")),
                                           base_size = 7, padding = grid::unit(c(4,3),"mm")),
                                         rows = NULL)

      # Attach Totals column to rest of table
      loop_table <- gridExtra::gtable_cbind(loop_table, grobtotals)

      # Draw border around main part of table
      loop_table <- gtable::gtable_add_grob(loop_table,
                                            grobs = grid::rectGrob(gp = grid::gpar(fill = NA, lwd = 2)),
                                            t = 2,
                                            b = nrow(loop_table)-1,
                                            l = 2,
                                            r = ncol(loop_table)-1
      )

      # Draw grid around the whole table
      loop_table <- gtable::gtable_add_grob(loop_table,
                                            grobs = grid::rectGrob(gp = grid::gpar(fill = NA, lwd = 1)),
                                            t = 1,
                                            b = nrow(loop_table),
                                            l = 1,
                                            r = ncol(loop_table)
      )

      # Add title and padding
      grobtitle <- grid::textGrob(title_names[match(i, table_id)], gp = grid::gpar(fontsize = 15))
      padding <- ggplot2::unit(7, "mm")

      loop_table <- gtable::gtable_add_rows(
        loop_table,
        heights = grid::grobHeight(grobtitle) + padding,
        pos = 0)

      loop_table <- gtable::gtable_add_grob(
        loop_table,
        grobtitle,
        1, 1, 1, ncol(loop_table))


      # Add current iteration of table to list
      out_tables[[i]] <- loop_table

    }

    return(out_tables)

  }

  ## Return all table visuals in a list
  out <- table_build(data)
  return(out)

}

