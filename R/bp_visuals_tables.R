dow_tod_plots <- function(data){


  # Primary variables needed:    SBP, DBP, Weekday, Time_of_Day
  # Secondary variables needed:  SBP_Category (bp_tables), DBP_Category (bp_tables)

  SBP = DBP = Weekday = Time_of_Day = Sun = Mon = Tue = Wed = Thu = Fri = Sat = Total = Morning = Afternoon = Evening = Night = NULL
  rm(list = c("SBP", "DBP", "Weekday", "Time_of_Day",
              "Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Total",
              "Morning", "Afternoon", "Evening", "Night"))

  # 1) Create tables using gt
  # 2) Save finished tables as png images in temp directory
  # 3) Use temp directory to pull and combine using grid.arrange


  # SBP Day of Week
  sbp_dow_table <- gt::gt(data = bp_tables(data)['SBP_by_Day_of_Week'][[1]], rownames_to_stub = TRUE) %>%

    gt::summary_rows(fns = list(Total = ~sum(., na.rm = TRUE)), decimals = 0) %>%

    gt::tab_header(
      title = gt::md("**SBP by Day of the Week**")#,
      #subtitle = gt::md("*test subtitle*")
    ) %>%

    gt::data_color(
      columns = gt::vars(Sun, Mon, Tue, Wed, Thu, Fri, Sat),
      colors = scales::col_numeric(
        palette = paletteer::paletteer_d(
          palette = "ggsci::yellow_material"
        ) %>% as.character(),
        domain = NULL
      )
    ) %>%

    gt::tab_options(
      # hide the top-most border
      table.border.top.color = "white",

      # make the title size match the body text
      #heading.title.font.size = px(16),

      # change the column labels section
      column_labels.border.top.width = 3,
      column_labels.border.top.color = "gray",
      column_labels.border.bottom.width = 3,
      column_labels.border.bottom.color = "black",

      # hide the bottom-most line or footnotes
      # will have a border
      table.border.bottom.color = "white",

      table.background.color = "white",

      grand_summary_row.border.color = "black"
    ) %>%

    # Alight table values to the center
    gt::cols_align(align="center") %>%

    # Add Gray line between body and Total column
    gt::tab_style(
      style = list(
        gt::cell_borders(
          sides = "left",
          color = "gray57",
          weight = gt::px(2)
        )
      ),
      locations = list(
        gt::cells_body(
          columns = gt::vars(Total)
        )
      )
    ) %>%

    # Bold the Total Column values
    gt::tab_style(
      style = list(
        gt::cell_text(
          color = "gray",
          weight = "bold"
        )
      ),
      locations = list(
        gt::cells_body(
          columns = gt::vars(Total)
        )
      )
    )



  # SBP Time of Day
  sbp_tod_table <- gt::gt(data = bp_tables(data)['SBP_by_Time_of_Day'][[1]], rownames_to_stub = TRUE) %>%

    gt::summary_rows(fns = list(Total = ~sum(., na.rm = TRUE)), decimals = 0) %>%

    gt::tab_header(
      title = gt::md("**SBP by Time of Day**")#,
      #subtitle = "Subtitle"
    ) %>%

    gt::data_color(
      columns = gt::vars(Morning, Afternoon, Evening, Night),
      colors = scales::col_numeric(
        palette = paletteer::paletteer_d(
          #palette = "ggsci::purple_material"
          palette = "ggsci::yellow_material"
        ) %>% as.character(),
        domain = NULL
      )
    ) %>%

    gt::tab_options(
      # hide the top-most border
      table.border.top.color = "white",

      # make the title size match the body text
      #heading.title.font.size = px(16),

      # change the column labels section
      column_labels.border.top.width = 3,
      column_labels.border.top.color = "gray",
      column_labels.border.bottom.width = 3,
      column_labels.border.bottom.color = "black",

      # hide the bottom-most line or footnotes
      # will have a border
      table.border.bottom.color = "white",

      table.background.color = "white",

      grand_summary_row.border.color = "black"
    ) %>%

    # Alight table values to the center
    gt::cols_align(align="center") %>%

    # Add Gray line between body and Total column
    gt::tab_style(
      style = list(
        gt::cell_borders(
          sides = "left",
          color = "gray57",
          weight = gt::px(2)
        )
      ),
      locations = list(
        gt::cells_body(
          columns = gt::vars(Total)
        )
      )
    ) %>%

    # Bold the Total Column values
    gt::tab_style(
      style = list(
        gt::cell_text(
          color = "gray",
          weight = "bold"
        )
      ),
      locations = list(
        gt::cells_body(
          columns = gt::vars(Total)
        )
      )
    )




  # DBP Day of Week
  dbp_dow_table <- gt::gt(data = bp_tables(data)['DBP_by_Day_of_Week'][[1]], rownames_to_stub = TRUE) %>%

    gt::summary_rows(fns = list(Total = ~sum(., na.rm = TRUE)), decimals = 0) %>%

    gt::tab_header(
      title = gt::md("**DBP by Day of the Week**")#,
      #subtitle = "Subtitle"
    ) %>%

    gt::data_color(
      columns = gt::vars(Sun, Mon, Tue, Wed, Thu, Fri, Sat),
      colors = scales::col_numeric(
        palette = paletteer::paletteer_d(
          palette = "ggsci::yellow_material"
        ) %>% as.character(),
        domain = NULL
      )
    ) %>%

    gt::tab_options(
      # hide the top-most border
      table.border.top.color = "white",

      # make the title size match the body text
      #heading.title.font.size = px(16),

      # change the column labels section
      column_labels.border.top.width = 3,
      column_labels.border.top.color = "gray",
      column_labels.border.bottom.width = 3,
      column_labels.border.bottom.color = "black",

      # hide the bottom-most line or footnotes
      # will have a border
      table.border.bottom.color = "white",

      table.background.color = "white",

      grand_summary_row.border.color = "black"
    ) %>%

    # Alight table values to the center
    gt::cols_align(align="center") %>%

    # Add Gray line between body and Total column
    gt::tab_style(
      style = list(
        gt::cell_borders(
          sides = "left",
          color = "gray57",
          weight = gt::px(2)
        )
      ),
      locations = list(
        gt::cells_body(
          columns = gt::vars(Total)
        )
      )
    ) %>%

    # Bold the Total Column values
    gt::tab_style(
      style = list(
        gt::cell_text(
          color = "gray",
          weight = "bold"
        )
      ),
      locations = list(
        gt::cells_body(
          columns = gt::vars(Total)
        )
      )
    )





  # DBP Time of Day
  dbp_tod_table <- gt::gt(data = bp_tables(data)['DBP_by_Time_of_Day'][[1]], rownames_to_stub = TRUE) %>%

    gt::summary_rows(fns = list(Total = ~sum(., na.rm = TRUE)), decimals = 0) %>%

    gt::tab_header(
      title = gt::md("**DBP by Time of Day**")#,
      #subtitle = "Subtitle"
    ) %>%

    gt::data_color(
      columns = gt::vars(Morning, Afternoon, Evening, Night),
      colors = scales::col_numeric(
        palette = paletteer::paletteer_d(
          #palette = "ggsci::purple_material"
          palette = "ggsci::yellow_material"
        ) %>% as.character(),
        domain = NULL
      )
    ) %>%

    gt::tab_options(
      # hide the top-most border
      table.border.top.color = "white",

      # make the title size match the body text
      #heading.title.font.size = px(16),

      # change the column labels section
      column_labels.border.top.width = 3,
      column_labels.border.top.color = "gray",
      column_labels.border.bottom.width = 3,
      column_labels.border.bottom.color = "black",

      # hide the bottom-most line or footnotes
      # will have a border
      table.border.bottom.color = "white",

      table.background.color = "white",

      grand_summary_row.border.color = "black"
    ) %>%

    # Alight table values to the center
    gt::cols_align(align="center") %>%

    # Add Gray line between body and Total column
    gt::tab_style(
      style = list(
        gt::cell_borders(
          sides = "left",
          color = "gray57",
          weight = gt::px(2)
        )
      ),
      locations = list(
        gt::cells_body(
          columns = gt::vars(Total)
        )
      )
    ) %>%

    # Bold the Total Column values
    gt::tab_style(
      style = list(
        gt::cell_text(
          color = "gray",
          weight = "bold"
        )
      ),
      locations = list(
        gt::cells_body(
          columns = gt::vars(Total)
        )
      )
    )


  tmploc <- tempdir()

  # Save to PNG
  sbp_dow_table %>% gt::gtsave(filename = file.path(tmploc, "sbp_dow.png") )
  dbp_dow_table %>% gt::gtsave(filename = file.path(tmploc, "dbp_dow.png") )

  sbp_tod_table %>% gt::gtsave(filename = file.path(tmploc, "sbp_tod.png") )
  dbp_tod_table %>% gt::gtsave(filename = file.path(tmploc, "dbp_tod.png") )

  # Retrieve PNGs
  img1 <- grid::rasterGrob(png::readPNG( file.path(tmploc, "sbp_dow.png") ))
  img2 <- grid::rasterGrob(png::readPNG( file.path(tmploc, "dbp_dow.png") ))

  img3 <- grid::rasterGrob(png::readPNG( file.path(tmploc, "sbp_tod.png") ))
  img4 <- grid::rasterGrob(png::readPNG( file.path(tmploc, "dbp_tod.png") ))


  #t1 <- grid.arrange(img1, img2, img3, img4, nrow = 2, ncol = 2)

  out <- list(img1, img2, img3, img4)
  return(out)

}


# Example:
# dow_tod_plots_tmp <- dow_tod_plots(data)
# grid.arrange(dow_tod_plots_tmp[[1]], dow_tod_plots_tmp[[2]],
#              dow_tod_plots_tmp[[3]],dow_tod_plots_tmp[[4]],
#              nrow = 2, ncol = 2)



