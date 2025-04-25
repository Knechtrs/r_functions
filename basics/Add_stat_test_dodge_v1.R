Add_stat_test_dodge <- function(plot, yData, Group, Dodge = NULL, test = "t.test", Facet = NULL, paired = FALSE, id = NULL, FontSize = 12, yPosition = NULL) {
  
  # Extract data from ggplot object
  df <- plot$data
  
  # create function to rename column names so they are always the same
  rename_Col <- function(df, Name, NewName) {
    if (!is.null(Name)) {
      df <- df %>%
        dplyr::rename(!!NewName := !!rlang::sym(Name))
    }
    return(df)
  }
  
  df <- df %>%
    rename_Col(Name = yData, NewName = "yData_col") %>%
    rename_Col(Name = Group, NewName = "Group_col") %>%
    rename_Col(Name = Dodge, NewName = "Dodge_col") %>%
    rename_Col(Name = Facet, NewName = "Facet_col") 
  
  
  # Add 'id' if provided and needed
  if (!is.null(id) && paired) {
    df <- df %>% rename_Col(Name = id, NewName = "ID_Col")
  }
  
  #---- stat test ----#
  formula <- stats::as.formula("yData_col ~ Group_col")
  
  # Choose the test function based on input
  test_fun <- switch(test,
                     "t.test" = rstatix::t_test,
                     "wilcox.test" = rstatix::wilcox_test,
                     stop("Invalid test specified. Use 't.test' or 'wilcox.test'.")
  )
  
  # Apply the correct grouping or paired test
  if (paired && !is.null(id)) {
    stat.test <- test_fun(df, formula, paired = TRUE, id = "ID_col")
  } else if (!is.null(Dodge)) {
    stat.test <- df %>% 
      dplyr::group_by(Dodge_col) %>% 
      test_fun(formula, paired = paired)
  } else if ("Facet_col" %in% colnames(df)) {
    stat.test <- df %>%
      dplyr::group_by(Facet_col) %>%
      test_fun(formula, paired = paired)
  } else {
    stat.test <- test_fun(df, formula, paired = paired)
  }

  
  # # Add significance and plot position
  stat.test <- stat.test %>%
    rstatix::add_significance() %>%
    add_xy_position(x = "Group_col", fun = "max") %>%
    dplyr::mutate(
      yMax = if (!is.null(yPosition)) yPosition else y.position * 1.05,
      p_formatted = format_pvalue(p)
    )
  
  #---- for facet_wrap: re-calculate y.position ----#
  if(!is.null(Facet)) {
    # Step 1: Calculate max y per facet from the original data
    y_max_per_facet <- df %>%
      dplyr::group_by(Facet_col) %>%
      dplyr::summarise(y_max = max(yData_col, na.rm = TRUE), .groups = "drop")
    
    # Step 2: Join to stat.test
    stat.test <- stat.test %>%
      dplyr::left_join(y_max_per_facet, by = "Facet_col") %>% 
      mutate(
        yMax = if (!is.null(yPosition)) yPosition else y_max * 1.05 ) %>% 
      select(-y_max)
  
  # need to rename column, so stat_pvalue_manual knows it for facetting
  stat.test <- stat.test %>%
    dplyr::rename(Parameter = Facet_col)
  }
  
  #---- for doge: re-calculate x-positions ----#
  if(!is.null(Dodge)) {
    stat.test <- stat.test %>%
      add_xy_position(x = "Group_col", fun = "max", dodge = plot$layers[[1]]$position$dodge.width, group = "Dodge_col") %>% 
      dplyr::mutate(
        yMax = y.position +
          0 +  # base bump
          as.numeric(factor(Dodge_col)) * 0.15  # additional offset per group
      )
  }

  #---- add labels to plot ----# 
  
  # Set linetype and tip.length first
  linetype_val <- if (!is.null(Dodge)) "solid" else "blank"
  tip_length_val <- if (linetype_val == "solid") 0.05 else 0
  
  # Base plot with stat_pvalue_manual
  Plot_out <- plot +
    ggpubr::stat_pvalue_manual(
      data = stat.test,
      label = "p_formatted",
      y.position = "yMax",
      xmin = "xmin",
      xmax = "xmax",
      inherit.aes = FALSE,
      vjust = -0.25,
      linetype = linetype_val,
      tip.length = tip_length_val,
      size = FontSize / 2.835
    )
  
  # Conditionally add y scale
  if (!is.null(Dodge)) {
    Plot_out <- Plot_out + scale_y_continuous(limits = c(0, stat.test$y.position * 1.4))
  } else if (is.null(Dodge) && is.null(Facet)) {
    Plot_out <- Plot_out + scale_y_continuous(limits = c(0, stat.test$y.position * 1.2))
  }
  
  # Final plot
  Plot_out
  
  # Return result
  return(list(
    Plot = Plot_out,
    data_stat = stat.test
  ))
  
}
