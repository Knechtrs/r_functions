add_stat_test_dodge <- function(
    plot, 
    yData,
    Group,  # groups to compare for stat test, set to NULL when only dodged!
    Dodge = NULL, # column name of dodge variable
    dodge_width = NULL, # need to specify dodge_width
    stat_group_by = NULL, # if dodge and group are the same: need to define group_by variable for stat.test
    test_across_group = FALSE, ## not yet in use! ## set to TRUE if test across group in dodge situation 
    test = "t.test", # choose: t.test, wilcox, or permutation
    Facet = NULL, # column name for faceting
    paired = FALSE, # paired data? => set to TRUE and need to define paired id => id
    id = NULL, # paired id, indicating which pairs to compare
    FontSize = 12,
    yPosition = NULL, # if you want to set y-position of p-value label manually
    adjust_y_position = FALSE, # set TRUE, if additional space between labels is needed
    expand_y_0 = TRUE,
    lower_ylimit = 0,
    pval_gap = 1,   # extra space above max(data), in y units
    expand_lower_y_mult = 0,
    show_brackets = FALSE, # show lines comparing groups?
    bracket_dist = NULL, # increase space between brackets in y-axis units. needs to be set!
    format_pvalue_dif = NULL, # optional function to format p-values differently
    top_align = FALSE, # top align p-value and brackets
    show_effectsize = FALSE,      # compute effect size 
    effectsize_type = "g",       # "d" = Cohens: stand. mean difference (like z-score), "g = Hedges g: n<20", or "r = Wilcoxon r: with Wilcoxon test"
    method_sd = "z", # ("rm", "av", "z", "b", "d", "r") see help
    remove_comp_g1 = NULL, # remove specific comparison from stat.test df: group1
    remove_comp_g2 = NULL, # remove specific comparison from stat.test df: group2
    ref_group = NULL # comparison to ref group
    ) {
  
  # browser()

  # Extract data from ggplot object
  df <- plot$data
  
  df <- df %>% droplevels()
  
  #---- rename column names ----#
  # create function to rename column names so they are always the same
  rename_Col <- function(df, Name, NewName) {
    if (!is.null(Name)) {
      df <- df %>%
        dplyr::rename(!!NewName := !!rlang::sym(Name))
    }
    return(df)
  }
  
  # apply rename function: rename column names
  df <- df %>%
    rename_Col(Name = yData, NewName = "yData_col") %>%
    rename_Col(Name = Facet, NewName = "Facet_col")
  
  # check if group and dodge column are the same
  if (!is.null(Dodge) && Group == Dodge) {
    # Explicit duplication if Group == Dodge
    df <- rename_Col(df, Name = Dodge, NewName = "Dodge_col")
    df <- df %>% dplyr::mutate(Group_col = Dodge_col)
    df <- rename_Col(df, Name = stat_group_by, NewName ="stat_group_by")
  } else {
  df <- df %>% 
    rename_Col(Name = Dodge, NewName = "Dodge_col") %>%
    rename_Col(Name = Group, NewName = "Group_col")
  }
  
  # Add 'id' if provided and needed: for paired testing
  if (!is.null(id) && paired) {
    df <- df %>% rename_Col(Name = id, NewName = "ID_Col")
  }
  
  #---- stat test ----#
  formula <- stats::as.formula("yData_col ~ Group_col")
  
  # Choose the test function based on input
  test_fun <- switch(test,
                     "t.test" = rstatix::t_test,
                     "wilcox.test" = rstatix::wilcox_test,
                     "permutation" = perm_test_fun,
                     stop("Invalid test specified. Use 't.test', 'wilcox.test', or 'permutation'.")
  )
  
  ## Determine base grouping structure ##
  group_vars <- c()
  
  if (!is.null(Facet)) {
    group_vars <- c(group_vars, "Facet_col")  # always include Facet_col, when faceted
  }
  
  # set dodge and group:
  if (!is.null(stat_group_by)) { # dodge == group
    group_vars <- c(group_vars, "stat_group_by")
  } else if (!is.null(Dodge) && Group != Dodge){ # dodge != group
    group_vars <- c(group_vars, "Dodge_col")
  } else if (!is.null(Dodge) && is.null(Group)){ #only dodge
    group_vars <- c(group_vars, "Dodge_col")
  } else if (!is.null(Group) && is.null(Dodge)) {# only group
    group_vars <- c(group_vars)
  } 
  
  # check if n>=3 for each group_vars, otherwise filter df and print message
  # summarise counts per group
  
  if (!is.null(Group) && is.null(Dodge)) {
    tmp <- df %>%
      dplyr::group_by(dplyr::across(dplyr::all_of(group_vars)), Group_col) %>%
      dplyr::summarise(n = dplyr::n(), .groups = "drop")
  } else {
    tmp <- df %>%
      dplyr::group_by(dplyr::across(dplyr::all_of(group_vars))) %>%
      dplyr::summarise(n = dplyr::n(), .groups = "drop")
  }
  
  # find which groups are too small
  low_n <- tmp %>% dplyr::filter(n <= 2)
  
  if (nrow(low_n) > 0) {
    message("Groups with n <= 2 were removed: ",
            paste(apply(low_n, 1, paste, collapse = " "), collapse = "; "))
    
      # filter df to keep only groups with n > 2
    if (!is.null(Group) && is.null(Dodge)) {
      df <- df %>%
        inner_join(tmp %>% dplyr::filter(n > 2),
                   by = c(group_vars, "Group_col"))
    } else {
      df <- df %>%
        inner_join(tmp %>% dplyr::filter(n > 2),
                   by = c(group_vars))
    }
  }
  
  
  # ensure correct df order for paired testing
  # (runs AFTER group_vars is defined)
  if (paired && !is.null(id)) {
    
    # drop donors that don't have both levels of Group_col
    df <- df %>%
      dplyr::group_by(dplyr::across(dplyr::all_of(c(group_vars, "ID_Col")))) %>%
      dplyr::filter(dplyr::n_distinct(Group_col) == 2) %>%
      dplyr::ungroup()
    
    # sort so paired rows align correctly for rstatix
    df <- df %>%
      dplyr::arrange(
        dplyr::across(dplyr::all_of(group_vars)),
        ID_Col,
        Group_col
      )
  }
  
  # Apply test
  # if (length(group_vars) > 0) {
  #     stat.test <- df %>%
  #       dplyr::group_by(across(all_of(group_vars))) %>%
  #       test_fun(formula, paired = paired)
  # } else {
  #     stat.test <- test_fun(df, formula, paired = paired)
  # }
  
  if (length(group_vars) > 0) {
    stat.test <- df %>%
      dplyr::group_by(across(all_of(group_vars))) %>%
      test_fun(formula, paired = paired, ref.group = ref_group)
  } else {
    stat.test <- test_fun(df, formula, paired = paired, ref.group = ref_group)
  }
  

  #----- Add significance and xy positions and plot position ----#

  # if standard rstatix tests used
  if (!is.null(stat_group_by) && test != "permutation") { # need to calculate x-position differently, when dodge=group
    stat.test <- stat.test %>%
      rstatix::add_significance() %>%
      add_xy_position(x = "stat_group_by", fun = "max", dodge = if (!is.null(dodge_width)) dodge_width else plot$layers[[2]]$position$width) %>%
      dplyr::mutate(
        yMax = if (!is.null(yPosition)) yPosition else y.position * pval_gap * 1.05,
        p_formatted = if (!is.null(format_pvalue_dif)) format_pvalue_dif(p) else format_pvalue(p)
      )
  } else if (test != "permutation" && !is.null(Dodge)) { # if dodge != group
    stat.test <- stat.test %>%
      rstatix::add_significance() %>%
      add_xy_position(x = "Group_col", fun = "max", group= "Dodge_col", dodge = if (!is.null(dodge_width)) dodge_width else plot$layers[[2]]$position$width) %>%
      dplyr::mutate(
        yMax = if (!is.null(yPosition)) yPosition else y.position * pval_gap * 1.05,
        p_formatted = if (!is.null(format_pvalue_dif)) format_pvalue_dif(p) else format_pvalue(p)
      )
  } else if (test != "permutation" && is.null(Dodge)) { # no dodge!
    stat.test <- stat.test %>%
      rstatix::add_significance() %>%
      add_xy_position(fun = "max") %>%
      dplyr::mutate(
        yMax = if (!is.null(yPosition)) yPosition else y.position * pval_gap * 1.05,
        p_formatted = if (!is.null(format_pvalue_dif)) format_pvalue_dif(p) else format_pvalue(p)
      )
  }
  

  # adjust x-position if needed permutation if permutation tests used
  if (test == "permutation") {
    stat.test <- stat.test %>%
      mutate(
        xmin = 1,
        xmax = 2,
        yMax = map_dbl(data, ~ max(.x$yData_col, na.rm = TRUE))*1.1,
        p_formatted = if (!is.null(format_pvalue_dif)) format_pvalue_dif(p) else format_pvalue(p)
      ) %>%
      rename(data_nested = data) # rename to avoid stat_pvalue_manual conflict
  }

# If permutation test and no Dodge provided: map group1/group2 to global x positions
  # After the permutation test initial setup
  if (test == "permutation") {
    stat.test <- stat.test %>%
      mutate(
        xmin = 1,
        xmax = 2,
        yMax = map_dbl(data_nested, ~ max(.x$yData_col, na.rm = TRUE))*1.1,
        p_formatted = if (!is.null(format_pvalue_dif)) format_pvalue_dif(p) else format_pvalue(p)
      )
    
    # If no Dodge: map to global x positions
    if (is.null(Dodge)) {
      x_levels <- levels(factor(plot$data[[Group]]))
      stat.test <- stat.test %>%
        dplyr::mutate(
          xmin = match(group1, x_levels),
          xmax = match(group2, x_levels)
        )
    }
    
    # If Dodge provided and stat_group_by used (Group == Dodge case)
    if (!is.null(Dodge) && !is.null(stat_group_by)) {
      dodge_w <- if (!is.null(dodge_width)) dodge_width else plot$layers[[2]]$position$width
      
      stat.test <- stat.test %>%
        dplyr::mutate(
          dodge_numeric = as.numeric(as.factor(stat_group_by)),  # Use the actual column name
          xmin = dodge_numeric - dodge_w/2,
          xmax = dodge_numeric + dodge_w/2
        ) %>%
        dplyr::select(-dodge_numeric)
    }
  }
  
  # top align yMax?
  if (top_align && nrow(stat.test) > 0) {
    # Find global top value
    top_val <- max(stat.test$yMax, na.rm = TRUE)
    
    stat.test <- stat.test %>%
      dplyr::mutate(
        yMax = top_val,
        y_bracket = if (show_brackets) top_val - 0.02 * diff(range(plot$data[[yData]], na.rm = TRUE)) else y_bracket
      )
  }
  
  # option to adjust y_position if ymax are too close together
  if(adjust_y_position) {
    spacing <- 0.1 # gap between brackets
    
    stat.test <- stat.test %>%
      mutate(yMax = min(yMax) + (rank(yMax, ties.method = "first") - 1) * spacing) %>%
      ungroup()
  }
  
  
  # ---- Add effect size ---- #
  if (show_effectsize && test != "permutation") {
    
    # Decide whether to apply Hedges' correction
    if (effectsize_type == "d") {
      set_adjust <- FALSE 
    } else if (effectsize_type == "g") {
        set_adjust <- TRUE 
    } else {
          stop("effectsize_type must be 'd' (Cohen's d) or 'g' (Hedges' g).") 
      }
    
    effectsize_tbl <- df %>%
      dplyr::group_by(across(all_of(group_vars))) %>%
      dplyr::group_modify(~{
        if (paired) {
          # Paired design → repeated_measures_d
          out <- effectsize::repeated_measures_d(
            yData_col ~ Group_col | ID_Col,
            data = .x,
            ci = 0.95,
            method = method_sd,
            adjust = set_adjust
          )
        } else {
          # Unpaired design → cohens_d or hedges_g
          if (effectsize_type == "d") {
            out <- effectsize::cohens_d(
              yData_col ~ Group_col,
              data = .x,
              ci = 0.95,
              hedges.correction = FALSE
            )
          } else if (effectsize_type == "g") {
            out <- effectsize::hedges_g(
              yData_col ~ Group_col,
              data = .x,
              ci = 0.95
            )
          }
        }
        
        # # Normalize column names so join always works
        # out %>%
        #   dplyr::rename(
        #     effsize = dplyr::any_of(c("d_rm", "Cohens_d", "Hedges_g"))
        #   )
      }) %>%
      dplyr::ungroup()
    
    # Join back to stat.test 
    stat.test <- stat.test %>%
      dplyr::left_join(effectsize_tbl, by = group_vars)
    
  }


  #---- for facet_wrap: re-calculate y.position ----#
  if(!is.null(Facet) && test != "permutation") {
    if(!is.null(stat_group_by)) { # if dodge = Group: also group by stat_group_by
      # Step 1: Calculate max y per facet from the original data
      y_max_per_facet <- df %>%
        dplyr::group_by(Facet_col, stat_group_by) %>%
        dplyr::summarise(y_max = max(yData_col, na.rm = TRUE), .groups = "drop")
      # Step 2: Join to stat.test
      stat.test <- stat.test %>%
        dplyr::left_join(y_max_per_facet, by = c("Facet_col", "stat_group_by")) %>%
        mutate(
          yMax = if (!is.null(yPosition)) yPosition else y_max * pval_gap * 1.05 ) %>% # 1.05 adds little space between max value and bracket.
        select(-y_max)

    } else { # if dodge != group
      # Step 1: Calculate max y per facet from the original data
      y_max_per_facet <- df %>%
        dplyr::group_by(Facet_col) %>%
        dplyr::summarise(y_max = max(yData_col, na.rm = TRUE), .groups = "drop")

      # Step 2: Join to stat.test
      stat.test <- stat.test %>%
        dplyr::left_join(y_max_per_facet, by = "Facet_col") %>%
        mutate(
          yMax = if (!is.null(yPosition)) yPosition else y_max * pval_gap * 1.05 ) %>% # 1.05 adds little space between max value and bracket.
        select(-y_max)
    }
  }

  #---- if values between groups in facet are wastly different. need to adjust y-position per facet!
  if (!is.null(Facet)) {
    yMax_lookup <- stat.test %>%
      reframe(yMax_bigger = max(yMax), .by = "Facet_col")

    stat.test <- stat.test %>%
      left_join(yMax_lookup, by = "Facet_col") %>%
      mutate(yMax = yMax + 0.05*yMax_bigger)
  }


  #---- need to rename column, so stat_pvalue_manual knows it for faceting ----#
  if(!is.null(Facet)) {
  stat.test <- stat.test %>%
    dplyr::rename(!!Facet := Facet_col)
  }

  #---- # need to rename stat_group_by, so it is the same as in plot metadata ----#
  if(!is.null(stat_group_by)) {
    stat.test <- stat.test %>%
      dplyr::rename(!!stat_group_by := !!rlang::sym("stat_group_by"))
  }

  #---- add labels to plot ----#
  
  ### remove certain groups from stat.test
  if (!is.null(remove_comp_g1) && !is.null(remove_comp_g2)) {
    stat.test <- stat.test %>%
      dplyr::filter(!(group1 == remove_comp_g1 & group2 == remove_comp_g2))
  }

  
  # add brackets?
  
  if (show_brackets && !is.null(stat.test) && nrow(stat.test) > 0) {
    
    y_range <- diff(range(plot$data[[yData]], na.rm = TRUE))
    
    stat.test <- stat.test %>%
      dplyr::group_by(dplyr::across(dplyr::all_of(Facet))) %>%
      dplyr::mutate(
        comp_rank = rank(yMax, ties.method = "first"),
        # raise BOTH label and bracket progressively
        # yMax = yMax + (comp_rank - 1) * bracket_dist * y_range,
        # y_bracket = yMax - 0.02 * y_range
        y_bracket = yMax + (comp_rank - 1) * bracket_dist * y_range,
        yMax = y_bracket + 0.01 * y_range   # small offset for label above bracket
      ) %>%
      dplyr::ungroup()
    
    Plot_out <- plot +
      ggplot2::geom_segment(
        data = stat.test,
        aes(x = xmin, xend = xmax,
            y = y_bracket, yend = y_bracket),
        inherit.aes = FALSE
      ) +
      ggpubr::stat_pvalue_manual(
        data = stat.test,
        label = "p_formatted",
        y.position = "yMax",  # now raised
        xmin = "xmin",
        xmax = "xmax",
        vjust = -0.25,
        linetype = "blank",
        tip.length = 0,
        size = FontSize / 2.835,
        inherit.aes = FALSE    # <- IMPORTANT
      )
  }
  
  if (!show_brackets) {
    
    # Set linetype and tip.length first: add line if comp across group factor and not dodge
    linetype_val <- if (!is.null(Dodge) && is.null(stat_group_by)) "solid" else "blank"
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
  }


  # Determine y-axis expansion factor based on combination of Dodge and Facet
  y_expand_mult <- case_when(
    !is.null(Dodge) && !is.null(Facet) ~ 0.3,  # both dodge and facet
    !is.null(Dodge) &&  is.null(Facet) ~ 0.4,  # only dodge
    is.null(Dodge) && !is.null(Facet) ~ 0.25, # only facet
    is.null(Dodge) &&  is.null(Facet) ~ 0.2   # neither dodge nor facet
  )

  # Apply scale_y_continuous with dynamic expansion
  if (expand_y_0) {
    Plot_out <- Plot_out +
      scale_y_continuous(
        limits = c(lower_ylimit, NA),
        expand = expansion(mult = c(expand_lower_y_mult, y_expand_mult))
      )
  } else {
    Plot_out <- Plot_out +
      scale_y_continuous(
        limits = c(lower_ylimit, NA),
        expand = expansion(mult = c(expand_lower_y_mult, y_expand_mult))
      )
  }
  

  # Final plot
  Plot_out

  # Return result
  return(list(
    Plot = Plot_out,
    data_stat = stat.test
  ))

}
