maxwell_plotting_stresscurves <- function(
    result1 = list_maxwell_results_one, # fitted data results one element
    result2 = list_maxwell_results_two, # fitted data results two element
    which_models = c("one", "two", "both"), # choose which fit data to plot
    df = data_StressRelax, # original data
    time = "Time", # column of time data
    fit_var = "Load_norm", # column name of fit data
    color_var = NULL, # column name for color variable. if NULL set label color_label!
    color_label = "Hematoma", # legend label color 
    color_vec =   colors_fit, 
    # fit_label1 = "one element", # legend label
    # fit_label2 = "two element", # legend label
    t_max = 3000,
    id = "exp",
    show_labels = TRUE
) {
  
  fitted_data_all <- list()
  param_labels_all <- list()
  
  
  if (which_models %in% c("one", "both") && !is.null(result1)) {
    data1 <- result1$fitted_data %>%
      group_by(!!sym(id)) %>%
      slice(seq(1, n(), by = floor(n()/100))) %>% # reduce df size
      ungroup() %>%
      mutate(Model = list_maxwell_results_one$model_type)
    fitted_data_all <- append(fitted_data_all, list(data1))
    
    if (show_labels) {
      labels1 <- result1$parameters %>%
        mutate(label = sprintf("\u03C4 = %.1f, A = %.2f", tau, A)) %>%
        select(all_of(id), label)
      
      label_positions1 <- data1 %>%
        group_by(!!sym(id)) %>%
        summarise(x = max(!!sym(time)) * 0.1,
                  y = max(!!sym(fit_var)) - 0.1) %>%
        left_join(labels1, by = id)
      
      label_positions1$Model <- list_maxwell_results_one$model_type
      param_labels_all <- append(param_labels_all, list(label_positions1))
    }
  }
  
  if (which_models %in% c("two", "both") && !is.null(result2)) {
    data2 <- result2$fitted_data %>%
      group_by(!!sym(id)) %>%
      slice(seq(1, n(), by = floor(n()/100))) %>%
      ungroup() %>%
      mutate(Model = list_maxwell_results_two$model_type)
    fitted_data_all <- append(fitted_data_all, list(data2))
    
    if (show_labels) {
      labels2 <- result2$parameters %>%
        mutate(label = sprintf("\u03C41 = %.1f, \u03C42 = %.1f\nA1 = %.2f, A2 = %.2f", tau1, tau2, A1, A2)) %>%
        select(all_of(id), label)
      
      label_positions2 <- data2 %>%
        group_by(!!sym(id)) %>%
        summarise(x = max(!!sym(time)) * 0.1,
                  y = max(!!sym(fit_var)) - 0.3) %>%
        left_join(labels2, by = id)
      
      label_positions2$Model <- list_maxwell_results_two$model_type
      param_labels_all <- append(param_labels_all, list(label_positions2))
    }
  }
  
  data_fitted_combined <- bind_rows(fitted_data_all)
  label_positions <- if (show_labels) bind_rows(param_labels_all) else NULL
  
  # Determine color aesthetic mapping: option to define just label or column in df
  if (!is.null(color_var)) {
    aes_color <- !!sym(color_var)  # Use dynamic column for coloring
  } else {
    aes_color <- color_label  # Fixed label for legend
    df[[color_label]] <- color_label       # Add dummy column for consistent behavior
  }
  
  # create plot
  plot_fitted <- ggplot() +
    geom_path(data = df,
              aes(x = !!sym(time), y = !!sym(fit_var), color = aes_color),
              linewidth = 1.5) +
    geom_path(data = data_fitted_combined,
              aes(x = !!sym(time), y = !!sym(fit_var), color = Model),
              alpha = 1,
              linewidth = 1,
              linetype = "11") +
    facet_wrap(reformulate(id), nrow = 2) +
    scale_color_manual(
      name = "",  # optional legend title
      values = colors_fit
    ) +
    # scale_color_manual(
    #   name = "",
    #   values = c(
    #     setNames(data_color, color_label),
    #     setNames(fit_color1, fit_label1),
    #     setNames(fit_color2, fit_label2)
    #   )
    # ) +
    scale_x_continuous(limits = c(0, t_max), expand = expansion(mult = c(0.05, 0))) +
    labs(x = "Time (s)", y = "Normalized stress") +
    theme_layout +
    theme_fontsize(FontSize)
  
  if (show_labels && !is.null(label_positions)) {
    plot_fitted <- plot_fitted +
      geom_text(data = label_positions,
                aes(x = x, y = y, label = label),
                hjust = 0, vjust = 0, size = 3)
  }
  
  return(plot_fitted)
}
