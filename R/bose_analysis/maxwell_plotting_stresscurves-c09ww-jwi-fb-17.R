maxwell_plotting_stresscurves <- function(
    result1 = list_maxwell_results_one, # fitted data results one element
    result2 = list_maxwell_results_two, # fitted data results two element
    which_models = c("one", "two", "both"), # choose which fit data to plot
    df = data_stressrelax, # original data
    time = "Time", # column of time data
    fit_var = "Load_norm", # column name of fit data
    color_var = NULL, # column name for color variable. if NULL set label color_label!
    color_vec = Color.Gels,
    t_max = NULL, # max time value that get's plotted
    slice_it = FALSE, # should dataframe be log sliced? reduces file size of ggplot
    slice_points = 100, # number of points per sample
    id = "alg_batch",
    facet_scale = "free",
    n_row = NULL, # define number of rows
    linewidth = 1 # line width of fitted data
    # linetype = 11 # one on one off
    # show_labels = TRUE
) {
  
  # browser()
  
  # optional: filter for time > t_max
  if(!is.null(t_max)) {
    result1$fitted_data <- result1$fitted_data %>% 
      filter(!!sym(time) <= .env$t_max)
    
    result2$fitted_data <- result2$fitted_data %>% 
      filter(!!sym(time) <= .env$t_max)
    
    df <- df %>% 
      filter(!!sym(time) <= .env$t_max)
  }

  # initialize empty lists
  fitted_data_all <- list() 
  param_labels_all <- list()
  
  if (which_models %in% c("one", "both") && !is.null(result1)) {
    data1 <- result1$fitted_data %>%
      group_by(!!sym(id)) %>%
      ungroup() %>%
      mutate(Model = recode(list_maxwell_results_one$model_type,
                            "one" = "1-element", "two" = "2-element"))
    fitted_data_all <- append(fitted_data_all, list(data1))
  }
  
  if (which_models %in% c("two", "both") && !is.null(result2)) {
    data2 <- result2$fitted_data %>%
      group_by(!!sym(id)) %>%
      ungroup() %>%
      mutate(Model = recode(list_maxwell_results_two$model_type,
                            "one" = "1-element", "two" = "2-element"))
    fitted_data_all <- append(fitted_data_all, list(data2))
  }
  
  # combine dataframes (multiple donors)
  data_fitted_combined <- bind_rows(fitted_data_all)
  
  # check if color_var exists in df. If not, add as column:
  if (!(color_var %in% names(df))) {
    df <- df %>%
      mutate(!!sym(color_var) := color_var)
  }

  # create plot
  plot_fitted <- ggplot() +
    geom_path(data = df,
              aes(x = !!sym(time), y = !!sym(fit_var), color = !!sym(color_var)),
              linewidth = linewidth*2) +
    geom_path(data = data_fitted_combined %>% arrange(!!sym(time)), # order of time data is important for geom_path!
              aes(x = !!sym(time), y = !!sym(fit_var), linetype = Model),
              color = "black",
              alpha = 0.7,
              linewidth = linewidth) +
              # linetype =  linetype) + 
    facet_wrap(reformulate(id), if(!is.null(n_row)){nrow = n_row}, scales = facet_scale) +
    scale_color_manual(
      name = "",
      values = unlist(color_vec)
    ) +
  scale_y_continuous(limits = c(0, NA), expand = expansion(mult = c(0.05, 0))) +
  scale_linetype_manual(values = c("1-element" = 11, "2-element" = "solid")) +
    labs(x = "Time (s)", y = "Normalized stress") +
    theme_layout +
    theme_fontsize(FontSize) 
  
  return(plot_fitted)
}
