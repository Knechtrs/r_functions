emod_analysis <- function(
    df_data = df_data,
    id = "exp",
    load = "load",
    time = "time",
    disp = "disp",
    groupvar = "Alginate",
    gel_height = params$GelProperties$Height,
    gel_r = params$GelProperties$Radius,
    emod_low = params$Emod_low,
    emod_high = params$Emod_high,
    FontSize = params$theme$FontSize
) {

  #---- calculate stress and strain ----#
  data_emod <- df_data %>%
    group_by(!!sym(id)) %>%
    mutate(max_time = .data[[time]][which.max(.data[[load]])]) %>%
    filter(!!sym(time) < max_time) %>%
    mutate(
      !!sym(disp) := max(!!sym(disp)) - !!sym(disp),
      strain = !!sym(disp) / gel_height,
      stress = !!sym(load) * 9.81 / (pi * gel_r^2)
    ) %>%
    ungroup()
  
  #---- subset data for strain range ----#
  data_fit_range <- data_emod %>%
    filter(strain >= emod_low & strain <= emod_high)
  
  #---- plot E-modulus range ----#
  plot_EmodFit <- ggplot() +
    geom_line(data = data_emod , aes(x = !!sym(disp), y = !!sym(load))) +
    geom_smooth(data = data_fit_range, aes(x = !!sym(disp), y = !!sym(load)),
                color = "red", linewidth = 1.2, method = "lm", se = FALSE) +
    facet_wrap(reformulate(id), scales = "free_y") +
    theme_layout +
    theme_fontsize(FontSize) +
    labs(
      x = "Displacement (mm)",
      y = "Load (g)"
    ) +
    ggtitle(
      paste0("E-modulus fit between ", emod_low * 100, "% - ", emod_high * 100, "% strain")
    )
  
  #---- calculate modulus ----#
  df_modulus <- data_fit_range %>%
    group_by(!!sym(id), !!sym(groupvar)) %>%
    nest() %>%
    mutate(
      model = map(data, ~ lm(stress ~ strain, data = .x)),
      Emod = map_dbl(model, ~ coef(.x)[["strain"]])
    ) %>%
    select(-data, -model) %>%
    ungroup()
  
  return(list(
    data = df_modulus,
    plot = plot_EmodFit
    ))
}
