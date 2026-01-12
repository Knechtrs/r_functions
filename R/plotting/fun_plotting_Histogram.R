fun_plotting_Histogram <- function(
    data,
    xvar,
    yvar1,
    ColorVar,
    quantile_lines = TRUE,
    Color = NULL,
    LineVar = NULL,
    LineThickness = 1,
    LineCode = NULL,
    title = NULL,
    reverse_y = FALSE
) {
  
  if (reverse_y) {
    data <- data %>%
      mutate({{yvar1}} := forcats::fct_rev(as_factor({{yvar1}})))
  }

  p <- ggplot(data, aes(
    x = {{xvar}},
    y = {{yvar1}},
    color = {{ColorVar}},
    linetype = {{LineVar}}
  )) +
    ggridges::geom_density_ridges(
      aes(height = after_stat(ndensity)),
      fill = NA,
      quantile_lines = quantile_lines,
      quantile_fun = median,
      alpha = 1,
      scale = 0.8,
      linewidth = LineThickness,
      key_glyph = "path" 
    ) +
    scale_x_continuous(limits = c(NA, NA), expand = c(0, 0)) +
    labs(y = NULL, title = title)
  
  if (!is.null(Color)) {
    p <- p + scale_color_manual(values = unlist(Color))
  } else {
    p <- p + scale_color_brewer(palette = "Set2")
  }
  
  if (!is.null(LineCode)) {
    p <- p + scale_linetype_manual(values = unlist(LineCode))
  }

  return(p)
}
