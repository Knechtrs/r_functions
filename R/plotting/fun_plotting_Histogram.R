fun_plotting_Histogram <- function(
    data,
    xvar,
    yvar1,
    ColorVar,
    quantile_lines = TRUE,
    Color = NULL,
    LineVar = NULL,
    LineCode = NULL,
    title = NULL              
) {

  p <- ggplot(data, aes(
    x = {{xvar}},
    y = {{yvar1}},
    color = {{ColorVar}},
    linetype = {{LineVar}}
  )) +
    geom_density_ridges(
      aes(height = after_stat(ndensity)),
      fill = NA,
      quantile_lines = quantile_lines,
      quantile_fun = median,
      alpha = 0.3,
      scale = 0.8
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
