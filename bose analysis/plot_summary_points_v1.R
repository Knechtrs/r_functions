plot_summary_points <- function(
    data,
    xvar,
    yvar,
    fillvar,
    Group = fillvar,
    use_dodge = FALSE,
    dodge_width = 0.5,
    jitter_width = 0.1,
    colors = "grey",
    ylimits = c(0, NA),
    fontsize = 7,
    facet = NULL,
    facet_scales = "fixed"
) {
  require(ggplot2)
  require(dplyr)
  require(rlang)
  
  dodge <- if (use_dodge) position_dodge(width = dodge_width) else position_identity()
  jitter_dodge <- if (use_dodge) position_jitterdodge(jitter.width = jitter_width, dodge.width = dodge_width) else position_jitter(width = jitter_width)
  
  p <- ggplot(data, aes(x = !!sym(xvar), y = !!sym(yvar), fill = !!sym(fillvar), group = !!sym(Group))) +
    geom_jitter(
      size = 3, shape = 21, alpha = 0.8, color = "black",
      position = jitter_dodge
    ) +
    stat_summary(
      fun.data = mean_se,
      geom = "errorbar",
      linewidth = 1,
      width = 0.2,
      color = "black",
      position = dodge
    ) +
    stat_summary(
      fun = mean,
      geom = "crossbar",
      width = 0.4,
      fatten = 2,
      color = "black",
      position = dodge
    ) +
    scale_y_continuous(limits = ylimits, expand = expansion(mult = c(0, 0.05))) +
    scale_fill_manual(values = colors) +
    theme_Layout +
    theme_fontsize(fontsize) +
    theme(
      legend.position = "none",
      axis.title.x = element_blank(),
      plot.title = element_text(hjust = 0.5)
    )
  
  if (!is.null(facet)) {
    p <- p + facet_wrap(facets = facet, scales = facet_scales)
  }
  
  return(p)
}
