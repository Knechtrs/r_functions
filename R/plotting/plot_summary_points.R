plot_summary_points <- function(
    data, # data
    xvar = NULL, 
    yvar,
    fillvar = NULL, # fill variable for color
    Group = NULL, # group for doging
    use_dodge = FALSE,
    dodge_width = 0.5,
    jitter_width = 0.1,
    colors = "grey", # color vector for groups
    ylimits = c(0, NA),
    fontsize =FontSize,
    pointsize = PointSize,
    linewidth = LineWidth,
    facet = NULL,
    facet_scales = "fixed"
) {
  require(ggplot2)
  require(dplyr)
  require(rlang)
  
  # If xvar is NULL, use a constant "All"
  if (is.null(xvar)) {
    data <- data %>% mutate(x_dummy = "All")
    xvar <- "x_dummy"
  }
  
  # If fillvar or Group are NULL, use constant
  if (is.null(fillvar)) {
    data <- data %>% mutate(fill_dummy = "all")
    fillvar <- "fill_dummy"
  }
  if (is.null(Group)) {
    Group <- fillvar
  }
  
  # Positioning
  dodge <- if (use_dodge) position_dodge(width = dodge_width) else position_identity()
  jitter_dodge <- if (use_dodge) {
    position_jitterdodge(jitter.width = jitter_width, dodge.width = dodge_width)
  } else {
    position_jitter(width = jitter_width)
  }
  
  # Base plot
  p <- ggplot(data, aes(x = !!sym(xvar), y = !!sym(yvar), fill = !!sym(fillvar), group = !!sym(Group))) +
    geom_jitter(
      size = pointsize, shape = 21, alpha = 0.8, color = "black",
      position = jitter_dodge
    ) +
    stat_summary(
      fun.data = mean_se,
      geom = "errorbar",
      linewidth = linewidth,
      width = 0.2,
      color = "black",
      position = dodge
    ) +
    stat_summary(
      fun = mean,
      geom = "crossbar",
      linewidth = linewidth,
      width = 0.4,
      fatten = 2,
      color = "black",
      position = dodge
    ) +
    scale_y_continuous(limits = ylimits, expand = expansion(mult = c(0, 0.05))) +
    theme_layout +
    theme_fontsize(fontsize) +
    theme(
      legend.position = "none",
      axis.title.x = element_blank(),
      plot.title = element_text(hjust = 0.5)
    )
  
  # Only apply fill scale if fillvar was originally specified
  if (!is.null(fillvar) && fillvar != "fill_dummy") {
    p <- p + scale_fill_manual(values = colors)
  }
  
  if (!is.null(facet)) {
    p <- p + facet_wrap(facets = facet, scales = facet_scales)
  }
  
  return(p)
}
