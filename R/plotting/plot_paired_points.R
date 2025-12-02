plot_paired_points <- function(
    data,
    xvar = NULL,
    yvar,
    yaxis_trans = NULL, # e.g. ln or log
    fillvar = NULL, # variable for fill color
    colorvar = NULL, # varibale for color. Shouldn't be NULL!
    connectVar1, # variable for connecting: within dodge
    connectVar2 = NULL, # variable 2 for connecting: ensures not across group
    use_dodge = FALSE, # set TRUE, if dodging
    Group = NULL, # variable for dodge
    dodge_width = 0.5,
    colors = "grey",
    fill_colors = colors,
    ylimits = c(0, NA),
    expand_lower_y_mult = 0,
    fontsize =FontSize,
    pointsize = PointSize,
    linewidth = LineWidth,
    facet = NULL, # varible for faceting or set to NULL 
    facet_scales = "free",
    nrow_facets = NULL,
    base_family = "Arial"
) {
  
  # browser()
  
  require(ggplot2)
  require(dplyr)
  require(rlang)
  
  # If xvar is NULL, use a constant "All"
  if (is.null(xvar)) {
    data <- data %>% mutate(x_dummy = "All")
    xvar <- "x_dummy"
  }
  
  # If colorvar or Group are NULL, use constant
  if (is.null(colorvar)) {
    data <- data %>% mutate(color_dummy = "all")
    colorvar <- "color_dummy"
  }
  
  if (is.null(Group)) {
    Group <- colorvar
  }
  
  # If fillvar is NULL, use constant
  if (is.null(fillvar)) {
    data <- data %>% mutate(fill_dummy = "all")
    fillvar <- "fill_dummy"
  }

  
  library(dplyr)
  library(rlang)
  
  # Positioning
  dodge <- if (use_dodge) position_dodge(width = dodge_width) else position_identity()
  
  if (use_dodge) {
    first_level <- levels(as.factor(pull(data, !!sym(Group))))[1]
    data <- data %>%
      mutate(
        x_dodge = as.numeric(as.factor(!!sym(xvar))),
        x_dodge = ifelse(
          !!sym(Group) == first_level,
          x_dodge - dodge_width / 2,
          x_dodge + dodge_width / 2
        )
      )
    x_aes <- sym("x_dodge")
  } else {
    x_aes <- sym(xvar)
  }
  
  # Plotting
    p <- ggplot(data, aes(x = !!sym(x_aes), y = !!sym(yvar), color = !!sym(colorvar), fill = !!sym(fillvar), group = !!sym(Group)))

  
  # Conditional line layer
  if (!is.null(connectVar2)) {
    p <- p + geom_line(aes(group = interaction(!!sym(connectVar1), !!sym(connectVar2))), color = "black")
  } else {
    p <- p + geom_line(aes(group = !!sym(connectVar1)), color = "black")
  }
  
  # Only apply color scale if colorvar was originally specified
  if (!is.null(fillvar) && fillvar != "fill_dummy") {
    p <- p + geom_point(shape = 21, size = pointsize, position= dodge)
  } else {
    p <- p + geom_point(shape = 21, size = pointsize, fill = "white", position= dodge)
  }
  
  # Remaining plot layers
  p <- p +
    scale_x_discrete(limits = c(levels(as.factor(pull(data, !!sym(xvar)))))) +
    scale_y_continuous(limits = ylimits, expand = expansion(mult = c(expand_lower_y_mult, 0.05))) +
    theme_layout +
    theme_fontsize(fontsize, base_family) +
    theme(
      legend.position = "none",
      axis.title.x = element_blank(),
      plot.title = element_text(hjust = 0.5)
    )
  
  # option to transform y-axis
  if (!is.null(yaxis_trans)) {
    p <- p + scale_y_continuous(trans = yaxis_trans)  
  }
  
  # Only apply color scale if colorvar was originally specified
  if (!is.null(colorvar) && colorvar != "color_dummy") {
    p <- p + scale_color_manual(values = colors)
  } else {
    # colorvar is dummy → everything = "all"
    p <- p + scale_color_manual(values = c(all = colors))
  }
  
  # Only apply fill scale if colorvar was originally specified
  if (!is.null(fillvar) && fillvar != "fill_dummy") {
    p <- p + scale_fill_manual(values = fill_colors)
  }
  
  # facet
  if (!is.null(facet)) {
      if(!is_null(nrow_facets)) {
            p <- p + facet_wrap(facets = facet, scales = facet_scales, nrow = nrow_facets)
            } else {
            p <- p + facet_wrap(facets = facet, scales = facet_scales)
            }
  }

  return(p)
}

