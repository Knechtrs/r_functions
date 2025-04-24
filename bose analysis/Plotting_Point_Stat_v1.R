Plotting_Point_Stat <- function(data, x, y, color_palette) {
  ggplot(data, aes(x = {{x}}, y = {{y}}, color = {{x}}, fill = {{x}})) +  
    geom_jitter(size = 3, width = 0.05, height = 0, shape=21, alpha=0.6) +  # Adding jitter to points
    # geom_point(size = 3) +
    stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.2, color = "grey50") +
    stat_summary(fun = mean, geom = "point", shape = 95, size = 5, color = "grey50") +
    scale_y_continuous(limits = c(0, NA), expand = expansion(mult = c(0, 0.05))) +
    scale_color_manual(values = color_palette) +
    scale_fill_manual(values = color_palette) +
    theme_Layout +
    theme_fontsize(FontSize)
}