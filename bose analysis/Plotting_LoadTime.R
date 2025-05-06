Plotting_LoadTime <- function(data=df_data, x = Time, y1= Load, y2 = Load_BF) {
  data %>%
    ggplot() +
    geom_path(aes(x=Time, y=Load), linewidth=1, alpha=0.7, color="black") + # use geom_path here, b.c connects points in the order they appear in the dataset. Geom_line by x-axis order
    geom_path(aes(x=Time, y=Load_BF), linewidth=1, alpha=0.7, color="red") +
    facet_wrap(~exp, scale="free_y") +
    theme_Layout +
    theme_fontsize(FontSize) +
    theme(strip.background = element_blank())
}