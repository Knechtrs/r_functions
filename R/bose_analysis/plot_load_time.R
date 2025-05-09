plot_load_time <- function(
    data=df_data,
    x = "Time",
    y1= "Load",
    add_bf = FALSE,
    y2 = "load_BF"
    ) {
  p <- data %>%
    ggplot() +
    geom_path(aes(x=!!sym(x), y=!!sym(y1)), linewidth=1, alpha=0.7, color="black") + # use geom_path here, b.c connects points in the order they appear in the dataset. Geom_line by x-axis order
    facet_wrap(~exp, scale="free") +
    theme_layout +
    theme_fontsize(FontSize) +
    theme(strip.background = element_blank())
  
  if(add_bf) {
   p <- p +
     geom_path(aes(x=!!sym(x), y=!!sym(y2)), linewidth=1, alpha=0.7, color="red")
  }
  
  return(p)
}