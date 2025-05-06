theme_fontsize <- function(base_size = 6) {
  
  # dynamically adapt margins
  margin_size <- max(5.5, base_size * 1.2)
  
  theme(
    text = element_text(size = base_size),
    axis.title = element_text(size = base_size),
    axis.text = element_text(size = base_size),
    plot.title = element_text(size = base_size),
    strip.text = element_text(size = base_size),
    legend.text = element_text(size = base_size),
    legend.title = element_text(size = base_size),
    plot.margin = margin(
      t = margin_size,
      r = margin_size,
      b = margin_size,
      l = margin_size
    )
  )
}