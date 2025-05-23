### create function to plot gmm_density plot to visualize cut-off of module score  ###

plot_gmm_density <- function(ref_expression, gmm_model) {
    
    # calculate the mean as comparison
    mean_threshold <- mean(ref_expression)
    median_threshold <- median(ref_expression)
  
  # Create data frame for plotting
  density_data <- data.frame(
    Expression = ref_expression,
    Cluster = factor(gmm_model$classification)
  )
  
  # Plot density with vertical lines for thresholds
  ggplot(density_data, aes(x = Expression, fill = Cluster)) +
    geom_density(alpha = 0.5) +
    geom_density(inherit.aes = FALSE,aes(x = Expression), alpha=0.5, fill="black", color="black") +
    geom_vline(xintercept = mean_threshold, color = "black", linetype = "dotted", size=1) +
    geom_vline(xintercept = median_threshold, color = "black", linetype = "dotted", size=1) +
    scale_y_continuous(limit=c(0,NA), expand = expansion(mult=c(0,0.1))) +
    # scale_x_continuous(limit=c(0,NA), expand = expansion(mult=c(0,0.1))) +
    annotate("text", x = mean_threshold, y = 1, label = "Mean", 
             angle = 90, vjust = 1.5, color = "black") +
    annotate("text", x = median_threshold, y = 1, label = "Median", 
         angle = 90, vjust = -1, color = "black") +
    labs(x = "Module score", y = "Density") +
    theme_Layout +
    theme_fontsize()
}