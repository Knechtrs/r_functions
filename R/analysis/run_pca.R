run_pca <- function(
    df,
    scale_cols = NULL,  # Can be NULL or character vector of column names to group by
    center = TRUE, # center function in prcomps: sets mean = 0
    scale = FALSE, # scale function in prcomps: sets sd = 1
    color_cols = NULL, # metadata columns to be used for both coloring and annotation
    fontsize = 8,
    pointsize = 3,
    color_values = NULL, # colors for color_cols values,
    add_loadings = FALSE # should loadings be shonw in plot?
    ) {
  
  # browser()
  
  # Optional grouping
  if (!is.null(scale_cols)) {
    
    df <- df %>% 
      group_by(across(all_of(scale_cols))) %>%
      mutate(across(where(is.numeric), ~ scale(.x)[,1])) %>%
      ungroup()
  }
  
  # Select numeric columns for PCA
  cytokine_data <- df %>% select(where(is.numeric))
  
  # NA check
  if( sum(is.na(cytokine_data)) != 0) {
    stop( message("Number of NAs in numeric data: ", sum(is.na(cytokine_data))))
  }

  #---- run pca ----#
  
  # check input variables:
  if(!is.null(scale_cols) && center == TRUE) {
    message("data is already scaled. Are you sure you want center scaled data?")
  } else if (!is.null(scale_cols) && scale == TRUE) {
    message("scaling has already been performed. Rescaling in prcomp call will overwrite previous scaling by groups")
  }
  
  # Run PCA with scaling (recommended for cytokine data)
  pca_result <- prcomp(cytokine_data, center = center, scale. = scale)
  
  summary_output <- summary(pca_result)
  
  # Proportion of variance explained by each component
  var_explained <- pca_result$sdev^2 / sum(pca_result$sdev^2)
  
  #---- create ggplots ----#
  
  # Create a dataframe with PCA scores and original grouping variables
  pca_df <- data.frame(
    PC1 = pca_result$x[,1],
    PC2 = pca_result$x[,2])
  
  if (!is.null(color_cols)) {
    # Safely bind requested metadata columns (if any)
    pca_df <- bind_cols(
      pca_df,
      df %>% select(all_of(color_cols))
    )
  }
  
  # check color_cols input
  if (is.null(color_cols)) {
    stop("Please select at least one column name for colors")
  }
  
  if(length(color_cols) > 1) {
    pca_df <- pca_df %>%
      unite("color_col", all_of(color_cols), sep = "_", remove = FALSE)
  } else {
    pca_df <- pca_df %>% dplyr::rename("color_col" = color_cols)
  }

  # Plot colored by different factors
  plot <- ggplot(pca_df, aes(x = PC1, y = PC2, color = color_col)) +
    geom_point(size = pointsize) +
    labs(x = paste0("PC1 (", round(var_explained[1]*100, 1), "%)"),
         y = paste0("PC2 (", round(var_explained[2]*100, 1), "%)")
         ) +
    theme_layout +
    theme_fontsize(fontsize) +
    theme(legend.title = element_blank())
  
  if(!is.null(color_values)) {
    plot <- plot +
      scale_color_manual(values = color_values) +
      theme(legend.title= element_blank())
    }
  
  
  # optional: add loadings
  if (add_loadings) {
    # Get loadings
    loadings_df <- as.data.frame(pca_result$rotation[, 1:2])
    loadings_df$varname <- rownames(loadings_df)
    
    # Rescale arrows
    scale_factor <- max(abs(pca_df$PC1), abs(pca_df$PC2)) * 2
    loadings_df$PC1 <- loadings_df$PC1 * scale_factor
    loadings_df$PC2 <- loadings_df$PC2 * scale_factor
    
    # Add arrows and labels
    plot <- plot +
      geom_segment(
        data = loadings_df,
        aes(x = 0, y = 0, xend = PC1, yend = PC2),
        arrow = arrow(length = unit(0.2, "cm")),
        color = "grey60"
      ) +
      geom_text_repel(
        data = loadings_df,
        aes(x = PC1, y = PC2, label = varname),
        size = fontsize/2.54,
        color = "grey60",
        force = 5
      )
  }
  
  return(list(
    summary_output = summary_output,
    pca_result = pca_result,
    plot = plot
  ))
  
}