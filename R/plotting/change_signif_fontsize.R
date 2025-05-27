change_signif_fontsize <- function(plot, fontsize = 8) {
  # Initialize layer_index to NULL
  layer_index <- NULL
  
  # Find the layer containing p.signif
  for (i in seq_along(plot$layers)) {
    
    layer_data <- plot$layers[[i]]$data
    
    # Check if "p.signif" exists in the column names
    if ("p.signif" %in% colnames(layer_data)) {
      layer_index <- i
      break  # Exit the loop once found
    }
  }
  
  # Check if we found a layer with p.signif
  if (is.null(layer_index)) {
    message("No layer with p.signif found")
    return(plot)  # Return original plot unchanged
  }
  
  message("Found p-values in layer ", layer_index)
  
  # Modify the font size in the appropriate parameter
  plot$layers[[layer_index]]$aes_params$label.size <- fontsize/2.54
  
  # Return the modified plot
  return(plot)
}