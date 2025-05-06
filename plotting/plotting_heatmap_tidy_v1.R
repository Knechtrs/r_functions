Plotting_heatmap_tidy <- function(
    df,
    row_var,
    col_var,
    value_var,
    col_Order, # order of column
    BrewerColor = NULL, # default: "PuOr"
    ClusterRows = FALSE,
    ClusterColumns = FALSE,
    font_size_row = 6,
    font_size_col = 6,
    legend_title = NULL,
    color_breaks = 11,
    auto_palette = TRUE, # automatically switches between diverging and sequential data
    symmetric_color_scale = FALSE, # should color legend be symetric?
    annotation_vars = NULL,  # optional tile annotations
    annotation_palettes = NULL, # color palette for annotation
    row_order = NULL # option to manually define row order
) {
  # Load required packages if not already loaded
  required_packages <- c("dplyr", "rlang", "tidyHeatmap", "ComplexHeatmap", "circlize", "grid", "RColorBrewer", "tidyr")
  for(pkg in required_packages) {
    if(!requireNamespace(pkg, quietly = TRUE)) {
      stop(paste0("Package '", pkg, "' is needed for this function to work. Please install it."))
    }
  }
  
  # NULL coalescing operator definition
  `%||%` <- function(x, y) if (is.null(x)) y else x
  
  # Create strings from the NSE vars for safer evaluation
  row_var_str <- deparse(substitute(row_var))
  col_var_str <- deparse(substitute(col_var))
  value_var_str <- deparse(substitute(value_var))
  
  # Check if columns exist
  if (!value_var_str %in% names(df)) {
    stop(paste("Column", value_var_str, "not found in dataframe"))
  }
  if (!row_var_str %in% names(df)) {
    stop(paste("Column", row_var_str, "not found in dataframe"))
  }
  if (!col_var_str %in% names(df)) {
    stop(paste("Column", col_var_str, "not found in dataframe"))
  }
  
  # Get the numeric values for color scaling - use string names to be safer
  values_numeric <- df[[value_var_str]]
  value_min <- min(values_numeric, na.rm = TRUE)
  value_max <- max(values_numeric, na.rm = TRUE)
  
  # Optionally make scale symmetric around 0
  if (symmetric_color_scale) {
    max_abs <- max(abs(value_min), abs(value_max))
    value_min <- -max_abs
    value_max <- max_abs
  }
  
  # Set color palette based on data characteristics
  if (auto_palette) {
    if (value_min < 0 && value_max > 0) {
      BrewerColor <- BrewerColor %||% "PuOr"  # diverging
    } else {
      BrewerColor <- BrewerColor %||% "Blues" # sequential
    }
  } else {
    BrewerColor <- BrewerColor %||% "Blues"  # Default
  }
  
  # Create color ramp function
  my_color_ramp <- circlize::colorRamp2(
    breaks = seq(value_min, value_max, length.out = color_breaks),
    colors = grDevices::colorRampPalette(rev(RColorBrewer::brewer.pal(
      min(11, color_breaks), BrewerColor)))(color_breaks)
  )
  
  # If annotations are requested, prepare the data first
  base_df <- df
  if (!is.null(annotation_vars)) {
    # Create a unique ID for joining back later
    base_df <- base_df %>% 
      dplyr::mutate(._row_id = dplyr::row_number())
    
    # Extract metadata from ID column
    metadata_df <- base_df %>%
      tidyr::separate_wider_delim(
        col = !!rlang::sym(col_var_str),
        delim = "_", 
        names = col_Order,
        too_few = "align_start",  # Handle IDs with fewer segments
        too_many = "drop"         # Handle IDs with more segments
      )
    
    # Join metadata back to the original dataframe
    for (annot_var in annotation_vars) {
      if (annot_var %in% colnames(metadata_df)) {
        base_df[[annot_var]] <- metadata_df[[annot_var]]
      }
    }
  }
  
  # Create the main heatmap with explicit strings for more robust evaluation
  Plot <- tidyHeatmap::heatmap(base_df,
                               .row = !!rlang::sym(row_var_str),
                               .column = !!rlang::sym(col_var_str),
                               .value = !!rlang::sym(value_var_str),
                               column_title = NULL,
                               row_title = NULL,
                               scale = "none",
                               cluster_rows = ClusterRows,
                               cluster_columns = ClusterColumns,
                               row_dend_reorder = ClusterRows,
                               column_dend_reorder = ClusterColumns,
                               row_order = row_order,
                               col = my_color_ramp,
                               row_names_gp = grid::gpar(fontsize = font_size_row),
                               column_names_gp = grid::gpar(fontsize = font_size_col),
                               show_column_names = (font_size_col > 0),
                               heatmap_legend_param = list(
                                 title = legend_title %||% value_var_str,
                                 legend_gp = grid::gpar(fontsize = font_size_row),
                                 labels_gp = grid::gpar(fontsize = font_size_row),
                                 title_gp = grid::gpar(fontsize = font_size_row, fontface = "plain"),
                                 direction = "vertical",
                                 legend_width = grid::unit(3, "cm"),
                                 position = "bottom"
                               )
  )
  
  # Add annotations to the plot
  if (!is.null(annotation_vars)) {
    for (annot_var in annotation_vars) {
      if (annot_var %in% colnames(base_df)) {
        
        # Check for palette for this annotation
        this_palette <- NULL
        if (!is.null(annotation_palettes) && !is.null(annotation_palettes[[annot_var]])) {
          this_palette <- annotation_palettes[[annot_var]]
        }
        
        # Get the palette for this annotation
        this_palette <- annotation_palettes[[annot_var]]
        
        Plot <- tidyHeatmap::add_tile(
          Plot,
          !!rlang::sym(annot_var),
          palette = this_palette,
          size = grid::unit(font_size_row, "pt"),
          annotation_name_gp = grid::gpar(fontsize = font_size_row),
          annotation_legend_param = list(
            labels_gp = grid::gpar(fontsize = font_size_row),  # Adjust annotation legend label font size
            title_gp = grid::gpar(fontsize = font_size_row)
          )
        )
      }
    }
  }
  
  return(Plot)
}