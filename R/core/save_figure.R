save_figure <- function(
    plot,
    filename = NULL,
    width_cm = NA,
    height_cm = NA,
    folder = "Output/Plots/Version_1/Individual",
    file_type = c(".rds", ".png", ".pdf", ".svg"),
    dpi = 600
) {
  # Ensure valid file type
  file_type <- match.arg(file_type)
  
  # Create directory if it doesn't exist
  if (!dir.exists(folder)) {
    dir.create(folder, recursive = TRUE)
  }
  
  # Derive filename from plot expression if not supplied
  if (is.null(filename)) {
    filename <- deparse(substitute(plot))
  }
  
  # Construct full file path
  filepath <- file.path(folder, paste0(filename, file_type))
  
  # Save according to requested type
  if (file_type == ".rds") {
    saveRDS(plot, file = filepath)
  } else {
    # Convert dimensions from cm to inches if provided
    width_in <- if (!is.na(width_cm)) width_cm / 2.54 else NULL
    height_in <- if (!is.na(height_cm)) height_cm / 2.54 else NULL
    
    # Handle SVG dependency
    if (file_type == ".svg" && !requireNamespace("svglite", quietly = TRUE)) {
      stop("Package 'svglite' required for saving SVG files. Please install it.")
    }
    
    # Determine device function or format
    device_fun <- switch(
      file_type,
      ".png" = "png",
      ".pdf" = cairo_pdf,
      ".svg" = svglite::svglite
    )
    
    # Save with ggsave
    ggsave(
      filename = filepath,
      plot = plot,
      device = device_fun,
      width = width_in,
      height = height_in,
      dpi = dpi
    )
  }
  
  invisible(filepath)
}