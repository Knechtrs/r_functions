save_figure <- function(
    plot,
    filename = NULL,
    width_cm = NA,
    height_cm = NA,
    folder = "Output/Plots",
    file_type = c(".rds", ".png", ".pdf", ".svg"),
    dpi = 600,
    scale = 1
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
      ".pdf" = if (capabilities("cairo")) cairo_pdf else "pdf",
      ".svg" = svglite::svglite
    )
    
    # Conditional ggsave based on SVG vs other formats
    if (file_type != ".svg") {
      ggsave(
        filename = filepath,
        plot     = plot,
        device   = device_fun,
        width    = width_in,
        height   = height_in,
        dpi      = dpi,
        scale = scale
      )
    } else {

      svglite(
        filename      = filepath,
        width         = width_in,
        height        = height_in,
        fix_text_size = FALSE
      )
      
      plot(plot)
      invisible(dev.off())
    }
  }
  
  invisible(filepath)
}