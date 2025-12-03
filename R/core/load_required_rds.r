# Automatically load required RDS files for a figure
load_required_rds <- function(path, plot_vec) {
  for (p in plot_vec) {
    file = file.path(path, paste0(p, ".rds"))
    if (file.exists(file)) {
      assign(p, readRDS(file), envir = .GlobalEnv)
      message("Loaded: ", p)
    }
  }
}
