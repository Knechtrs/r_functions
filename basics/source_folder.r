source_folder <- function(subfolder) {
  list.files(here(subfolder), pattern = "\\.R$", full.names = TRUE) %>%
    purrr::walk(source)
}