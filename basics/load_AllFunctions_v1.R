load_AllFunctions <- function (Folder) 
{
    files <- list.files(path = Folder, pattern = "\\.R$", full.names = TRUE)
    for (file in files) {
        source(file, local = .GlobalEnv)
        func_name <- sub("\\.R$", "", basename(file))
        cat("Loaded function:", func_name, "\n")
    }
}
