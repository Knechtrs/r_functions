get_umap_plot_data <- function(seurat_obj) {
  df_umap <- Embeddings(seurat_obj, reduction = "umap") %>% 
    as.data.frame()

  df_umap$gel <- seurat_obj@meta.data$gel
  df_umap$timepoint <- seurat_obj@meta.data$timepoint

  return(df_umap)
}