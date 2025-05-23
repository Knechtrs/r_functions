library(pROC)
library(mclust)
library(Seurat)

## load functions that are needed for Roc analysis
# load plot_ggm_density function to visualize cut-off of module score for environments
source('/data/cephfs-1/work/groups/duda/users/knechtrs_c/Projects/Functions/seurat/plot_gmm_density.r')

# load Plotting_Percentage_Gel_Marker function
source('/data/cephfs-1/work/groups/duda/users/knechtrs_c/Projects/Functions/Plotting/Plotting_Percentage_Gel_Marker.r')

# load Plotting_Percentage_Gel function
source('/data/cephfs-1/work/groups/duda/users/knechtrs_c/Projects/Functions/Plotting/Plotting_Percentage_Gel.r')


# create ROC analysis function
perform_roc_analysis <- function(seurat_obj, gene_set, reference_gene = "CD14") {
  # Remove reference gene from gene set
  gene_set_filtered <- setdiff(gene_set, reference_gene)
  
  # Filter cells based on expression threshold
  cells_to_keep <- seurat_obj@assays$RNA$data[reference_gene, ] > 0
  
  # Create a new filtered Seurat object
  seurat_obj_filtered <- subset(seurat_obj, cells = colnames(seurat_obj)[cells_to_keep])
  
  # Compute module score on filtered object
  seurat_obj_filtered <- AddModuleScore(
    seurat_obj_filtered, 
    features = list(gene_set_filtered), 
    name = "ModuleScore"
  )
    
  # Extract module score and reference gene expression
  module_score <- seurat_obj_filtered@meta.data$ModuleScore1
  ref_expression <- seurat_obj_filtered@assays$RNA$data[reference_gene, ]
  
  # Unsupervised clustering to define binary groups: Module score high and low cells
  gmm_model_ModuleScore <- Mclust(module_score, G = 2)
  binary_groups_ModuleScore <- gmm_model_ModuleScore$classification - 1

  seurat_obj_filtered@meta.data$binary_groups_Fast <- as.factor(binary_groups_ModuleScore)
  
  # plot density plot of GMM model: To check how well binary group identification worked
  Density_plot <- plot_gmm_density(module_score, gmm_model_ModuleScore)

 # plot Gel percentage for binary groups (high and low expr of specific marker): Checks well gene set predicts mechanical environment 
   Plot_perc_Gel <- Plotting_Percentage_Gel(seurat_obj_filtered)

    ## Optional: plot Barplot for CD14 high and low and check Fast vs Slow percentage
    # Unsupervised clustering to define binary groups
      gmm_model_Marker <- Mclust(ref_expression, G = 2)
      binary_groups_Marker <- gmm_model_Marker$classification - 1
    # add binary data to meta.data
      seurat_obj_filtered@meta.data$binary_groups_Marker <- as.factor(binary_groups_Marker)
    # Create plot
      Plot_perc_Gel_Marker <- Plotting_Percentage_Gel_Marker(seurat_obj_filtered)
      
  # Perform ROC analysis
  roc_obj <- roc(binary_groups_ModuleScore, ref_expression)
  
  # Return comprehensive results
  return(list(
    Density_plot = Density_plot,
    Plot_perc_Gel = Plot_perc_Gel,
    Plot_perc_Gel_Marker = Plot_perc_Gel_Marker,
    roc_object = roc_obj,
    auc_value = auc(roc_obj)
  ))
}