#---- perform ROC anylsis for d1 and d2 on TopFeatures_d1 ----#

library(ggsci)

# create function
ROC_anylsis <- function(seurat_obj, reference_gene, GeneFeatures) {

    # perform roc analysis by donor
    seurat_obj_List <- SplitObject(seurat_obj, split.by = "donor")
        
    # apply perform_roc_analysis function
    Roc_Results <- purrr::map(seurat_obj_List, function(seurat_obj) {
      perform_roc_analysis(
        seurat_obj, 
        gene_set = GeneFeatures,
        reference_gene = reference_gene
      )
    })
   
    library(tidyverse)
    library(pROC)
    library(RColorBrewer)
    library(gridExtra)
    library(grid)
    
    # Extract data and create ROC dataframe in one step
    roc_df <- Roc_Results %>%
      imap(~data.frame( # use imap for named vector/list inputs
        TPR = .x$roc_object$sensitivities,
        FPR = 1 - .x$roc_object$specificities,
        Donor = .y
      )) %>% 
      bind_rows()

    # Define the donor color mapping explicitly
    donor_colors <- c(
      "A" = "#4DBBD5FF",  # light blue
      "B" = "#00A087FF",  # teal/green
      "C" = "#3C5488FF"   # dark blue
    )
    
    # reorder Donor: 
    # Define the preferred donor order
    donor_order <- c("A", "B", "C")
    
    # Create AUC table with ordered donors
    auc_table <- tibble(
      Donor = names(Roc_Results),
      AUC = round(map_dbl(Roc_Results, ~.x$auc_value), 3)
    ) %>%
      # Convert Donor to factor with specific order
      mutate(Donor = factor(Donor, levels = donor_order)) %>%
      # Sort by the factor levels
      arrange(Donor)
    
    # Create the tableGrob with matched colors
    table_plot <- tableGrob(
      auc_table,
      rows = NULL,
      theme = ttheme_minimal(
        base_size = 10,
        core = list(
          fg_params = list(
            fontface = c(rep("bold", nrow(auc_table)), rep("plain", nrow(auc_table))),
            col = c(donor_colors, rep("black", each = length(names(Roc_Results))))  # Color first column, black second
          ),
          bg_params = list(fill = "transparent", col = NA)
        ),
        padding = unit(c(1, 1), "mm")
      )
    )

    return(
        list(
            roc_df = roc_df,
            table_plot = table_plot,
            auc_table = auc_table
            )
          )
}