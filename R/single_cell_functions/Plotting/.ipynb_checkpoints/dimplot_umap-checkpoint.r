dimplot_umap <- function(
    df,
    colorvar = "gel",
    colors = Color.Gels,
    pointsize = 0.1,
    fontsize = 8
    ){
    
    # randomize points
    df <- df[sample(nrow(df)), ]

    # create plot
    p <- ggplot(df, aes(x = umap_1, y = umap_2, color = !!sym(colorvar))) +
        geom_point(size = pointsize, shape = 16) +
        scale_color_manual(values = colors) +
        theme_fontsize(fontsize) +
        theme_layout +
        theme_UMAP +
        guides(color = guide_legend(override.aes = list(size = 4))) +
        theme(
            legend.position = "bottom",
            legend.title = element_blank(),
            legend.key.spacing.x = unit(20, "pt"),
            legend.key.spacing.y = unit(10, "pt"),
            legend.justification = "center",
            legend.box.spacing = unit(10, "pt")
        )
    return(p)
}