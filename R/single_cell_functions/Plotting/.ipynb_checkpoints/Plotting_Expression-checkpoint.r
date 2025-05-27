### Plot gene expression by timepoint and gel: ###
## Plot significant genes over time (MVG vs VLVG) and check for consistency and effect size  ##

library(ggplot2)
library(dplyr)
library(patchwork)
library(purrr)
library(ggpubr)
library(rstatix)
library(plotrix)

# load custom functions that are needed
source('/data/cephfs-1/work/groups/duda/users/knechtrs_c/Projects/Functions/basics/format_pvalue.r')

# Define the plotting function without individual axis labels
Plotting_Expression <- function(df, Marker, FontSize=10) {
    gene_matrix <- tryCatch(
        GetAssayData(df, assay = "RNA", slot = "data"),
        error = function(e) {
            message("Could not access RNA data slot: ", e$message)
            return(NULL)
        }
    )

    if (is.null(gene_matrix) || !(Marker %in% rownames(gene_matrix))) {
        message(glue::glue("Marker '{Marker}' not found in RNA assay. Skipping."))
        return(NULL)
    }

    # Add Marker expression to metadata
    df@meta.data$MarkerExp <- df@assays$RNA$data[Marker, ]

    # Calculate mean expression per donor/timepoint/gel
    mean_expression <- df@meta.data %>%
        # filter(!timepoint %in% "d0") %>%
        group_by(timepoint, donor, gel) %>%
        summarize(MarkerExp = mean(MarkerExp, na.rm = TRUE), .groups = "drop") %>%
        arrange(timepoint, gel, donor)

    # Paired t-tests per timepoint between gels
    stat.test <- mean_expression %>%
        filter(!timepoint %in% "d0") %>%
        arrange(donor, timepoint) %>%
        group_by(timepoint) %>%
        pairwise_t_test(MarkerExp ~ gel, paired = TRUE) %>% 
        add_significance() %>%
        add_xy_position(x = "timepoint", fun = "max", step.increase = 0.17) %>%
        mutate(
            yMax = y.position * 1.1,
            p_formatted = format_pvalue(p.adj),
            xmin = xmin + 1, # account for d0
            xmax = xmax +1 # account for d0
        )

    # # # Summary stats per group
    # # df.summary <- mean_expression %>%
    # #     group_by(timepoint, gel) %>%
    # #     summarize(
    # #         Mean = mean(MarkerExp, na.rm = TRUE),
    # #         sd = sd(MarkerExp, na.rm = TRUE),
    # #         sem = std.error(MarkerExp, na.rm = TRUE),
    # #         .groups = "drop"
    # #     )

    # Dodge positions for line plotting
    mean_expression <- mean_expression %>%
      mutate(Stimulus_Dodge = as.numeric(as.factor(timepoint))) %>%
      mutate(Stimulus_Dodge = case_when(
        timepoint == "d0" ~ as.numeric(as.factor(timepoint)),  # keep original
        gel %in% c("Fast", "VLVG") ~ Stimulus_Dodge - 0.25,
        TRUE ~ Stimulus_Dodge + 0.25
      ))

    # Plotting
    Plot <- mean_expression %>%
        ggplot(aes(x = timepoint, y = MarkerExp, color = gel, fill = gel)) +
        geom_line(aes(x = Stimulus_Dodge, group = interaction(donor, timepoint)), color = "black") +
        geom_point(aes(x = Stimulus_Dodge), shape = 21, size = 2, fill = "white") +
        scale_y_continuous(limits = c(0, NA), expand = expansion(mult = c(0, 0.3))) +
        scale_x_discrete(limits = c("0h","24h", "48h")) +
        scale_color_manual(values = Color.Gels) +
        scale_fill_manual(values = Color.Gels) +
        labs(x = "Timepoint", y = "expr. (a.u.)") +
        theme_fontsize(FontSize) +
        theme_Layout +
        theme(
            legend.position = "none",
            legend.direction = "horizontal",
            legend.title = element_blank(),
            legend.justification = "center",
            axis.title.x = element_blank(),
            plot.title = element_text(hjust = 0.5, size = 8)
        ) +
        ggtitle(Marker) +
        stat_pvalue_manual(
            inherit.aes = FALSE,
            data = stat.test,
            y.position = "yMax",
            label = "p_formatted",
            vjust = -0.25,
            tip.length = 0,
            linetype = "blank",
            size = FontSize / 2.835
        )

    return(Plot)
}