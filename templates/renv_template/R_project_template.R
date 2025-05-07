# renv::init()
install.packages(c(
  "here", "tidyverse", "ggtext", "ggrepel", "ggpubr",
  "rstatix", "yaml", "cowplot", "sessioninfo", "rmarkdown"
))
renv::snapshot()
