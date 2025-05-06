# === Load required libraries ===
library(tidyverse)     # dplyr, ggplot2, purrr, tidyr
library(minpack.lm)    # for nlsLM() nonlinear fits
library(signal)        # for Butterworth filtering
library(ggpubr)        # used in plotting summary points
library(forcats)       # factor handling
library(stringr)       # string manipulation
library(rlang)         # for tidy evaluation (e.g., .data)
library(purrr)         # used in nest/map for model fitting
library(yaml)          # for loading configuration parameters

# === Source all R scripts from bose_analysis ===
bose_path <- here::here("R", "bose_analysis")
bose_scripts <- list.files(bose_path, pattern = "\.R$", full.names = TRUE)

for (script in bose_scripts) {
  message("Sourcing: ", basename(script))
  source(script)
}
