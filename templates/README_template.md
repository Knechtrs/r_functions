# Data Analysis Project Template

This repository follows a standardized structure for data analysis project, designed for reproducibility, modularity, and clarity.

## Project Structure

```
project_root/
├── config/                  # YAML configuration and theme settings
│   ├── params_v1.yaml
│   ├── theme_settings_poster_v1.yaml
│   ├── theme_settings_ppt_v1.yaml
│   └── theme_settings_publication_v1.yaml
│
├── data/                    # All input data and derived intermediate data
│   ├── raw/                 # Unmodified source data
│   ├── metadata/            # Data dictionaries or annotations
│   └── processed/           # Cleaned or transformed datasets
│
├── outputs/                 # All analysis outputs
│   ├── figures_raw/         # Diagnostic or exploratory plots
│   ├── figures_final/       # Final publication-ready plots
│   ├── tables/              # Result tables, CSVs, RDS, etc.
│   └── temp/                # Temporary or cached files
│
├── reports/                 # Quarto notebooks, PDFs, manuscripts
│
├── scripts/                 # Code modules used for the analysis
│   ├── analysis/            # Main analysis workflow scripts
│   ├── data_processing/     # Data wrangling and cleaning
│   ├── plotting/            # Plot-generating functions and layouts
│   └── utilities/           # Helpers, stat tests, shared functions
│
├── .gitignore               # Git version control exclusions
├── README.md                # This file
└── your_project.Rproj       # RStudio project file
```

## Usage Notes

- All reusable functions or modules go into `scripts/` subfolders based on purpose.
- Use `here::here()` to construct file paths reliably across systems.
- Place raw data files only in `data/raw/` and never modify them.
- Use `outputs/` for all generated content to keep inputs and outputs cleanly separated.

## Getting Started

1. Set up your config files in `config/`
2. Add raw data to `data/raw/` and write processing logic in `scripts/data_processing/`
3. Write your analysis in `scripts/analysis/` and reporting in `reports/`
4. Save all output content to `outputs/`

## Versioning and Environment

- Use Git to track changes.

## Authors
Raphael S Knecht (May 2025)
