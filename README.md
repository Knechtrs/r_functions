# R Functions Library

This repository contains a structured set of reusable R functions, organized by purpose and used across multiple data analysis projects.

## Structure Overview

```

## Root Folder: `R_functions/`

- `.gitignore` — Git exclusions
- `.Rhistory` — R session history (typically ignored)
- `R_functions.Rproj` — RStudio project file

---

### Folder: `dev/`

- `source_folder.R` — Draft or in-development code

---

### Folder: `R/` — Reusable R Functions

#### Subfolder: `bose_analysis/` — Domain-specific tools for biomechanics
- `Import_process_data.R`

#### Subfolder: `config/` — YAML config and parameter readers
- `load_yaml.R`

#### Subfolder: `core/` — General-purpose utilities used across projects
- `create_summary_table.R`
- `format_pvalue.R`
- `fun_ggSave.R`
- `fun_ggSave_list.R`
- `fun_ggSave_PDF.R`
- `fun_ggSave_SVG.R`
- `load_all_bose.R` 

#### Subfolder: `plotting/` — ggplot helpers and plot types
- `add_stat_test_dodge.R`
- `plot_paired_points.R`
- `plot_summary_points.R`

#### Subfolder: `theme/` — Custom ggplot themes
- `theme_fontsize.R`
- `theme_layout.R`

---

### Folder: `templates/` — Project Scaffolding Tools

- `gitignore_template`
- `new_project.sh`
- `quarto_template.qmd`
- `README_template.md`

#### Subfolder: `templates/config/`
- `params.yaml`
- `theme_settings_poster.yaml`
- `theme_settings_ppt.yaml`
- `theme_settings_publication.yaml`

```

## Usage

- Functions in the `R/` directory are designed to be sourced into Quarto or R scripts using `here::here()`.
- Template files can be copied into new analysis projects to standardize folder structure and configuration.
- Use `dev/` for experimental functions that are not yet finalized.

## Notes

- All functions are version-controlled; avoid editing them directly inside a project. Instead, use `load_*` scripts to include only what’s needed.
- The `templates/` folder includes the project scaffold script (`new_project.sh`), starter Quarto document, and YAML configurations for themes.

## Author

Maintained by Raphael S. Knecht, AG Duda.
