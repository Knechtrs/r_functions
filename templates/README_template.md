# Project Title

Short description of the project.

## Project Structure

- `R/` – custom functions
- `config/` - input variables
- `figures/` – plotting scripts
- `data/raw/` – raw input data
- `data/processed/` – cleaned/processed data
- `data/plots_rds/` – saved ggplot objects
- `Output/final_figures/` – final publication-ready plots
- `Output/temp/` – temporary plots (ignored by git)
- `scripts/` – analysis scripts
- `reports/` – knitted reports

## Setup

## Version history

- v1.0: Initial setup

## restore version using renv::restore()

## How to Run:
- go to: C:\Users\knechtrs\OneDrive - Charité - Universitätsmedizin Berlin\Data_analysis\R_functions\templates
- double click on new_project.sh
- enter which data analysis subfolder folder should be created
- enter new folder name

- open r-project
- tools -> version control -> change to git # changes will be tracked by git, except folders defined in gitignore
- if you made changes or first set-up: go to git tab (top right), stage files and commit (#add comment, e.g. initilize)