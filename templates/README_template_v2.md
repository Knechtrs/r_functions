## Version history

- v1.0: Initial setup
- v2: less folder, better structured

C:.
├── config/            # YAML or JSON configuration files (settings, paths, thresholds)
├── data/
│   ├── raw/           # untouched original files
│   ├── processed/     # cleaned / processed data (multiple versions allowed)
│   └── metadata/      # sample sheets, experimental design
├── outputs/           # all generated outputs
│   ├── figures_raw/   # plots generated directly by scripts
│   ├── figures_final/ # cleaned plots for publication
│   ├── tables/        # exported summary tables
│   └── temp/          # intermediate temp files
├── reports/           # Quarto, RMarkdown reports (v1, v2, v3)
├── scripts/
│   ├── data_processing/   # clean, process, wrangle
│   ├── plotting/          # create plots
│   ├── analysis/          # stats, models
│   └── utilities/         # helper functions
├── R/                 # custom R functions (optional: could move into scripts/utilities)
└── README.md          # project overview

# How to Run:
- go to: C:\Users\knechtrs\OneDrive - Charité - Universitätsmedizin Berlin\Data_analysis\R_functions\templates
- double click on new_project.sh
- enter which data analysis subfolder folder should be created: without quotes!
- enter new folder name: without quotes!

- open r-project
- tools -> version control -> change to git # changes will be tracked by git, except folders defined in gitignore
- if you made changes or first set-up: go to git tab (top right), stage files and commit (#add comment, e.g. initilize)