#!/bin/bash

#--------------------------
# 1. Ask for basic info
#--------------------------

# Ask for data analysis subfolder
echo "Define data analysis subfolder:"
read subfolder

# Define base directory
basedir="/c/Users/knechtrs/OneDrive - Charité - Universitätsmedizin Berlin/Data_analysis/${subfolder}/"

# Ask for project name
echo "Enter new project folder name:"
read projectname

#--------------------------
# 2. Create folder structure
#--------------------------

mkdir -p "${basedir}${projectname}/config"
mkdir -p "${basedir}${projectname}/data/raw"
mkdir -p "${basedir}${projectname}/data/processed"
mkdir -p "${basedir}${projectname}/data/metadata"
mkdir -p "${basedir}${projectname}/outputs/figures_raw"
mkdir -p "${basedir}${projectname}/outputs/figures_final"
mkdir -p "${basedir}${projectname}/outputs/tables"
mkdir -p "${basedir}${projectname}/outputs/temp"
mkdir -p "${basedir}${projectname}/reports"
mkdir -p "${basedir}${projectname}/scripts/data_processing"
mkdir -p "${basedir}${projectname}/scripts/plotting"
mkdir -p "${basedir}${projectname}/scripts/analysis"
mkdir -p "${basedir}${projectname}/scripts/utilities"

echo "Folder structure created."

#--------------------------
# 3. Copy templates
#--------------------------

# Template directory
templatedir="/c/Users/knechtrs/OneDrive - Charité - Universitätsmedizin Berlin/Data_analysis/R_functions/templates"

# Copy README template
if [ -f "${templatedir}/README_template.md" ]; then
  cp "${templatedir}/README_template.md" "${basedir}${projectname}/README.md"
  echo "README template copied."
else
  echo "Warning: No README_template.md found!"
fi

# Copy .gitignore template
if [ -f "${templatedir}/gitignore_template" ]; then
  cp "${templatedir}/gitignore_template" "${basedir}${projectname}/.gitignore"
  echo ".gitignore template copied."
else
  echo "Warning: No gitignore_template found!"
fi

#--------------------------
# 4. Copy default YAML config templates
#--------------------------

templateconfigdir="${templatedir}/config"

if [ -d "${templateconfigdir}" ]; then
  cp "${templateconfigdir}"/*.yaml "${basedir}${projectname}/config/"
  echo "Config templates (.yaml) copied to config/ folder."
else
  echo "Warning: Config templates folder not found!"
fi

# #--------------------------
# # 5. Copy default R scripts
# #--------------------------

# # Define source directories
# rbasics="/c/Users/knechtrs/OneDrive - Charité - Universitätsmedizin Berlin/Data_analysis/R_functions/basics"
# rtheme="/c/Users/knechtrs/OneDrive - Charité - Universitätsmedizin Berlin/Data_analysis/R_functions/theme"

# # Copy to scripts/utilities
# cp "${rbasics}/create_summary_table_v2.R" "${basedir}${projectname}/scripts/utilities/"
# cp "${rbasics}/Load_config_v1.R" "${basedir}${projectname}/scripts/utilities/"
# cp "${rbasics}/Load_param_v1.R" "${basedir}${projectname}/scripts/utilities/"

# # Copy to scripts/plotting
# cp "${rbasics}/Add_stat_test_dodge_v1.R" "${basedir}${projectname}/scripts/plotting/"
# cp "${rbasics}/format_pvalue_v1.R" "${basedir}${projectname}/scripts/plotting/"
# cp "${rbasics}/plot_summary_points_v3.R" "${basedir}${projectname}/scripts/plotting/"
# cp "${rtheme}/theme_fontsize_v1.R" "${basedir}${projectname}/scripts/plotting/"

# echo "Default R scripts copied to utilities/ and plotting/ folders."

#--------------------------
# 6. Create .Rproj file
#--------------------------

cat > "${basedir}${projectname}/${projectname}.Rproj" << EOL
Version: 1.0

RestoreWorkspace: No
SaveWorkspace: No
AlwaysSaveHistory: Default

EnableCodeIndexing: Yes
UseSpacesForTab: Yes
NumSpacesForTab: 2
Encoding: UTF-8

RnwWeave: knitr
LaTeX: pdfLaTeX
EOL

echo ".Rproj project file created."

#--------------------------
# 6. Final message
#--------------------------

echo "✅ Project '${projectname}' created successfully in ${basedir}${projectname}/"
echo "Press any key to exit..."
read -n 1 -s
