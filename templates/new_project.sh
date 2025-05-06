#!/bin/bash

# -----------------------------------------------
# AG Duda / Raphael Knecht — New Project Scaffold
# Sets up folders, templates, and utility scripts
# Cross-platform compatible (Windows/macOS/Linux)
# -----------------------------------------------

# === 1. Prompt user for input ===

read -p "Define data analysis subfolder (e.g. fracture, macrophage, etc.): " subfolder
subfolder=${subfolder:-"general"}

read -p "Enter new project folder name: " projectname
if [ -z "$projectname" ]; then
  echo "ERROR: Project name is required. Exiting."
  exit 1
fi

# === 2. Define Paths (Windows-safe) ===

real_home="$(cd "$USERPROFILE" && pwd)"
userbase="${real_home}/OneDrive - Charité - Universitätsmedizin Berlin/Data_analysis"
basedir="${userbase}/${subfolder}"
projectdir="${basedir}/${projectname}"
rfunctions="${userbase}/R_functions/R"

echo "Detected real home: $real_home"
echo "Using userbase: $userbase"

# === 3. Create Folder Structure ===

mkdir -p "${projectdir}"/{config,data/{raw,metadata,processed},outputs/{figures_raw,figures_final,tables,temp},reports,scripts/{analysis,data_processing,plotting,utilities}}

echo "Created folder structure in: ${projectdir}"

# === 4. Copy Template Files ===

cp "${userbase}/R_functions/templates/quarto_template.qmd" "${projectdir}/reports/" 2>/dev/null
cp "${userbase}/R_functions/templates/gitignore_template" "${projectdir}/.gitignore" 2>/dev/null
cp "${userbase}/R_functions/templates/README_template.md" "${projectdir}/README.md" 2>/dev/null
cp "${rfunctions}/config/"*.yaml "${projectdir}/config/" 2>/dev/null

echo "Template files copied to project folder."

# === 5. Git Init ===
cd "${projectdir}"
git init
echo "Initialized empty Git repository."

# === 6. renv Setup ===
echo "renv::init()" > "${projectdir}/scripts/init_renv.R"
echo "Added renv init script."

# === Done ===
echo "Project '${projectname}' is ready at: ${projectdir}"
echo "Press any key to exit..."
read -n 1 -s
