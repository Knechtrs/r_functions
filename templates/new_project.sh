#!/bin/bash

# -----------------------------------------------
# AG Duda / Raphael Knecht — New Project Scaffold
# Sets up folders, templates, and utility scripts
# Cross-platform compatible (Windows/macOS/Linux)
# -----------------------------------------------

# === 1. Prompt user for input ===

read -p "Define data analysis subfolder: " subfolder
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
renv_template="${userbase}/R_functions/templates/renv_template"
cp "${renv_template}/renv.lock" "${projectdir}/"
cp "${renv_template}/.Rprofile" "${projectdir}/"
mkdir -p "${projectdir}/renv"
cp "${renv_template}/renv/activate.R" "${projectdir}/renv/"
cp "${renv_template}/renv/settings.json" "${projectdir}/renv/"

cp "${userbase}/R_functions/templates/quarto_template.qmd" "${projectdir}/reports/" 2>/dev/null
cp "${userbase}/R_functions/templates/gitignore_template" "${projectdir}/.gitignore" 2>/dev/null
cp "${userbase}/R_functions/templates/README_template.md" "${projectdir}/README.md" 2>/dev/null
cp "${rfunctions}/config/"*.yaml "${projectdir}/config/" 2>/dev/null

echo "Template files copied to project folder."

# === 5. Git Init ===
cd "${projectdir}"
git init
echo "Initialized empty Git repository."

# === 6. Add renv auto-restore logic to .Rprofile ===
echo '' >> "${projectdir}/.Rprofile"
echo '# Auto-restore renv on project load' >> "${projectdir}/.Rprofile"
echo 'if (!requireNamespace("renv", quietly = TRUE)) install.packages("renv")' >> "${projectdir}/.Rprofile"
echo 'tryCatch(renv::restore(prompt = FALSE), error = function(e) message("renv restore skipped: ", e$message))' >> "${projectdir}/.Rprofile"
echo "Auto-restore logic added to .Rprofile."


# === 7. Create RStudio Project File ===
rproj_file="${projectdir}/${projectname}.Rproj"
echo "Version: 1.0" > "$rproj_file"
echo "RestoreWorkspace: No" >> "$rproj_file"
echo "SaveWorkspace: No" >> "$rproj_file"
echo "AlwaysSaveHistory: Default" >> "$rproj_file"
echo "EnableCodeIndexing: Yes" >> "$rproj_file"
echo "UseSpacesForTab: Yes" >> "$rproj_file"
echo "NumSpacesForTab: 2" >> "$rproj_file"
echo "Encoding: UTF-8" >> "$rproj_file"
echo "RStudio project file created."

# === Done ===
echo "Project '${projectname}' is ready at: ${projectdir}"
echo "Press any key to exit..."
read -n 1 -s
