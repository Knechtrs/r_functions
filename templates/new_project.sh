
# Ask for data analysis subfolder: 
echo "Define data analysis subfolder:"
read subfolder

# Define the base directory where new projects should be created
basedir="/c/Users/knechtrs/OneDrive - Charité - Universitätsmedizin Berlin/Data_analysis/${subfolder}/"

# Ask for project name
echo "Enter new project folder name:"
read projectname

# Create the project folder at the correct place
mkdir -p "${basedir}${projectname}/R"
mkdir -p "${basedir}${projectname}/config"
mkdir -p "${basedir}${projectname}/figures"
mkdir -p "${basedir}${projectname}/data/raw"
mkdir -p "${basedir}${projectname}/data/processed"
mkdir -p "${basedir}${projectname}/data/plots_rds"
mkdir -p "${basedir}${projectname}/Output/final_figures/version_1"
mkdir -p "${basedir}${projectname}/Output/temp"
mkdir -p "${basedir}${projectname}/scripts"
mkdir -p "${basedir}${projectname}/reports"

# Set template directory manually (absolute path)
templatedir="/c/Users/knechtrs/OneDrive - Charité - Universitätsmedizin Berlin/Data_analysis/R_functions/templates"

# Create README file
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

# Create .Rproj file
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
echo "Project '${projectname}' created successfully in ${basedir}"
