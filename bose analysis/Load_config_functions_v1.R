##########################################################################################################
#### 1. Load Configurations and functions
##########################################################################################################

params <- read_yaml(here("config/params_v1.yaml"))
params


#---- Set theme settings ----# 

# set global fontsize
FontSize <- params$FontSize

# Define the colors for the gels
Color.Gels <- params$Color.Gels

# Define path to R_functions
path_Rfunctions <- params$path_Rfunctions

#--- load functions ---#

# load functions that imports all functions in folder
source(paste0(path_Rfunctions,"basics/load_AllFunctions_v1.R"))

# load all basic functions
load_AllFunctions(Folder = paste0(path_Rfunctions,"basics"))

# load all theme functions
load_AllFunctions(Folder = paste0(path_Rfunctions,"theme"))