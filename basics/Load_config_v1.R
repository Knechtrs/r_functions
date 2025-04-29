##########################################################################################################
#### 1. Load Configurations and functions
##########################################################################################################

# load param.yaml file with default settings
params <- load_param(default_file = here("config", "params_v1.yaml"))

# #---- optionally: change to  specific theme ----#
# #Powerpoint
# params <- load_param(override_file = here("config", "theme_settings_ppt_v1.yaml"))
# 
# # Poster
# params <- load_param(override_file = here("config", "theme_settings_poster_v1.yaml"))

# Publication
params <- load_param(override_file = here("config", "theme_settings_publication_v1.yaml"))


#---- Set theme settings ----# 

# set global fontsize
FontSize <- params$theme$FontSize

# define pointsize for geom_point
PointSize <- params$theme$PointSize

# define linewidht
LineWidth <- params$theme$LineWidth

# Define the colors for the gels
Color.Gels <- params$Color.Gels

# Define path to R_functions
path_Rfunctions <- params$path_Rfunctions
