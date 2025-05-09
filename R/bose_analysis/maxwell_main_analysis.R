maxwell_main_analysis <- function(
    df = data_StressRelax,
    t_max = params$tMax,
    model_type = "two",
    id = id, # sample identifier. Analysis is performed per sample 
    time = "Time", # time column name
    fit_var = "Load_norm" # column name of data to fit
  ){

# Clean and split data by experimental groups and name list items
list_df <- df %>%
  na.omit() %>%
  droplevels() %>%
  group_by(across(all_of(id)))

names_list <- group_keys(list_df)[[1]]  # assumes `id` is a single column

list_df <- group_split(list_df)         # now split
names(list_df) <- names_list            # assign names


# Apply fitting to each dataset:  uses custom maxwell_fitting_function
list_fitted <- lapply(list_df, function(df) {
  maxwell_fitting_function(
    df %>% drop_units(.),
    time = "Time",
    fit_var = fit_var,
    model_type = model_type
  )
})


# Extract parameters
params_fitted <- map_df(list_fitted, ~ coef(.x$optimized_params), .id = id)

# add total relaxation column to two element model
if(model_type == "two") {
  params_fitted <- params_fitted %>%
    mutate(
      total_relaxation = A1 + A2
      # RelaxationPercentage = TotalRelaxation * 100
    )
}

# Get fitted data and residuals
data_fitted <- map_df(list_fitted, ~ .x$df_predict, .id = id)
data_residuals <- map_df(list_fitted, ~ .x$df_residuals, .id = id)
data_r_squared <- map_df(list_fitted, ~ tibble(r_squared = .x$r_squared), .id = id) %>%
  rename_with(~ paste0("r_squared_", model_type), .cols = "r_squared")


return(list(
  parameters = params_fitted,
  fitted_data = data_fitted,
  residuals = data_residuals,
  r_squared = data_r_squared,
  model_fits = list_fitted,
  model_type = model_type
))

}
