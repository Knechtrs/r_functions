
#---- Plot r squared plot to see which model worked better ----#+
maxwell_plot_rsquared <- function(
    df1 = list_maxwell_results_one$r_squared, # result from one element fit
    df2 = list_maxwell_results_two$r_squared, # result from two element fit
    id = "exp", # sample id 
    color1 = "#21908C", # color one element
    color2 = "#E6AB02" # color two element
    ){
  df_r_squared <- full_join(df1, df2, by=id)
  
  Plot_r_squared <- df_r_squared %>% 
    pivot_longer(cols= starts_with("r_squared"), names_to = "Model", values_to = "r_squared") %>% 
    mutate(Model = recode(Model,
                          "r_squared_one" = "one element",
                          "r_squared_two" = "two element")) %>% 
    plot_summary_points(
      data = .,
      xvar = "Model",
      yvar = "r_squared",
      fillvar = "Model",
      colors = c("one element" = color1, "two element" = color2),
      fontsize = FontSize
    )
  
  return(list(
    "df_r_squared" = df_r_squared,
    "Plot_r_squared" = Plot_r_squared
    ))
}
