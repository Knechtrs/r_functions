
# Create a function to calculate linear regression for each group
calc_regression <- function(data, xvar, yvar) {
  # Convert column names to symbols
  x <- rlang::ensym(xvar)
  y <- rlang::ensym(yvar)
  
  # Create formula dynamically
  formula <- as.formula(paste0(rlang::as_string(y), " ~ ", rlang::as_string(x)))
  
  # Fit model
  model <- lm(formula, data = data)
  
  # Extract R² and p-value
  r_squared <- summary(model)$r.squared
  p_value <- summary(model)$coefficients[2, 4]
  
  # Return result as data.frame
  return(data.frame(
    R2 = round(r_squared, 2),
    p_value = round(p_value, 3),
    label = paste0(
      "R² = ", round(r_squared, 2),
      "\np = ", ifelse(p_value < 0.001, "< 0.001", round(p_value, 3))
    )
  ))
}