
# Define Maxwell models
maxwell_model_one <- function(t, tau, A) {
  1 - A * (1 - exp(-t / tau))
}

maxwell_model_two <- function(t, tau1, tau2, A1, A2) {
  1 - A1 * (1 - exp(-t / tau1)) - A2 * (1 - exp(-t / tau2))
}

# define global variabls 


# Main function to fit data with either model
fit_maxwell_model <- function(data_StressRelax, model_type = "one", tMax) {
  # Clean and split data by experimental groups
  list_StressRelax <- data_StressRelax %>%
    na.omit() %>%
    droplevels() %>%
    split(.$exp)
  
  # Core fitting function for both models
  fit_model <- function(df, model_type) {
    # Basic data validation
    if (nrow(df) < 3 || !all(c("Time", "Load_norm") %in% colnames(df))) {
      return(NULL)
    }
    
    df <- df[complete.cases(df[, c("Time", "Load_norm")]), ]
    df <- df[df$Time > 0, ]

    
    if (nrow(df) < 3) return(NULL)

    # Attempt to fit the appropriate model
    if (model_type == "one") {
      # Try with multiple starting values if needed
      starting_values <- list(
        list(tau = 100, A = 0.5),
        list(tau = 10, A = 0.2),
        list(tau = 500, A = 0.8)
      )
    } else {
      starting_values <- list(
        list(tau1 = 70, tau2 = 1500, A1 = 0.3, A2 = 0.7),
        list(tau1 = 200, tau2 = 1000, A1 = 0.4, A2 = 0.5),
        list(tau1 = 10, tau2 = 200, A1 = 0.4, A2 = 0.5)
        
      )
    }

    # Try each set of starting values
    best_fit <- NULL
    best_error <- Inf

    for (start_values in starting_values) {
      tryCatch({
        if (model_type == "one") {
          test_fit <- nlsLM(
            Load_norm ~ maxwell_model_one(Time, tau, A),
            data = df,
            start = start_values,
            lower = c(0.001, 0.001),
            upper = c(Inf, 1),
            control = nls.lm.control(maxiter = 1000)
          )
        } else {
          test_fit <- nlsLM(
            Load_norm ~ maxwell_model_two(Time, tau1, tau2, A1, A2),
            data = df,
            start = start_values,
            lower = c(0, 0, 0, 0),
            upper = c(Inf, Inf, 1, 1),
            control = nls.lm.control(maxiter = 500)
          )
        }

        current_error <- sum((df$Load_norm - predict(test_fit))^2)
        if (current_error < best_error) {
          best_error <- current_error
          best_fit <- test_fit
        }
      }, error = function(e) {
        # Silent failure
      })
    }

    if (is.null(best_fit)) return(NULL)

    # Generate predictions
    # Time_vec <- seq(0.01, tMax, by = 0.01)
    Time_vec <- df$Time
    

    if (model_type == "one") {
      load_predict <- maxwell_model_one(
        Time_vec,
        coef(best_fit)["tau"],
        coef(best_fit)["A"]
      )
    } else {
      load_predict <- maxwell_model_two(
        Time_vec,
        coef(best_fit)["tau1"],
        coef(best_fit)["tau2"],
        coef(best_fit)["A1"],
        coef(best_fit)["A2"]
      )
    }

    df_predict <- data.frame(Time = Time_vec, load_predict = load_predict)
    residuals <- df$Load_norm - predict(best_fit, newdata = df)
    df_residuals <- data.frame(Time = df$Time, residuals = residuals)

    # Calculate R-squared
    ss_total <- sum((df$Load_norm - mean(df$Load_norm))^2)
    ss_residual <- sum(residuals^2)
    r_squared <- 1 - (ss_residual/ss_total)

    return(list(
      optimized_params = best_fit,
      df_predict = df_predict,
      df_residuals = df_residuals,
      r_squared = r_squared
    ))
  }

  # Apply fitting to each dataset
  list_fitted <- lapply(list_StressRelax, function(df) fit_model(df, model_type))
  list_fitted <- list_fitted[!sapply(list_fitted, is.null)]

  if (length(list_fitted) == 0) return(NULL)

  # Extract parameters
  params_fitted <- map_df(list_fitted, ~ coef(.x$optimized_params), .id = "exp")

  if (model_type == "two") {
  #   params_fitted <- params_fitted %>%
  #     mutate(RelaxationPercentage = A * 100)
  # } else {
    params_fitted <- params_fitted %>%
      mutate(
        TotalRelaxation = A1 + A2
        # RelaxationPercentage = TotalRelaxation * 100
      )
  }

  # Get fitted data and residuals
  data_fitted <- map_df(list_fitted, ~ .x$df_predict, .id = "exp")
  data_residuals <- map_df(list_fitted, ~ .x$df_residuals, .id = "exp")
  data_r_squared <- map_df(list_fitted, ~ .x$r_squared, .id = "exp")
  data_r_squared <- data_r_squared %>% pivot_longer(cols = everything() ,names_to = "Patient_ID", values_to = paste0("r_squared_", model_type))



  return(list(
    parameters = params_fitted,
    fitted_data = data_fitted,
    residuals = data_residuals,
    r_squared = data_r_squared,
    model_fits = list_fitted,
    model_type = model_type
  ))
}

# Simplified plotting function
plot_maxwell_fit <- function(result, data_StressRelax, tMax) {
  if (is.null(result)) return(NULL)

  model_type <- result$model_type

  # Reduce number of points for plotting
  data_fitted_sliced <- result$fitted_data %>%
    group_by(exp) %>%
    slice(seq(1, n(), by = floor(n()/100))) %>%
    ungroup()

  # Create parameter labels
  if (model_type == "one") {
    fitted_params_df <- result$parameters %>%
      mutate(label = sprintf("τ = %.1f, A = %.2f", tau, A)) %>%
      select(exp, label)
  } else {
    fitted_params_df <- result$parameters %>%
      mutate(label = sprintf("τ1 = %.1f, τ2 = %.1f\nA1 = %.2f, A2 = %.2f", tau1, tau2, A1, A2)) %>%
      select(exp, label)
  }

  # # Label positions
  # text_positions <- data_fitted_sliced %>%
  #   group_by(exp) %>%
  #   summarize(
  #     Time = max(Time) * 0.5,
  #     load_predict = min(load_predict) + 0.1
  #   )
  #
  # label_positions <- fitted_params_df %>%
  #   left_join(text_positions, by = "exp")

  # Create plots
  plot_fitted <- ggplot() +
    # Change geom_path to include mapping for the legend
    geom_path(data = data_StressRelax,
              aes(x = Time, y = Load_norm, color = "Sample"),
              linewidth = 1.5) +
    geom_path(data = data_fitted_sliced,
              aes(x = Time, y = load_predict, color = "Fit"),
              alpha = 1,
              linewidth = 1,
              linetype="11") +
    # geom_text(data = label_positions,
    #           aes(x = Time, y = load_predict, label = label),
    #           hjust = 0, vjust = 0, size = 3) +
    facet_wrap(~ exp, nrow=2) +
    # Add scale_color_manual to control the colors and legend
    scale_color_manual(name = "", # No title for the legend
                       values = c("Sample" = "#762A83",
                                  "Fit" = "orange")) +
    scale_x_continuous(limits=c(0,tMax), expand=expansion(mult=c(0.05,0))) +
    labs(
      x = "Time (s)",
      y = "Normalized stress"
    ) +
    theme_Layout +
    theme_fontsize(8)

  plot_residuals <- ggplot(result$residuals, aes(x = Time, y = residuals)) +
    geom_point(alpha = 0.5, size = 0.5) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
    facet_wrap(~ exp, nrow = 2) +
    labs(
      title = paste(ifelse(model_type == "one", "One-Element", "Two-Element"), "Maxwell Model Residuals"),
      x = "Time",
      y = "Residuals"
    ) +
    theme_Layout +
    theme_fontsize(8)

  return(list(
    plot_fitted = plot_fitted,
    plot_residuals = plot_residuals
  ))
}

# Simplified main function
analyze_maxwell_model <- function(data_StressRelax, model_type = "one", tMax = max(data_StressRelax$Time)) {
  if (!model_type %in% c("one", "two")) {
    stop("Model type must be either 'one' or 'two'")
  }

  result <- fit_maxwell_model(data_StressRelax, model_type, tMax)
  plots <- plot_maxwell_fit(result, data_StressRelax, tMax)

  return(list(
    result = result,
    plots = plots
  ))
}



