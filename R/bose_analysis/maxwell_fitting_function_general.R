# Generalized fitting function

maxwell_model_n <- function(time, ...) {
  par <- list(...)
  n <- length(par) / 2
  A <- unlist(par[1:n])
  tau <- unlist(par[(n + 1):(2 * n)])
  rowSums(sapply(1:n, function(i) 1- A[i] * (1- exp(-time / tau[i]))))
}

maxwell_fitting_function_general <- function(
    df,
    time = "Time",
    fit_var = "load_norm",
    model_type = 1,            # can be "one", "two", or a numeric value like 3, 4, 5
    scan_models = FALSE,       # if TRUE, will scan from 1 to model_type
    criterion = "AIC"          # model selection: "AIC", "BIC", or "R2"
) {
  
  # browser()
  
  if (nrow(df) < 3 || !all(c(time, fit_var) %in% colnames(df))) return(NULL)
  
  df <- df[complete.cases(df[, c(time, fit_var)]), ]
  df <- df[df[[time]] > 0, ]
  if (nrow(df) < 3) return(NULL)
  
  # Determine number of elements
  if (model_type == "one") model_type <- 1
  if (model_type == "two") model_type <- 2
  max_elements <- as.numeric(model_type)
  
  # If scan_models is TRUE: loop over models 1:max_elements
  range_elements <- if (scan_models) 1:max_elements else max_elements
  
  all_fits <- list()
  
  for (n_elements in range_elements) {
    param_names <- c(paste0("A", 1:n_elements), paste0("tau", 1:n_elements))
    formula_str <- paste0(fit_var, " ~ maxwell_model_n(", time, ", ", paste(param_names, collapse = ", "), ")")
    
    # Generate starting values
    starting_values_list <- lapply(1:3, function(i) {
      
      # Better spacing of time constants
      log_taus <- seq(log(min(df[[time]])), log(max(df[[time]]) * 10), length.out = n_elements)
      taus <- exp(log_taus) * runif(n_elements, 0.5, 2.0)
      
      # More conservative amplitude distribution
      As <- rep(0.8/n_elements, n_elements) * runif(n_elements, 0.8, 1.2)
      # 
      # taus <- exp(seq(log(1), log(1000), length.out = n_elements)) * runif(n_elements, 0.8, 1.2)
      # As <- runif(n_elements, 0.2, 0.8)
      # As <- As / sum(As) * 0.95  # Ensure sum is below 1 to allow equilibrium offset
      
      # As <- rep(1 / n_elements, n_elements)
      # As <- As / sum(As * runif(n_elements, 0.9, 1.1))  # keeps total near 1
      # taus <- seq(10, 2000, length.out = n_elements) * runif(n_elements, 0.9, 1.1)
      
      # taus <- seq(10, 2000, length.out = n_elements) * runif(n_elements, 0.9, 1.1)
      # As <- rep(1 / n_elements, n_elements) * runif(n_elements, 0.9, 1.1)
      par <- setNames(c(As, taus), param_names)
      par
    })
    
    best_fit <- NULL
    best_error <- Inf
    
    for (start_values in starting_values_list) {
      tryCatch({
        fit <- nlsLM(
          formula = as.formula(formula_str),
          data = df,
          start = start_values,
          lower = rep(0.0001, 2 * n_elements),
          upper = rep(Inf, 2 * n_elements),
          control = nls.lm.control(maxiter = 1000)
        )
        current_error <- sum((df[[fit_var]] - predict(fit))^2)
        if (current_error < best_error) {
          best_fit <- fit
          best_error <- current_error
        }
      }, error = function(e) {
        message("Fit failed for ", n_elements, " elements: ", e$message, "\n", "start values:", start_values )
      })
    }
    
    if (is.null(best_fit)) next
    
    # Generate predictions and residuals
    time_vec <- df[[time]]
    stress_predict <- do.call(maxwell_model_n, c(list(time_vec), as.list(coef(best_fit))))
    df_predict <- tibble(!!time := time_vec, !!fit_var := stress_predict)
    residuals <- df[[fit_var]] - predict(best_fit, newdata = df)
    df_residuals <- tibble(!!time := df[[time]], residuals = residuals)
    
    # Stats
    ss_total <- sum((df[[fit_var]] - mean(df[[fit_var]]))^2)
    ss_residual <- sum(residuals^2)
    r_squared <- 1 - (ss_residual / ss_total)
    rmse <- sqrt(mean(residuals^2))
    aic <- AIC(best_fit)
    bic <- BIC(best_fit)
    
    all_fits[[n_elements]] <- list(
      n_elements = n_elements,
      optimized_params = best_fit,
      df_predict = df_predict,
      df_residuals = df_residuals,
      r_squared = r_squared,
      rss = ss_residual,
      rmse = rmse,
      aic = aic,
      bic = bic,
      n = nrow(df)
    )
  }
  
  # return(all_fits)
  
  # # Assume all_fits is a list of model fit results (some may be NULL)
  #
  summary_table <- purrr::compact(all_fits) %>%   # remove NULLs
    purrr::map_dfr(~{
      tibble(
        n_elements       = .x$n_elements,
        r_squared        = .x$r_squared,
        rmse             = .x$rmse,
        rss              = .x$rss,
        aic              = .x$aic,
        bic              = .x$bic,
        n_obs            = .x$n
      )
    })
  
  df_residuals <- purrr::compact(all_fits) %>%   # remove NULLs
    purrr::map_dfr(~{
      tibble(
        n_elements = .x$n_elements,
        time = .x$df_residuals$Time,
        residuals = .x$df_residuals$residuals
      )
    })
  
  return(
    list(
      summary_table,
      df_residuals = df_residuals
    )
  )
  
  # if (length(all_fits) == 0) return(NULL)
  #
  # # Select best model based on criterion
  # criterion <- tolower(criterion)
  # best_model_index <- switch(criterion,
  #                            "aic" = which.min(sapply(all_fits, function(x) x$aic)),
  #                            "bic" = which.min(sapply(all_fits, function(x) x$bic)),
  #                            "r2"  = which.max(sapply(all_fits, function(x) x$r_squared)),
  #                            stop("Invalid criterion specified")
  # )

  # return(all_fits[[best_model_index]])
}
