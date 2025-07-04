# Generalized fitting function

maxwell_model_n <- function(time, ...) {
  par <- list(...)
  n <- length(par) / 2
  A_partial <- unlist(par[1:(n-1)])  # Only fit n-1 amplitudes
  tau <- unlist(par[n:(2*n-1)])      # All time constants
  
  # Last amplitude ensures sum = 1
  A_last <- 1 - sum(A_partial)
  A <- c(A_partial, A_last)
  
  rowSums(sapply(1:n, function(i) A[i] * exp(-time / tau[i])))
}


# maxwell_model_n <- function(time, ...) { #... allows the function to accept a variable number of parameters
#   par <- list(...) # Captures all additional arguments as a list
#   n <- length(par) / 2  # Number of Maxwell elements
#   A <- unlist(par[1:n])
#   tau <- unlist(par[(n + 1):(2 * n)])
#   rowSums(sapply(1:n, function(i) A[i] * exp(-time / tau[i]))) # rowSums(...): Sum all Maxwell elements together
# }

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
    # param_names <- c(paste0("A", 1:n_elements), paste0("tau", 1:n_elements))
    param_names <- c(paste0("A", 1:(n_elements-1)), paste0("tau", 1:n_elements))
    formula_str <- paste0(fit_var, " ~ maxwell_model_n(", time, ", ", paste(param_names, collapse = ", "), ")")
    
    starting_values_list <- lapply(1:3, function(i) {
      log_taus <- seq(log(min(df[[time]])), log(max(df[[time]]) * 10), length.out = n_elements)
      taus <- exp(log_taus) * runif(n_elements, 0.5, 2)
      
      # Only generate n_elements - 1 A parameters (last one will be calculated)
      As <- rep(0.8/n_elements, n_elements - 1) * runif(n_elements - 1, 0.8, 1.2)
      
      # Ensure the sum of A parameters leaves room for the calculated last parameter
      As <- As * (0.8 / sum(As))  # Scale so sum is reasonable
      
      par <- setNames(c(As, taus), param_names)
      par
    })
    
    # # Generate starting values
    # starting_values_list <- lapply(1:3, function(i) {
    #   # 
    #   # Better spacing of time constants
    #   log_taus <- seq(log(min(df[[time]])), log(max(df[[time]]) * 10), length.out = n_elements)
    #   taus <- exp(log_taus) * runif(n_elements, 0.5, 2.0)
    # 
    #   # More conservative amplitude distribution
    #   As <- rep(0.8/n_elements, n_elements) * runif(n_elements, 0.8, 1.2)
    #  
    #   par <- setNames(c(As, taus), param_names)
    #   par
    # })
    
    
    best_fit <- NULL
    best_error <- Inf
    fit_attempts <- 0
    successful_fits <- 0
    
    for (start_values in starting_values_list) {
      fit_attempts <- fit_attempts + 1
      tryCatch({
        fit <- nlsLM(
          formula = as.formula(formula_str),
          data = df,
          start = start_values,
          lower = rep(0.0001, 2 * n_elements),
          upper = rep(Inf, 2 * n_elements),
          control = nls.lm.control(maxiter = 1000)
        )
        successful_fits <- successful_fits + 1
        current_error <- sum((df[[fit_var]] - predict(fit))^2)
        if (current_error < best_error) {
          best_fit <- fit
          best_error <- current_error
        }
      }, error = function(e) {
        # Silently continue to next starting value
      })
    }
    
    # Only show message if all attempts failed
    if (successful_fits == 0) {
      message("All ", fit_attempts, " fitting attempts failed for Donor ", unique(df$PatientLetter), ": ", n_elements, " elements")
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
  
  df_fit <- purrr::compact(all_fits) %>%   # remove NULLs
    purrr::map_dfr(~{
      tibble(
        n_elements = .x$n_elements,
        time = .x$df_residuals$Time,
        stress = df[[fit_var]],
        stress_predict = .x$df_predict$load_norm,
        residuals = .x$df_residuals$residuals
      )
    })
  
  return(
    list(
      summary_table,
      df_fit = df_fit
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
