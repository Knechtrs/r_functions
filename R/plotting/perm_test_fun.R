perm_test_fun <- function(data, formula, paired = TRUE, detailed = TRUE, n_perm = 10000, ...) {
  # Check required packages
  required_packages <- c("dplyr", "tidyr", "coin", "rlang")
  for (pkg in required_packages) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      stop(paste("Package", pkg, "needed for this function to work. Please install it."))
    }
  }
  
  # Extract variable names from formula
  response_var <- all.vars(formula)[1]
  group_var <- all.vars(formula)[2]
  
  # Get ID column name for paired test
  id_col <- "ID_Col"  # This matches the renamed column in add_stat_test_dodge
  
  # Check if paired test is requested but ID column is missing
  if (paired && !id_col %in% names(data)) {
    stop("Paired permutation test requires an ID column. Please provide the 'id' parameter.")
  }
  
  # Get group levels
  group_levels <- unique(data[[group_var]])
  if (length(group_levels) != 2) {
    stop("Permutation test requires exactly two groups to compare")
  }
  
  # Prepare output in the format expected by add_stat_test_dodge
  create_output <- function(p_value, stat_value = NA, method_name = "Permutation test") {
    # Format the output to match rstatix::t_test or rstatix::wilcox_test
    p_signif <- if (p_value < 0.001) "***" else
      if (p_value < 0.01) "**" else
        if (p_value < 0.05) "*" else
          if (p_value < 0.1) "." else "ns"
    
    data.frame(
      .y. = response_var,
      group1 = group_levels[1],
      group2 = group_levels[2],
      n1 = sum(!is.na(data[data[[group_var]] == group_levels[1], response_var])),
      n2 = sum(!is.na(data[data[[group_var]] == group_levels[2], response_var])),
      statistic = stat_value,
      p = p_value,
      p.adj = p_value,  # No adjustment in single test
      p.format = format.pval(p_value, digits = 3),
      p.signif = p_signif,
      method = method_name,
      stringsAsFactors = FALSE
    )
  }
  
  # For paired test - used when paired = TRUE
  run_paired_test <- function() {
    # Create paired data for analysis
    paired_data <- data %>%
      dplyr::select(!!rlang::sym(id_col), !!rlang::sym(group_var), !!rlang::sym(response_var)) %>%
      tidyr::pivot_wider(names_from = !!rlang::sym(group_var), values_from = !!rlang::sym(response_var))
    
    # Calculate paired differences for effect size
    diff_col <- paste0("diff_", group_levels[1], "_", group_levels[2])
    paired_data[[diff_col]] <- paired_data[[group_levels[1]]] - paired_data[[group_levels[2]]]
    
    # Skip if all differences are NA
    if (all(is.na(paired_data[[diff_col]]))) {
      warning("All paired differences are NA, cannot compute permutation test.")
      return(create_output(NA, NA, "Paired permutation test (failed)"))
    }
    
    # Calculate mean difference as test statistic
    obs_stat <- mean(paired_data[[diff_col]], na.rm = TRUE)
    
    # Prepare data for coin test (long format with complete pairs only)
    test_data_complete <- data %>%
      dplyr::group_by(!!rlang::sym(id_col)) %>%
      dplyr::filter(n() == 2) %>% # Keep only complete pairs
      dplyr::ungroup()
    
    # Skip if insufficient data
    if (nrow(test_data_complete) < 4) { # Need at least 2 pairs
      warning("Insufficient complete pairs for permutation test.")
      return(create_output(NA, NA, "Paired permutation test (insufficient data)"))
    }
    
    # Create formula for coin test
    f <- as.formula(paste(response_var, "~", group_var, "|", id_col))
    
    # Run permutation test using coin
    test_result <- tryCatch({
      # Use oneway_test from coin with paired structure
      test <- coin::oneway_test(
        formula = f,
        data = test_data_complete,
        distribution = coin::approximate(nresample = n_perm),
        alternative = "two.sided"
      )
      
      # Extract p-value as numeric value
      p_val <- as.numeric(coin::pvalue(test))
      
      if (is.na(p_val)) {
        warning("Failed to compute p-value in permutation test.")
        return(create_output(NA, NA, "Paired permutation test (failed)"))
      }
      
      return(create_output(p_val, obs_stat, "Paired permutation test"))
    }, 
    error = function(e) {
      warning("Error in paired permutation test: ", e$message)
      return(create_output(NA, NA, "Paired permutation test (error)"))
    })
    
    return(test_result)
  }
  
  # For unpaired test - used when paired = FALSE
  run_unpaired_test <- function() {
    # Create formula for coin test
    f <- as.formula(paste(response_var, "~", group_var))
    
    # Calculate observed mean difference as test statistic
    obs_stat <- mean(data[data[[group_var]] == group_levels[1], response_var], na.rm = TRUE) - 
      mean(data[data[[group_var]] == group_levels[2], response_var], na.rm = TRUE)
    
    # Run permutation test using coin
    test_result <- tryCatch({
      # Use oneway_test from coin for independent samples
      test <- coin::oneway_test(
        formula = f,
        data = data,
        distribution = coin::approximate(nresample = n_perm),
        alternative = "two.sided"
      )
      
      # Extract p-value as numeric value
      p_val <- as.numeric(coin::pvalue(test))
      
      if (is.na(p_val)) {
        warning("Failed to compute p-value in permutation test.")
        return(create_output(NA, NA, "Independent permutation test (failed)"))
      }
      
      return(create_output(p_val, obs_stat, "Independent permutation test"))
    }, 
    error = function(e) {
      warning("Error in independent permutation test: ", e$message)
      return(create_output(NA, NA, "Independent permutation test (error)"))
    })
    
    return(test_result)
  }
  
  # Run appropriate test based on paired parameter
  if (paired) {
    result <- run_paired_test()
  } else {
    # Currently the function is designed primarily for paired tests
    warning("Independent samples permutation test may not be fully integrated with add_stat_test_dodge.")
    result <- run_unpaired_test()
  }
  
  return(result)
}