create_summary_table <- function(data, columns) {
  if (!all(columns %in% names(data))) {
    stop("Some columns not found in data.")
  }
  
  data %>%
    summarise(
      across(
        all_of(columns),
        list(
          Mean = ~ mean(.x, na.rm = TRUE),
          SD   = ~ sd(.x, na.rm = TRUE)
        ),
        .names = "{.col}__{.fn}"
      )
    ) %>%
    pivot_longer(cols = everything(), names_to = "Variable", values_to = "Value") %>%
    separate(Variable, into = c("Variable", "Statistic"), sep = "__") %>%
    pivot_wider(names_from = Statistic, values_from = Value) %>%
    mutate(Mean_SD = sprintf("%.2f ± %.2f", Mean, SD))
}
