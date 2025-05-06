#---- calculate & plot t-half ----#

# find tHalf
df_tHalf <- data_StressRelax %>%
  group_by(exp) %>%
  filter(Load_norm < 0.5) %>%
  slice_min(Time, with_ties = FALSE) %>%
  ungroup()

Plot_tHalf <- plot_summary_points(
  data = df_tHalf,
  xvar = "Alginate",
  yvar = "Time",
  color = Color.Gels,
  fillvar = "Alginate",
  fontsize = FontSize
)