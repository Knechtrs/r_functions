##########################################################################################################
#### 4. E-modulus analysis
##########################################################################################################
#---- define parameters ----#
GelHeight <- params$GelProperties$Height
Gel_r <- params$GelProperties$Radius
Emod_low <- params$Emod_low # lower end of E-Mod fit
Emod_high <- params$Emod_high # upper end of E-Mod fit

#---- calculate stress and Strain ----#
data_Emod <- df_data %>%
  group_by(exp) %>%
  mutate(max_time = Time[which.max(Load)]) %>%  # Get the time at max(load_BF)
  filter(Time < max_time) %>%  # Filter for time after max(load_BF) timepoint
  mutate(Disp= max(Disp) - Disp, # normalize Disp
         Strain=Disp/GelHeight) %>% # calculate Strain
  mutate(Stress=Load*9.81/((pi*Gel_r^2))) %>%
  ungroup()

#---- check how good linear fit and strain range fits ----#
# subset data for Strain range
data_fitRange <- data_Emod %>% 
  filter(Strain >= Emod_low & Strain <= Emod_high) 

# check if fit is in a linear region
plot_EmodFit <- ggplot() +
  geom_line(data = data_Emod , aes(x = Disp, y = Load)) +
  geom_smooth(data = data_fitRange, aes(x = Disp, y = Load), color="red", linewidth=1.2, method = "lm", se = FALSE) +
  facet_wrap(~ exp, scale="free_y") +
  theme_bw() +
  theme(strip.background = element_blank()) +
  labs(x="displacement (mm)", y= "load (g)") +
  ggtitle(paste0("E-modulus fit between ", Emod_low*100, "% -", Emod_high*100, "% strain")) +
  theme_fontsize(FontSize)

#---- Calculating E modulus -----#

# Perform modulus fit using nest() + map()
df_modulus <- data_fitRange %>%
  group_by(exp, Alginate) %>%
  nest() %>% #nest() allows storing models in a list-column while keeping other data columns
  mutate(model = map(data, ~ lm(Stress ~ Strain, data = .x))) %>%
  select(-data) %>%  # Remove raw data column to keep it clean
  mutate(Emod = map_dbl(model, ~ coef(summary(.x))[2, 1])) %>%  # use map_dbl to return numeric value
  ungroup()

# create plot with E-Modulus
Plot_Emod <- plot_summary_points(
  data = df_modulus,
  xvar = "Alginate",
  yvar= "Emod",
  fillvar= "Alginate",
  colors = Color.Gels,
  fontsize = FontSize
) +
  labs(y="Elastic modulus (kPa)") 
