#---- apply butterworth filter ----#

# Define the Butterworth filter parameters
order <- params$FilterParams$order  # Filter order
cutoff_freq <- params$FilterParams$cutoff  # Cutoff frequency (Hz)

# get only stress relax data
data_StressRelax <- df_data %>%
  group_by(exp) %>%
  mutate(max_time = Time[which.max(Load)]) %>%  # Get the time at max(load_BF)
  filter(Time > max_time) %>%  # Filter for time after max(load_BF) timepoint
  mutate(Time = Time - max_time) %>%  # Adjust time relative to max(load_BF)
  ungroup()

# quickly check data
print(Plotting_LoadTime(data_StressRelax, y2=NULL))

library(signal)

# Pad the signal at beginning and end to remove edge artifacts
pad_signal <- function(signal, pad_len = 100) {
  pad_start <- rep(signal[1], pad_len)
  pad_end <- rep(signal[length(signal)], pad_len)
  c(pad_start, signal, pad_end)
}

# Apply the filter with padding and trim the result
apply_butterworth <- function(signal, order, cutoff_freq, fs, pad_len = 100) {
  butter_filter <- butter(order, cutoff_freq, type = "low", fs = fs)
  padded_signal <- pad_signal(signal, pad_len)
  filtered <- filtfilt(butter_filter, padded_signal)
  filtered_trimmed <- filtered[(pad_len + 1):(length(filtered) - pad_len)]
  return(filtered_trimmed)
}

# Apply to data
data_StressRelax <- data_StressRelax %>%
  mutate(Load = as.double(Load)) %>%
  group_by(exp) %>%
  mutate(
    Load_BF = apply_butterworth(Load, order, cutoff_freq, sampling_rate)
  ) %>%
  ungroup()

detach("package:signal", unload = TRUE)

# library(signal)
# 
# df_data <- df_data %>%
#   mutate(Load_BF = filtfilt(butter(order, cutoff_freq, type = "low", fs = sampling_rate), Load)) # smooth data with butterworth filter
# 
# detach("package:signal", unload = TRUE)

#---- normalize to load ----#
data_StressRelax <- data_StressRelax %>%
  group_by(exp) %>%
  mutate(Load_norm = Load_BF / max(Load_BF, na.rm = TRUE)) %>%  # Normalize to load_BF
  ungroup()

#---- check data ----#

# quickly check data: if butterworth filter worked well
print(Plotting_LoadTime(data_StressRelax))
