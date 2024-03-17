library(ggplot2)
library(dplyr)

# Parameters
n_paths <- 500 # Total number of paths
n_outliers <- 10  # Number of outlier paths
n_steps <- 100   # Number of steps in each path
mu <- 6          # Drift term for outliers

# Function to generate non-regular time points
generate_time_points <- function(n) {
  sort(runif(n, 0, 1))
}

# Function to generate a single Brownian motion path with irregular time intervals
generate_bm_path <- function(id, mu = 0) {
  time <- generate_time_points(n_steps)
  # Use diff() to get the correct time intervals and adjust the mean and sd accordingly
  time_intervals <- c(time[1], diff(time))
  steps <- c(0, cumsum(rnorm(n_steps - 1, mean = mu * time_intervals, sd = sqrt(time_intervals))))
  type <- ifelse(mu == 0, 'Standard BM', 'Drift BM')
  data.frame(id = id, time = time, path = steps, type = type)
}

# Generate paths with irregular time intervals
set.seed(123)  # For reproducibility
paths_df <- do.call(rbind, lapply(1:n_paths, function(i) {
  mu_value <- ifelse(i <= n_paths - n_outliers, 0, mu)
  generate_bm_path(i, mu_value)
}))
print(paths_df)
# Calculate the maximum and minimum values among Standard BM paths
max_max_value <- paths_df %>%
  filter(type == 'Standard BM') %>%
  group_by(id) %>%
  summarise(max_path = max(path)) %>%
  summarise(max_max_value = max(max_path)) %>%
  pull(max_max_value)

min_min_value <- paths_df %>%
  filter(type == 'Standard BM') %>%
  group_by(id) %>%
  summarise(min_path = min(path)) %>%
  summarise(min_min_value = min(min_path)) %>%
  pull(min_min_value)

# Extract the id of the path with the maximum maximum value
max_path_id <- paths_df %>%
  filter(type == 'Standard BM') %>%
  group_by(id) %>%
  summarise(max_path = max(path)) %>%
  filter(max_path == max_max_value) %>%
  pull(id)

# Extract the id of the path with the minimum minimum value
min_path_id <- paths_df %>%
  filter(type == 'Standard BM') %>%
  group_by(id) %>%
  summarise(min_path = min(path)) %>%
  filter(min_path == min_min_value) %>%
  pull(id)

# Plotting
ggplot(paths_df) +
  geom_line(aes(x = time, y = path, group = id, color = type), alpha = 0.5) +
  geom_line(data = paths_df %>% filter(id == max_path_id), aes(x = time, y = path), color = "red", size = 1) +
  geom_line(data = paths_df %>% filter(id == min_path_id), aes(x = time, y = path), color = "red", size = 1) +
  scale_color_manual(values = c("Standard BM" = "blue", "Drift BM" = "brown")) +
  theme_minimal() +
  labs(title = "Standard vs Drift Brownian Motion with Bandwidth", x = "Time", y = "Path") +
  guides(color = guide_legend(title = "Path Type")) +
  theme(legend.position = "right") # Adjust legend position if needed