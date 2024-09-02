### DAILY STEPS: BEFORE AND AFTER SMOOTHING & OUTLIER DETECTION


##### BEFORE SMOOTHING
# Calculate the total number of days with step data for each patient
patient_days <- daily_steps %>%
  group_by(ID) %>%
  summarize(total_days = n()) %>%
  ungroup()

# Filter out patients with less than 60 days of data
sufficient_data_patients <- patient_days %>%
  filter(total_days >= 60) %>%
  select(ID)

# Filter daily step data to include patients with sufficient data
daily_steps <- daily_steps %>%
  filter(ID %in% sufficient_data_patients$ID)

# Calculate the average daily steps in the first month
first_month <-daily_steps %>%
  group_by(ID) %>%
  filter(Day <= min(Day) + 30) %>%
  summarize(first_month = mean(DailySteps, na.rm = TRUE)) %>%
  ungroup()

# Calculate the average daily steps in the last month
last_month <- daily_steps %>%
  group_by(ID) %>%
  filter(Day >= max(Day) - 30) %>%
  summarize(last_month = mean(DailySteps, na.rm = TRUE)) %>%
  ungroup()

# Combine the first month and last month average steps
month_steps <- first_month %>%
  inner_join(last_month, by = "ID")

# Calculate the percentage change in average daily steps
month_steps <- month_steps %>% 
  group_by(ID) %>%
  mutate(percentage_change_steps = ((last_month - first_month) / first_month) * 100)

# Combine with disease progression outcomes
percent_changes_month <- percent_changes %>%
  left_join(month_steps, by = "ID")

# Independent t-test on the percentage changes in daily steps between groups
t_test <- t.test(percentage_change_steps ~ group, data = percent_changes_month)
t_statistic <- round(t_test$statistic, 2)
degrees_freedom <- round(t_test$parameter,0)
p_value <- formatC(t_test$p.value, format = "e", digits = 2)

# Visualise the distribution of daily steps percentage change between groups
ggplot(percent_changes_month, aes(x = group, y = percentage_change_steps)) +
  geom_boxplot(aes(fill = group), alpha = 0.5, outlier.shape = NA) +  
  scale_y_continuous(breaks = seq(-90, 70, by = 10)) +
  stat_boxplot(geom = "errorbar") +
  geom_jitter(width = 0.15, size = 2, alpha = 0.6, color = "darkblue") +  
  scale_fill_manual(values = c("Progression" = "firebrick", "Stable" = "steelblue")) +
  labs(
    title = "Comparison of % change in daily steps (before smoothing)",
    subtitle = "Disease Progression vs. Stable",
    x = "Clinical Outcome",
    y = "Percentage change in average daily step count (%)",
    caption = paste("t(", degrees_freedom, ") = ", t_statistic, ", p-value = ", p_value, sep = "")
  ) +
  theme_minimal() +
  theme(
    panel.grid.minor = element_blank(),
    axis.line = element_line(color = "black"),
    legend.position = "none",  
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5),  
    plot.subtitle = element_text(size = 12, hjust = 0.5),
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 14)
  ) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray")

## OUTLIER DETECTION

# Initial smoothing for outlier identification
steps_with_outliers <- daily_steps %>%
  group_by(ID) %>%
  arrange(Day) %>%
  # Initial Loess smoothing
  mutate(smoothed_steps_initial = predict(loess(DailySteps ~ as.numeric(Day), span = 0.2))) %>%
  # Calculate residuals
  mutate(residuals = DailySteps - smoothed_steps_initial) %>%
  # Identify outliers
  mutate(is_outlier = abs(residuals) > 2 * sd(residuals)) %>%
  ungroup()

# Remove outliers
steps_cleaned <- steps_with_outliers %>%
  filter(!is_outlier)

# Apply smoothing again on cleaned data (no outliers)
steps_smoothed <- steps_cleaned %>%
  group_by(ID) %>%
  arrange(Day) %>%
  # Apply Loess smoothing again
  mutate(smoothed_steps = predict(loess(DailySteps ~ as.numeric(Day), span = 0.2))) %>%
  ungroup()

# Function to plot this process for a specific patient
plot_outlier_removal_process <- function(patient_id) {
  
  # Original data with initial smoothing and outliers
  plot1 <- ggplot(steps_with_outliers %>% filter(ID == patient_id), aes(x = Day)) +
    geom_line(aes(y = smoothed_steps_initial, linetype = "Loess Line"), color = "green", linewidth = 1) +  # Initial smoothed line
    geom_point(aes(y = DailySteps, color = ifelse(is_outlier, "Outliers", "Inliers")), alpha = 0.6) +  # Highlight outliers
    scale_color_manual(name = "Legend", values = c("Inliers" = "blue", "Outliers" = "red")) +
    scale_linetype_manual(name = "", values = c("Loess Line" = "solid"), labels = c("Loess Smoothing Line")) +
    labs(title = "Daily steps over time of a patient",
         x = "Date", y = "Daily Steps",
         color = "Legend") +
    theme_minimal() +
    theme(panel.grid.minor = element_blank(),
          axis.line = element_line(color = "black"),
          legend.position = "right",
          legend.box = "vertical")
  
  # Final smoothed data without outliers
  plot2 <- ggplot(steps_smoothed %>% filter(ID == patient_id), aes(x = Day)) +
    geom_point(aes(y = DailySteps, color = "Inliers"), alpha = 0.6) +  
    geom_line(aes(y = smoothed_steps, color = "Loess Line"), linewidth = 1) +  # Final smoothed line
    scale_color_manual(values = c("Inliers" = "blue", "Loess Line" = "green"),
                       labels = c("Inliers", "Loess Smoothing Line")) +
    labs(title = "Daily steps over time of the same patient (after outlier removal)",
         x = "Date", y = "Daily Steps",
         colour = "Legend") +
    theme_minimal() +
    theme(panel.grid.minor = element_blank(),
          axis.line = element_line(color = "black"),
          legend.position = "right",
          legend.box = "vertical")
  
  # Arrange plots in a grid for comparison
  p <- gridExtra::grid.arrange(plot1, plot2, ncol = 1)
  print(p)
  
}

# Get a list of unique patient IDs
patient_ids <- unique(steps_smoothed$ID)

# Loop over each patient and perform the analysis
for (patient_id in patient_ids) {
  cat("Analysing patient:", patient_id, "\n")
  plot_outlier_removal_process(patient_id)
}


### AFTER SMOOTHING
# Calculate the total number of days with step data for each patient
patient_days_count <- steps_smoothed %>%
  group_by(ID) %>%
  summarize(total_days = n()) %>%
  ungroup()

# Filter out patients with less than 60 days of data
sufficient_data_patients <- patient_days_count %>%
  filter(total_days >= 60) %>%
  select(ID)

# Filter the smoothed daily steps data to include only patients with sufficient data
steps_smoothed <- steps_smoothed %>%
  filter(ID %in% sufficient_data_patients$ID)

# Calculate the average daily steps in the first month
first_month_avg_steps <- steps_smoothed %>%
  group_by(ID) %>%
  filter(Day <= min(Day) + 30) %>% 
  summarize(first_avg_steps = mean(smoothed_steps, na.rm = TRUE)) %>%
  ungroup()

# Calculate the average daily steps in the last month
last_month_avg_steps <- steps_smoothed %>%
  group_by(ID) %>%
  filter(Day >= max(Day) - 30) %>% 
  summarize(last_avg_steps = mean(smoothed_steps, na.rm = TRUE)) %>%
  ungroup()

# Combine the first month and last month average steps
month_avg_steps <- first_month_avg_steps %>%
  inner_join(last_month_avg_steps, by = "ID")

# Calculate the percentage change in average daily steps
month_avg_steps <- month_avg_steps %>%
  group_by(ID) %>%
  mutate(percentage_change_steps = ((last_avg_steps - first_avg_steps) / first_avg_steps) * 100)

percent_changes_steps <- percent_changes %>%
  left_join(month_avg_steps, by = "ID")


# Independent t-test on the percentage changes in smoothed daily steps between groups
t_test <- t.test(percentage_change_steps ~ group, data = percent_changes_steps)
t_statistic <- round(t_test$statistic, 2)
degrees_freedom <- round(t_test$parameter,0)
p_value <- formatC(t_test$p.value, format = "e", digits = 2)

# Visualise the distribution of smoothed daily step percentage changes between groups
ggplot(percent_changes_steps, aes(x = group, y = percentage_change_steps)) +
  geom_boxplot(aes(fill = group), alpha = 0.5, outlier.shape = NA) +
  scale_y_continuous(breaks = seq(-90, 70, by = 10)) +
  stat_boxplot(geom = "errorbar") +
  geom_jitter(width = 0.15, size = 2, alpha = 0.6, color = "darkblue") +
  scale_fill_manual(values = c("Progression" = "firebrick", "Stable" = "steelblue")) +
  labs(
    title = "Comparison of % change in daily steps",
    subtitle = "Disease Progression vs. Stable",
    x = "Clinical Outcome",
    y = "Percentage change in daily step count averages (%)",
    caption = paste("t(", degrees_freedom, ") = ", t_statistic, ", p-value = ", p_value, sep = "")
  ) +
  theme_minimal() +
  theme(
    panel.grid.minor = element_blank(),
    axis.line = element_line(color = "black"),
    legend.position = "none", 
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
    plot.subtitle = element_text(size = 12, hjust = 0.5),
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 14)  
  ) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray") 

