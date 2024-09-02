## ANALYSIS

# FVC measurements over time: both home and hospital spirometry

# Filter home FVC data to include only patients with at least 4 data points
home_FVC <- home_FVC %>% group_by(ID) %>% filter(n() >= 4)

# Function to perform the analysis for each patient
analyse_patient <- function(patient_id) {
  # Filter data for the current patient
  home_patient_data <- home_FVC %>% filter(ID == patient_id)
  hospital_patient_data <- hospitalFVC %>% filter(ID == patient_id)
  
  # Apply spline smoothing to the homeFVC data
  spline_fit <- smooth.spline(as.numeric(home_patient_data$date), home_patient_data$homeFVC)
  home_patient_data$smoothed_homeFVC <- predict(spline_fit, as.numeric(home_patient_data$date))$y
  
  # Create a plot for the patient
  p <- ggplot() +
    geom_line(data = home_patient_data, aes(x = date, y = smoothed_homeFVC), color = "darkgreen", linewidth = 1.2) +
    geom_point(data = home_patient_data, aes(x = date, y = homeFVC), color = "blue", size = 2) +
    geom_point(data = hospital_patient_data, aes(x = date, y = blfFVC), color = "red", size = 3) +
    geom_line(data = hospital_patient_data, aes(x = date, y = blfFVC), color = "firebrick", linetype = "dashed", size = 1) +
    labs(title = "Analysis of FVC measurements for a patient",
         x = "Date",
         y = "FVC (Litres)",
         caption = "Blue: Home FVC (raw), Green: Home FVC (smoothed), Red: Hospital FVC") +
    theme_minimal() +
    theme(
      panel.grid.major = element_blank(),  
      panel.grid.minor = element_blank(),  
      panel.background = element_blank(),
      axis.line = element_line(color = "black")
    )
  
  # Visualise plot
  print(p)
  
  # Return the spline fit object (if needed for further analysis)
  return(spline_fit)
}
# Get a list of unique patient IDs
patient_ids <- unique(home_FVC$ID)

# Loop over each patient and perform the analysis
for (patient_id in patient_ids) {
  cat("Analysing patient:", patient_id, "\n")
  analyse_patient(patient_id)
}

### HOME FVC: spline smoothing

# Apply spline smoothing to each patient's home FVC data
home_FVC_smoothed <- home_FVC %>%
  group_by(ID) %>%
  arrange(date) %>%
  mutate(smoothed_FVC = predict(smooth.spline(as.numeric(date), homeFVC), as.numeric(date))$y) %>%
  ungroup()

# Calculate the average smoothed FVC for the first week of the study
first_week_smooth <- home_FVC_smoothed %>%
  group_by(ID) %>%
  mutate(first_date = min(date)) %>%  # Get the earliest date for each patient
  filter(date < first_date + 7) %>%  # Keep only data within the first 7 days
  ungroup()

first_week_smooth <- first_week_smooth %>%
  group_by(ID, first_date) %>%
  summarise(first_avg_smooth = mean(smoothed_FVC, na.rm = TRUE))

# Calculate the average smoothed FVC for the first week of the study
last_week_smooth <- home_FVC_smoothed %>%
  group_by(ID) %>%
  mutate(last_date = max(date)) %>%  # Get the latest date for each patient
  filter(date > last_date - 7) %>%  # Keep only data within the last 7 days
  ungroup()

last_week_smooth <- last_week_smooth %>%
  group_by(ID, last_date) %>%
  summarise(last_avg_smooth = mean(smoothed_FVC, na.rm = TRUE))

### FVC analyses: Correlations
## At the start of the study:

# Correlation between hospital first visit FVC (baseline1 or 2) and first week home FVC
# Select hospital FVC at Baseline1; if there is no Baseline1, take value at "second" visit
hospital0 <- blf_subset %>%
  mutate(
    Baseline1 = if_else(is.na(Baseline1), Baseline2, Baseline1),
    FVC1 = if_else(is.na(FVC1), FVC2, FVC1)
  ) %>%
  select(ID, Baseline1, FVC1) %>%
  mutate(Baseline1 = gsub(" \\(o\\)| \\(t\\)", "", Baseline1)) %>%
  mutate(Baseline1 = as.Date(Baseline1, format = "%d/%m/%Y")) %>%
  arrange(ID)

# Merge start of the study FVC data
start_study <- first_week_smooth %>%
  left_join(hospital0 %>% select(ID, Baseline1, FVC1), by = "ID") %>%  # Select only ID and FVC1 from baseline1
  rename(baselineFVC = FVC1) 

# Pearson correlation test between hospital and home FVC at start of the study
correlation_test <- cor.test(start_study$baselineFVC, start_study$first_avg_smooth)

# Visualise the correlation between the baseline hospital FVC measurements
# and the average of the first week's home FVC measurements.
ggplot(start_study, aes(x = baselineFVC, y = first_avg_smooth)) +
  geom_point(color = "blue", size = 3) +  # Scatter plot points
  geom_smooth(method = "lm", se = TRUE, linetype = "dashed", color = "darkgreen") +
  labs(title = "Comparison of Hospital and Home FVC at start of the study",
       x = "Hospital FVC (L)",
       y = "Home FVC (L)",
       caption = paste("Green: linear regression line\nr =", round(correlation_test$estimate, 2), ", p-value =", format.pval(correlation_test$p.value, digits = 2))
  ) +
  theme_minimal()  +
  theme(
    panel.grid.minor = element_blank(), # Remove minor grid lines
    axis.line = element_line(color = "black"),
    legend.position = "none")

# Bland-Altman Plot at the start of the study: Comparison of the two measurement techniques

# Create a Bland-Altman plot to compare the agreement between the hospital FVC measurements
# and the home FVC measurements for the first week.
bland_altman_start <- start_study %>%
  mutate(
    Difference = first_avg_smooth - baselineFVC,  # Calculate the difference between Hospital and Home FVC
    Average = (first_avg_smooth + baselineFVC) / 2  # Calculate the average of Hospital and Home FVC
  )

# Calculate mean difference and limits of agreement
mean_diff <- mean(bland_altman_start$Difference, na.rm = TRUE)
sd_diff <- sd(bland_altman_start$Difference, na.rm = TRUE)
upper_limit <- mean_diff + 1.96 * sd_diff
lower_limit <- mean_diff - 1.96 * sd_diff


# Create the Bland-Altman plot to assess agreement between home and hospital FVC measurements
ggplot(bland_altman_start, aes(x = Average, y = Difference, label = ID)) +
  geom_point(color = "blue", size = 2) +
  geom_hline(yintercept = mean_diff, color = "red", linetype = "dotted") +
  geom_hline(yintercept = upper_limit, color = "seagreen", linetype = "dashed") +
  geom_hline(yintercept = lower_limit, color = "seagreen", linetype = "dashed") +
  labs(title = "Bland-Altman for Home and Hospital FVC at start of the study",
       x = "Average of Home and Hospital FVC (L)",
       y = "Difference (Hospital - Home) (L)",
       caption = "Green: 95% limits of agreement; Red: mean difference") +
  theme_minimal() +
  theme(
    panel.grid.minor = element_blank(),
    axis.line = element_line(color = "black"),
    legend.position = "right") +
  annotate("text", x = min(bland_altman_start$Average, na.rm = TRUE), y = mean_diff, 
           label = round(mean_diff, 2), 
           vjust = -1, color = "red") +
  annotate("text", x = min(bland_altman_start$Average, na.rm = TRUE), y = upper_limit, 
           label = round(upper_limit, 2), 
           vjust = -1, color = "seagreen") +
  annotate("text", x = min(bland_altman_start$Average, na.rm = TRUE), y = lower_limit, 
           label = round(lower_limit, 2), 
           vjust = 1.5, color = "seagreen")


## At the end of the study:

# Correlation between last hospital visit and last week home (mean)
# This part repeats the correlation and Bland-Altman analysis for the end of the study.

# Replace fourth visit FVC with third visit FVC if fourth is NA
hospital4 <- blf_subset %>%
  mutate(
    Baseline4 = if_else(is.na(Baseline4), Baseline3, Baseline4),
    FVC4 = if_else(is.na(FVC4), FVC3, FVC4)
  ) %>%
  select(ID, Baseline4, FVC4) %>%
  mutate(Baseline4 = as.Date(Baseline4, format = "%d/%m/%Y")) %>%
  arrange(ID)

# Merge home data with hospital data at the end of the study
end_study <- last_week_smooth %>%
  left_join(hospital4 %>% select(ID, Baseline4, FVC4), by = "ID") %>%  # Select only ID and FVC1 from baseline1
  rename(baseline4FVC = FVC4) 

# Perform a Pearson correlation test between the last hospital FVC and the last week's average home FVC
correlation_test <- cor.test(end_study$baseline4FVC, end_study$last_avg_smooth)

# Correlation plot of home and hospital FVC
ggplot(end_study, aes(x = baseline4FVC, y = last_avg_smooth)) +
  geom_point(color="blue", size = 3) +  # Scatter plot points
  geom_smooth(method = "lm", se = TRUE, linetype = "dashed", color = "darkgreen") +  # Add a linear regression line without confidence interval
  labs(title = "Comparison of Hospital and Home FVC at end of the study",
       x = "Hospital FVC (L)",
       y = "Home FVC (L)",
       caption = paste("Green: linear regression line\nr =", round(correlation_test$estimate, 2), ", p-value =", format.pval(correlation_test$p.value, digits = 2))
  ) +  
  theme_minimal() +
  theme(
    panel.grid.minor = element_blank(), # Remove minor grid lines
    axis.line = element_line(color = "black"),
    legend.position = "none")

# Bland Altman Plot for the end of the study: Comparison of the two measurement techniques

# Repeat Bland-Altman analysis for the end of the study.
bland_altman_end <- end_study %>%
  mutate(
    Difference = last_avg_smooth - baseline4FVC,  # Calculate the difference between Hospital and Home FVC
    Average = (last_avg_smooth + baseline4FVC) / 2  # Calculate the average of Hospital and Home FVC
  )

# Calculate mean difference and limits of agreement
mean_diff <- mean(bland_altman_end$Difference, na.rm = TRUE)
sd_diff <- sd(bland_altman_end$Difference, na.rm = TRUE)
upper_limit <- mean_diff + 1.96 * sd_diff
lower_limit <- mean_diff - 1.96 * sd_diff

# Create the Bland-Altman plot
ggplot(bland_altman_end, aes(x = Average, y = Difference)) +
  geom_point(color = "blue", size = 2) +
  geom_hline(yintercept = mean_diff, color = "red", linetype = "dotted") +
  geom_hline(yintercept = upper_limit, color = "seagreen", linetype = "dashed") +
  geom_hline(yintercept = lower_limit, color = "seagreen", linetype = "dashed") +
  labs(title = "Bland-Altman for Home and Hospital FVC at end of the study",
       x = "Average of Home and Hospital FVC (L)",
       y = "Difference (Hospital - Home) (L)",
       caption = "Green: limits of agreement; Red: mean difference") +
  theme_minimal() +
  theme(
    panel.grid.minor = element_blank(),
    axis.line = element_line(color = "black"),
    legend.position = "right") +
  annotate("text", x = min(bland_altman_end$Average, na.rm = TRUE), y = mean_diff, 
           label = round(mean_diff, 2), 
           vjust = 1.5, color = "red") +
  annotate("text", x = min(bland_altman_end$Average, na.rm = TRUE), y = upper_limit, 
           label = round(upper_limit, 2), 
           vjust = -1, color = "seagreen") +
  annotate("text", x = min(bland_altman_end$Average, na.rm = TRUE), y = lower_limit, 
           label = round(lower_limit, 2), 
           vjust = 1.5, color = "seagreen")


## Percentage changes in FVC between the start and end of the study, relative to the start
smoothed_change <- first_week_smooth %>%
  left_join(last_week_smooth, by = "ID")

# Calculate the percentage change in smoothed FVC from the first to the last week of the study
smoothed_change <- smoothed_change %>%
  group_by(ID) %>%
  mutate(percent_smooth_change = (last_avg_smooth - first_avg_smooth) *100 /(first_avg_smooth))

# Calculate the percentage change in hospital FVC between the first and last clinic visit 
FVC_changes <- hospital0 %>%
  select(ID, baseline1FVC = FVC1) %>%
  left_join(hospital4 %>% select(ID, baseline4FVC = FVC4), by = "ID") %>%
  mutate(
    hospitalFVC_change = ((baseline4FVC - baseline1FVC) / baseline1FVC) * 100
  ) 

# Combine the percentage changes from home and hospital FVC measurements
FVC_changes <- FVC_changes %>%
  left_join(smoothed_change, by = "ID")

# Perform a correlation test between the percentage changes in hospital FVC and home FVC
correlation_test <- cor.test(FVC_changes$hospitalFVC_change, FVC_changes$percent_smooth_change)

# Correlation plot of percentage changes
ggplot(FVC_changes, aes(x = hospitalFVC_change, y = percent_smooth_change)) +
  geom_point(size = 2, color = "blue") + 
  scale_x_continuous(breaks = seq(-40, 60, by = 20)) +
  scale_y_continuous(breaks = seq(-40, 60, by = 20)) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") + 
  geom_smooth(method = "lm", se = TRUE, color = "green") +  
  labs(title = "Changes (%) in Hospital FVC vs. Changes (%) in Home FVC",
       subtitle = "between start and end of study",
       x = "Percentage Change in Hospital FVC (%)",
       y = "Percentage Change in Home FVC (%)",
       caption = paste("Red: line of equality; Green: Linear regression\nr =", round(correlation_test$estimate, 2), ", p-value =", formatC(correlation_test$p.value, format = "e", digits = 2))
  ) +
  theme_minimal() +
  theme(
    panel.grid.minor = element_blank(),
    axis.line = element_line(color = "black"),
    legend.position = "right")


###### DIFFERENCE IN DATES BETWEEN READINGS 

## AT THE START
## Calculate the time difference between home and hospital FVC at the start
start_study2 <- start_study %>%
  mutate(
    date_diff = as.numeric(first_date - Baseline1),  #  Difference in dates
    FVC_diff_percent = ((first_avg_smooth - baselineFVC) / baselineFVC) * 100  # Percentage difference in FVC
  )

# Perform a correlation test between date difference and percentage FVC difference
correlation_test <- cor.test(start_study2$date_diff, start_study2$FVC_diff_percent)

# Plot the difference in dates against the percentage difference in FVC
ggplot(start_study2, aes(x = date_diff, y = FVC_diff_percent)) +
  geom_point(size = 3, color= "blue") +  # Scatter plot points
  geom_smooth(method = "lm", se = TRUE, linetype = "dashed", color = "darkgreen") + 
  labs(title = "Difference in Dates vs. Difference in FVC (%) at start of the study",
       x = "Difference in Dates (days)",
       y = "Percentage Difference in FVC (%)",
       caption = paste("Green: linear regression line\nr =", round(correlation_test$estimate, 3), ", p-value =", formatC(correlation_test$p.value, format = "e",digits = 2))
  ) +  
  theme_minimal() +
  theme(
    panel.grid.minor = element_blank(), 
    axis.line = element_line(color = "black"),
    legend.position = "right")

## Time difference between home and hospital FVC readings at end
end_study2 <- end_study %>%
  mutate(
    date_diff = as.numeric(last_date - Baseline4),
    FVC_diff_percent = ((last_avg_smooth - baseline4FVC) / baseline4FVC) * 100  
  )

# Perform a Pearson correlation test between date difference and percentage difference in FVC
correlation_test <- cor.test(end_study2$date_diff, end_study2$FVC_diff_percent)

# Plot the difference in dates against the percentage difference in FVC
ggplot(end_study2, aes(x = date_diff, y = FVC_diff_percent)) +
  geom_point(size = 3, color = "blue") +  # Scatter plot points
  geom_smooth(method = "lm", se = TRUE, linetype = "dashed", color = "darkgreen") + 
  labs(title = "Difference in Dates vs. Difference in FVC (%) at end of the study",
       x = "Difference in Dates (days)",
       y = "Percentage Difference in FVC (%)",
       caption = paste("Green: linear regression line\nr =", round(correlation_test$estimate, 3), ", p-value =", formatC(correlation_test$p.value, format = "e",digits = 2))
  ) +
  theme_minimal() +
  theme(
    panel.grid.minor = element_blank(),
    axis.line = element_line(color = "black"),
    legend.position = "right")


#####  Rates of average daily change

# Calculate the first and last smoothed FVC values and the corresponding dates
home_FVC_diffs <- first_week_smooth %>%
  left_join(last_week_smooth, by = "ID") %>%
  group_by(ID) %>%
  summarize(
    FVC_diff = last_avg_smooth - first_avg_smooth,
    date_diff_days = as.numeric(last_date - first_date)
  ) %>%
  ungroup()

# Hospital changes
# Calculate the first and last FVC values and the corresponding dates
hospital_FVC_data <- hospital0 %>%
  select(ID, Baseline1, FVC1) %>%  # Select the necessary columns
  rename(first_hospital_date = Baseline1, first_hospital_FVC = FVC1) %>%
  left_join(
    hospital4 %>%
      select(ID, Baseline4, FVC4) %>%  # Select the necessary columns
      rename(last_hospital_date = Baseline4, last_hospital_FVC = FVC4),
    by = "ID"
  )

# Calculate the FVC change and the difference in days
hospital_FVC_diffs <- hospital_FVC_data %>%
  mutate(
    FVC_diff = last_hospital_FVC - first_hospital_FVC,
    date_diff_days = as.numeric(last_hospital_date - first_hospital_date)
  ) %>%
  filter(!is.na(last_hospital_date))

## Average change in FVC per day over the course of the study

# Calculate the average change in FVC per day for both hospital and home measurements

# For hospital data
hospital_FVC_diffs <- hospital_FVC_diffs %>%
  mutate(
    hospital_avg_change = FVC_diff / date_diff_days
  )

# For home data
home_FVC_diffs <- home_FVC_diffs %>%
  mutate(
    home_avg_change = FVC_diff / date_diff_days
  )

# Merge the average changes from home and hospital data
avg_changes <- home_FVC_diffs %>%
  left_join(hospital_FVC_diffs %>% select(ID, hospital_avg_change), by = "ID")


# Perform a Pearson correlation test to assess the relationship between the average daily changes in hospital and home FVC
correlation_test <- cor.test(avg_changes$hospital_avg_change, avg_changes$home_avg_change)

# Plot the rates of change in hospital FVC vs. home FVC
ggplot(avg_changes, aes(x = hospital_avg_change, y = home_avg_change)) +
  geom_point(size = 2, color = "blue") +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
  labs(
    title = "Average change per day in Hospital FVC vs. Home FVC",
    x = "Average Change in Hospital FVC (L/day)",
    y = "Average Change in Home FVC (L/day)",
    caption = paste("Red: line of equality\nr =", round(correlation_test$estimate, 2), "p-value =", formatC(correlation_test$p.value, format = "e", digits = 2))
  ) +
  theme_minimal() +
  theme(panel.grid.minor = element_blank(),
        axis.line = element_line(color = "black")) +
  coord_cartesian(xlim = c(-0.0025, 0.0025), ylim = c(-0.010, 0.010))

