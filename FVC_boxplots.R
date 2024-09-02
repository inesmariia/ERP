## BOXPLOTS SPIROMETRY: GROUPED BY DISEASE PROGRESSION

# This script generates boxplots comparing the percentage change in FVC predicted 
# and percentage change in home FVC between patients grouped by disease progression

## Boxplot using change in FVC predicted

# Perform an independent t-test to compare the mean FVC predicted percentage change between 
# the progression and stable groups.

t_test <- t.test(FVCpred_change ~ group, data = percent_changes)
t_statistic <- round(t_test$statistic, 2)
degrees_freedom <- round(t_test$parameter,0)
p_value <- formatC(t_test$p.value, format = "e", digits = 2)

# Visualise the distribution of FVC predicted percentage change grouped by disease progression
ggplot(percent_changes, aes(x = group, y = FVCpred_change)) +
  geom_boxplot(aes(fill = group), alpha = 0.5, outlier.shape = NA) +
  stat_boxplot(geom = "errorbar") +
  geom_jitter(width = 0.15, size = 2, alpha = 0.6, color = "darkblue") +  
  scale_fill_manual(values = c("Progression" = "firebrick", "Stable" = "steelblue")) +
  labs(
    title = "Comparison of FVC predicted % change",
    subtitle = "Disease Progression vs. Stable",
    x = "Clinical Outcome",
    y = "Percentage Change in FVC Predicted (%)",
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


## Boxplot using smoothed home FVC % change

# Perform a t-test to compare the mean home FVC percentage change between the progression and stable groups.
t_test <- t.test(percent_smooth_change ~ group, data = percent_changes)
t_statistic <- round(t_test$statistic, 2)
degrees_freedom <- round(t_test$parameter,0)
p_value <- formatC(t_test$p.value, format = "e", digits = 2)

# Visualise the distribution of home FVC percentage change grouped by disease progression status.
ggplot(percent_changes, aes(x = group, y = percent_smooth_change)) +
  geom_boxplot(aes(fill = group), alpha = 0.5, outlier.shape = NA) +
  scale_y_continuous(breaks = seq(-50, 30, by = 10)) +
  stat_boxplot(geom = "errorbar") +
  geom_jitter(width = 0.15, size = 2, alpha = 0.6, color = "darkblue") +
  scale_fill_manual(values = c("Progression" = "firebrick", "Stable" = "steelblue")) +
  labs(
    title = "Comparison of home FVC % change",
    subtitle = "Disease Progression vs. Stable",
    x = "Clinical Outcome",
    y = "Percentage Change Home FVC (%)",
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
  ) 


