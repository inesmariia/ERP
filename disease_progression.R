### Disease Progression definition

# Calculate the percentage changes in FVCpredicted and DLCOpredicted
# Select relevant FVC predicted measurements
FVCpred <- blf %>%
  select(ID, `FVC predicted1`, `FVC predicted2`, `FVC predicted3`, `FVC predicted4`)

# Reshape the data from wide to long format
FVCpred_long <- FVCpred %>%
  pivot_longer(cols = starts_with("FVC predicted"), names_to = "Visit", values_to = "FVC_pred") %>%
  filter(!is.na(FVC_pred)) %>%
  arrange(ID, Visit)

# Calculate the percentage change from first to last available FVC predicted value
FVCpred_change <- FVCpred_long %>%
  group_by(ID) %>%
  summarise(
    FVCpred_change = (last(FVC_pred) - first(FVC_pred)) / first(FVC_pred) * 100
  )

# Select relevant DLCO predicted measurements
DLCOpred <- blf %>%
  select(ID, `DLCO predicted1`, `DLCO predicted2`, `DLCO predicted3`, `DLCO predicted4`)

# Reshape the data from wide to long format
DLCOpred_long <- DLCOpred %>%
  pivot_longer(cols = starts_with("DLCO predicted"), names_to = "Visit", values_to = "DLCO_pred") %>%
  filter(!is.na(DLCO_pred)) %>%  # Remove rows with NA values
  arrange(ID, Visit)  # Ensure data is sorted by ID and Visit

# Calculate the percentage change from first to last available DLCO predicted value
DLCOpred_change <- DLCOpred_long %>%
  group_by(ID) %>%
  summarise(
    DLCOpred_change = (last(DLCO_pred) - first(DLCO_pred)) / first(DLCO_pred) * 100
  )

# Find deceased patients
deceased <- blf %>%
  select(ID, Deceased)

# Merge the FVC and DLCO percentage changes with survival status
progression_data <- FVCpred_change %>%
  left_join(DLCOpred_change, by = "ID") %>%
  left_join(deceased, by = "ID")

# Classify patients into clinical outcomes
progression_data <- progression_data %>%
  mutate(clinical_outcome = case_when(
    FVCpred_change <= -10 & DLCOpred_change <= -10 ~ "Significant FVC and DLCO Drop",
    FVCpred_change <= -10 & DLCOpred_change > -10 ~ "Significant FVC Drop",
    FVCpred_change > -10 & DLCOpred_change <= -10 ~ "Significant DLCO Drop",
    TRUE ~ "No Significant Progression"
  )) %>%
  mutate(clinical_outcome = case_when(
    !is.na(Deceased) & clinical_outcome != "No Significant Progression" ~ paste(clinical_outcome, "(Death)"),
    !is.na(Deceased) & clinical_outcome == "No Significant Progression" ~ "Death",
    TRUE ~ clinical_outcome  # Retain the previous classification if not deceased
  ))


## Percentage Changes and Disease Progression

# Merge clinical outcomes with FVC percentage changes
percent_changes <- FVC_changes %>%
  left_join(progression_data %>% select(ID, clinical_outcome, FVCpred_change), by = "ID")


# Classify patients into two groups, progression or stable
percent_changes <- percent_changes %>%
  mutate(
    group = ifelse(
      clinical_outcome %in% c("No Significant Progression"),"Stable",
      "Progression"
    )
  )

