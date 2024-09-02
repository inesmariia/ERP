## Code below is the code used to preprocess all the data used in this project ##

# Install necessary packages
install.packages(c(readr, dplyr, stringr, tidyr, ggplot2, gridExtra))

# Load necessary libraries
library(readr) # For reading csv files
library(dplyr) # For data manipulation
library(ggplot2) # For data visualisations
library(stringr) # For string manipulation
library(gridExtra) # For data visualisations
library(tidyr) # For tidying data

# Set working directory
setwd("~/YourProjectFolder")

# Define paths to files
file_path1 <- "data/demographic.csv"
file_path2 <- "data/home_spirometry.csv"
file_path3 <- "data/hospital_spirometry.csv"

# Home Spirometry: Data Pre-processing & FVC measurements
# Read demographic and home spirometry data files
df1 <- read_csv(file_path1)
df2 <- read_csv(file_path2)

# Remove unnecessary columns from both datasets
df1 <- df1[, -((ncol(df1)-2):ncol(df1))]
df2 <- df2[, -((ncol(df2)-2):ncol(df2))]


# Cleaning transcription errors in demographic spirometry dataset
# Handle missing value in hospital_patient_id by assigning it with missing patient id
df1 <- df1 %>%
  mutate(hospital_patient_id = ifelse(is.na(hospital_patient_id), "LT01", hospital_patient_id))

# Extract the "LTXX" pattern from the user_email column and assign it to hospital_patient_id
df1$hospital_patient_id <- sub(".*(LT[0-9]{2}).*", "\\1", df1$user, ignore.case = TRUE)

# Correct specific hospital IDs (error in the dataset)
df1$hospital_patient_id[df1$hospital_patient_id == "LT28"] <- "LT27"

# Make sure all hospital IDs follow a consistent format
df1$hospital_patient_id <- sub("^lt([0-9]{2})$", "LT\\1", df1$hospital_patient_id, ignore.case = FALSE)

# Rename columns in the home spirometry dataset for clarity
df2 <- df2 %>%
  rename(
    id = `Id (Cms Patient)`,
    date = `Day of Created (Cms Patientmeasurement)`,
    FEV1 = Fev1,
    FVC = Fvc
  )

# Extract Month, Day, and Year components from the date
df2 <- df2 %>%
  mutate(
    Month = str_extract(date, "[A-Za-z]+"),
    Day = str_extract(date, "\\d{1,2}"),
    Year = str_extract(date, "\\d{4}")
  )

# Convert month name to month number
month_map <- c("January" = "01", "February" = "02", "March" = "03", "April" = "04", 
               "May" = "05", "June" = "06", "July" = "07", "August" = "08", 
               "September" = "09", "October" = "10", "November" = "11", "December" = "12")

# Map month name to month number and format the date
df2 <- df2 %>%
  mutate(
    Month = month_map[Month],
    # Format date as DD-MM-YY
    formatted_date = str_c(Day, Month, str_sub(Year, 3, 4), sep = "-")
  )

# Convert formatted date to Date type
df2 <- df2 %>%
  mutate(
    date = as.Date(formatted_date, format = "%d-%m-%y")
  )

# Use demographic data to match home spirometry measurements to patients
df2 <- df2 %>%
  left_join(df1 %>% select(id, hospital_patient_id), by = "id")

# Remove records without a valid patient ID
df2 <-df2 %>%
  filter(!is.na(hospital_patient_id))

# Get the highest FVC measurement each day for each patient
highest_measures <- df2 %>%
  group_by(hospital_patient_id, date) %>%
  summarize(
    max_FVC = max(FVC, na.rm = TRUE)
  ) %>%
  ungroup()

# Define the dataframe that we will work with focusing on home FVC measurements
home_FVC <- highest_measures %>%
  select(hospital_patient_id, date, max_FVC) %>%
  rename(ID = hospital_patient_id,
         homeFVC = max_FVC)

## Hospital Spirometry data: Data Pre-processing

# Read hospital spirometry data, skipping the column headers that need special handling
blf <- read_csv(file_path3, skip = 2, col_names = FALSE)

# Read the first two rows separately to allow for header manipulation 
header <- read_csv(file_path3, n_max = 2, col_names = FALSE)

# Convert the header to characters to avoid type mismatch issues
header <- header %>% mutate(across(everything(), as.character))

# Function to fill NA values with the previous valid value (handles merged cells in headers)
fill_na_with_previous <- function(x) {
  for(i in 2:length(x)) {
    if(is.na(x[i]) || x[i] == "") {
      x[i] <- x[i - 1]
    }
  }
  return(x)
}

# Apply the function to fill missing header values with the previous valid value
header[1,11:ncol(header)] <- fill_na_with_previous(header[1, 11:ncol(header)])

# Fill NA in the first row with an empty string
header <- header %>% 
  mutate_all(~replace_na(., ""))

# Combine header rows to create final column names
col_names <- paste(header[1, ], header[2, ], sep = "")

# Assign the combined column names to the dataset
names(blf) <- col_names

# Remove empty columns
blf <- blf %>%
  select_if(~!all(is.na(.)) & !all(. == ""))

# Remove last few rows (no patient data there)
blf <- blf[-c(35:39), ]

# Subset the data to include relevant columns for analysis
blf_subset <- blf %>%
  select(contains("ID"), contains("Enrolled"), contains("Baseline"), contains("FVC"),
         contains("FVCpredicted"), contains("DLCO"), contains("DLCOpredicted"))


# Update Baseline1 with the enrolled date where Baseline1 is NA and FVC1 is not NA
condition <- is.na(blf_subset$Baseline1) & !is.na(blf_subset$FVC1)

# Apply the condition to update Baseline1 with the Enrolled date
blf_subset[condition, "Baseline1"] <- blf_subset[condition, "Enrolled"]

# Reshape the hospital spirometry FVC data to look like home data
hospitalFVC <- blf_subset %>%
  pivot_longer(
    cols = starts_with("Baseline"),
    names_to = "timepoint",  # Temporarily name the column as 'timepoint'
    names_prefix = "Baseline",  # Remove the 'Baseline' prefix from the names
    values_to = "date"
  ) %>%
  pivot_longer(
    cols = starts_with("FVC"),
    names_to = "fvc_timepoint",
    names_prefix = "FVC", 
    values_to = "blfFVC"
  ) %>%
  filter(timepoint == fvc_timepoint) %>%
  select(ID, date, blfFVC) %>%
  mutate(date = gsub(" \\(o\\)| \\(t\\)", "", date)) %>%
  mutate(date = as.Date(date, format = "%d/%m/%Y")) %>%
  drop_na(date) %>%
  arrange(ID, date)


## Fitbit Activity data: Data Preproessing
# Set the main directory where all the Fitbitdata is stored
main_directory <- "/path/to/fitbitdata"

# List all files in the directory recursively
all_files <- list.files(main_directory, recursive = TRUE, full.names = TRUE)

# Separate intradaily from sleep summaries files
intradaily_files <- all_files[grepl("intradaily_output", all_files)]

# Function to extract patient ID from file path and read the CSV
read_csv_with_patient_id <- function(file_path) {
  # Extract patient ID from the file path (assuming folder structure includes "LTXX")
  patient_id <- basename(dirname(file_path))  # Extract the immediate folder name as the patient ID
  
  # Read the CSV file
  data <- readr::read_csv(file_path)
  
  # Add a new column for patient ID
  data$ID <- patient_id
  
  return(data)
}

# Read all intradaily data into a list of data frames
intradaily_data_list <- lapply(intradaily_files, read_csv_with_patient_id)

# Combine all intradaily data into a single data frame
combined_intradaily_data <- do.call(rbind, intradaily_data_list)

# Group the data by ID
grouped_intradaily_data <- combined_intradaily_data %>% group_by(ID)

# Intradaily Data Cleaning
# Rename columns
intradaily_data <- grouped_intradaily_data %>%
  rename(HR = `Heart rate`,
         SleepStage = `Sleep stage`)

# Subset the step data
steps_data <- intradaily_data %>%
  filter(!(is.na(Steps) | Steps == 0))

# Group steps by Day, calculate daily steps for each patient
daily_steps <- steps_data %>%
  mutate(Day = as.Date(Date)) %>%
  group_by(ID, Day) %>%
  summarise(DailySteps = sum(Steps, na.rm = TRUE)) %>%
  ungroup()

# Filter out patients with less than 4 days to prepare for smoothing
daily_steps <- daily_steps %>%
  group_by(ID) %>%
  filter(n() >=4)
