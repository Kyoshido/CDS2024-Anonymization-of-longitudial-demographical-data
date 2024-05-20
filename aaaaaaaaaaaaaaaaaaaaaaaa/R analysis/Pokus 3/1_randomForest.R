###############################################################################
###############################################################################
###############################################################################

# randomForest
# Did not work

###############################################################################

# Install and load necessary packages
# install.packages("randomForest")
# install.packages("data.table")
# install.packages("dplyr")
# install.packages("ggplot2")
# install.packages("NHANES")
library(randomForest)
library(data.table)
library(dplyr)
library(ggplot2)
library(NHANES)

# Define the number of time points and individuals
n_timepoints <- 5
n_individuals <- 200

# Create a longitudinal structure
set.seed(123)
ids <- 1:n_individuals
long_data <- data.table(
  id = rep(ids, each = n_timepoints),
  time = rep(1:n_timepoints, times = n_individuals)
)

# Load NHANES dataset
set.seed(123)
NHANES_sample <- NHANES[sample(nrow(NHANES), 1000), ]
NHANES_sample$Year <- sample(2000:2020, nrow(NHANES_sample), replace = TRUE)

# Select relevant columns and preprocess the data
demo_vars <- NHANES_sample %>% 
  select(Year, Age, Gender, Race1, Education, HHIncome)

# Convert categorical variables to factors
demo_vars$Gender <- as.factor(demo_vars$Gender)
demo_vars$Race1 <- as.factor(demo_vars$Race1)
demo_vars$Education <- as.factor(demo_vars$Education)
demo_vars$HHIncome <- as.factor(demo_vars$HHIncome)

# Handle missing values by removing them for simplicity
demo_vars <- demo_vars %>% na.omit()

# Ensure demographic data has unique IDs
demo_vars$id <- rep(1:n_individuals, each = nrow(demo_vars) %/% n_individuals + 1)[1:nrow(demo_vars)]

# Select one row per id to ensure uniqueness
demo_vars <- demo_vars %>% distinct(id, .keep_all = TRUE)

# Merge demographic data into longitudinal data by ID
long_data <- merge(long_data, demo_vars, by = "id", allow.cartesian = TRUE)

# Split data by time points
time_splits <- split(long_data, long_data$time)

# Train a random forest model for each time point
models <- lapply(time_splits, function(data) {
  randomForest(Age ~ Gender + Race1 + Education + HHIncome + Year, data = data, ntree = 100)
})

# Function to generate synthetic data for a single time point
generate_synthetic <- function(model, data) {
  new_data <- data
  new_data$Age <- predict(model, data)
  return(new_data)
}

# Generate synthetic data for each time point
synthetic_splits <- lapply(seq_along(models), function(i) {
  generate_synthetic(models[[i]], time_splits[[i]])
})

# Combine synthetic data into a single data.table
synthetic_data <- rbindlist(synthetic_splits)

# Compare original and synthetic data
summary(demo_vars)
summary(synthetic_data)

# Plotting to compare distributions

# Age distribution comparison
ggplot() +
  geom_density(data = demo_vars, aes(x = Age, color = "Original")) +
  geom_density(data = synthetic_data, aes(x = Age, color = "Synthetic")) +
  labs(title = "Age Distribution Comparison", x = "Age", y = "Density")

# Gender distribution comparison
ggplot() +
  geom_bar(data = demo_vars, aes(x = Gender, fill = "Original"), position = "dodge") +
  geom_bar(data = synthetic_data, aes(x = Gender, fill = "Synthetic"), position = "dodge", alpha = 0.7) +
  labs(title = "Gender Distribution Comparison", x = "Gender", y = "Count")

# Race distribution comparison
ggplot() +
  geom_bar(data = demo_vars, aes(x = Race1, fill = "Original"), position = "dodge") +
  geom_bar(data = synthetic_data, aes(x = Race1, fill = "Synthetic"), position = "dodge", alpha = 
             


###############################################################################
###############################################################################
###############################################################################