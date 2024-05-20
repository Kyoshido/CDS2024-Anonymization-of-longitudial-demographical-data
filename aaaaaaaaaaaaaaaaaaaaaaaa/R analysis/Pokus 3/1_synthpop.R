###############################################################################
###############################################################################
###############################################################################

# synthpop
# So synthpop is not really ready for longitudinal data

###############################################################################

library(NHANES)
# Load the NHANES data
data(NHANES)

###############################################################################

# Create a sample NHANES subset with a 'Year' variable for demonstration
set.seed(123)
NHANES_sample <- NHANES[sample(nrow(NHANES), 1000), ]
NHANES_sample$Year <- sample(2000:2020, nrow(NHANES_sample), replace = TRUE)

# Select relevant columns
demo_vars <- NHANES_sample[, c("Year","Age", "Gender", "Race1", "Education", "MaritalStatus")]
head(demo_vars)

###############################################################################

# Generate synthetic data
# install.packages("synthpop")
library(synthpop)
synth_data <- syn(demo_vars)

# Extract the synthetic dataset
synthetic_dataset <- synth_data$syn
head(synthetic_dataset)

# Generate synthetic data
synth_data <- syn(demo_vars)

# Extract the synthetic dataset
synthetic_dataset <- synth_data$syn
head(synthetic_dataset)

# Compare original and synthetic data
summary(demo_vars)
summary(synthetic_dataset)

# Plotting to compare distributions
library(ggplot2)

# Age distribution comparison
ggplot() +
  geom_density(data = demo_vars, aes(x = Age, color = "Original")) +
  geom_density(data = synthetic_dataset, aes(x = Age, color = "Synthetic")) +
  labs(title = "Age Distribution Comparison", x = "Age", y = "Density")

# Gender distribution comparison
ggplot() +
  geom_bar(data = demo_vars, aes(x = Gender, fill = "Original"), position = "dodge") +
  geom_bar(data = synthetic_dataset, aes(x = Gender, fill = "Synthetic"), position = "dodge", alpha = 0.7) +
  labs(title = "Gender Distribution Comparison", x = "Gender", y = "Count")

###############################################################################

# Load necessary library
library(ggplot2)

# Function to plot age distribution by year
plot_age_by_year <- function(data, title) {
  ggplot(data, aes(x = Age, 
                   color = as.factor(Year))) +
    geom_density() +
    labs(title = title, x = "Age", 
         y = "Density") +
    theme(legend.position = "none")
}

# Plot for original data
plot_age_by_year(demo_vars, "Original Data: Age Distribution by Year")

# Plot for synthetic data
plot_age_by_year(synthetic_dataset, "Synthetic Data: Age Distribution by Year")


# Function to plot gender distribution by year
plot_gender_by_year <- function(data, title) {
  ggplot(data, aes(x = Year, fill = Gender)) +
    geom_bar(position = "fill") +
    labs(title = title, x = "Year", y = "Proportion") +
    theme(axis.text.x = element_text(angle = 90, hjust = 1))
}

# Plot for original data
plot_gender_by_year(demo_vars, "Original Data: Gender Distribution by Year")

# Plot for synthetic data
plot_gender_by_year(synthetic_dataset, "Synthetic Data: Gender Distribution by Year")


###############################################################################
###############################################################################
###############################################################################