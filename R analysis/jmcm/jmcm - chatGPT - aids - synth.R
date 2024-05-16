# Load necessary libraries
library(jmcm)
library(ggplot2)

# Load the aids dataset
data("aids", package = "jmcm")

# Fit the joint model using jmcm function
fit.mcd <- jmcm(cd4 | id | time ~ 1 | 1,
                data = aids, 
                triple = c(1, 1, 1), 
                cov.method = 'mcd', 
                control = jmcmControl(trace = TRUE))

# Extract mean coefficients from the fitted model
mean_coefs <- fit.mcd@opt$beta

# Extract variance and covariance parameters
lambda <- fit.mcd@opt$lambda

# Number of synthetic subjects and time points per subject
unique_patients <- unique(aids$id)
n_synthetic <- length(unique_patients)  # Number of unique patients
obs_per_patient <- table(aids$id)       # Number of observations per patient

# Simulate synthetic data
set.seed(123)
patient <- rep(unique_patients, times=obs_per_patient)
time <- aids$time  # Use the same time structure as the original data

# Create design matrix for mean structure
X_synthetic <- model.matrix(~ time)

# Simulate CD4 counts based on the mean coefficients
mu <- X_synthetic %*% mean_coefs

# Simulate residuals using estimated variance parameters
sigma2 <- exp(lambda)  # Transform log-variance to variance
residuals <- rnorm(length(mu), mean=0, sd=sqrt(sigma2))

# Add residuals to the mean to get the synthetic CD4 counts
CD4_synthetic <- mu + residuals

# Create a data frame for the synthetic data
synthetic_data <- data.frame(patient=patient, time=time, CD4=CD4_synthetic)

# Ensure both data frames have the same columns
synthetic_data <- data.frame(
  time = synthetic_data$time,
  cd4 = synthetic_data$CD4,
  age = NA,
  packs = NA,
  drugs = NA,
  sex = NA,
  cesd = NA,
  id = synthetic_data$patient,
  dataset = "Synthetic"
)

# Add the dataset column to the original data
aids$dataset <- "Original"

# Combine the original and synthetic data for visualization
combined_data <- rbind(aids, synthetic_data)

# Plot the original and synthetic data
ggplot(combined_data, aes(x=time, y=cd4, group=interaction(id, dataset), color=dataset)) +
  geom_line(alpha=0.5) +
  geom_point(alpha=0.5) +
  labs(title="Comparison of Original and Synthetic Longitudinal Data",
       x="Time",
       y="CD4 Count",
       color="Dataset") +
  theme_minimal()
