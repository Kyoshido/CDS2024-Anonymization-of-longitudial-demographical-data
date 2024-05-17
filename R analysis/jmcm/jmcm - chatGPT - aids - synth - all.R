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
cd4_synthetic <- mu + residuals

# Simulate other covariates based on the distributions in the original data
age <- sample(aids$age, length(cd4_synthetic), replace = TRUE)
packs <- sample(aids$packs, length(cd4_synthetic), replace = TRUE)
drugs <- sample(aids$drugs, length(cd4_synthetic), replace = TRUE)
sex <- sample(aids$sex, length(cd4_synthetic), replace = TRUE)
cesd <- sample(aids$cesd, length(cd4_synthetic), replace = TRUE)

# Create a data frame for the synthetic data
synthetic_data <- data.frame(
   time = time,
   cd4 = cd4_synthetic,
   age = age,
   packs = packs,
   drugs = drugs,
   sex = sex,
   cesd = cesd,
   id = patient,
   dataset = "Synthetic"
)

# Add the dataset column to the original data
aids$dataset <- "Original"

# Combine the original and synthetic data for visualization
combined_data <- rbind(aids, synthetic_data)

# Plot the original and synthetic data for CD4 counts
var <- "cd4"
ggplot(combined_data, aes(x=time,
                          y=var,
                          group=interaction(id, 
                                            dataset), 
                          color=dataset)) +
   geom_line(alpha=0.5) +
   geom_point(alpha=0.5) +
   labs(title="Comparison of Original and Synthetic CD4 Counts",
        x="Time",
        y="CD4 Count",
        color="Dataset") +
   theme_minimal()

# Plot distributions of other covariates
plot_covariate_distribution <- function(variable, var_name) {
   ggplot(combined_data, aes_string(x=variable, fill="dataset")) +
      geom_histogram(alpha=0.5, position="identity", bins=30) +
      labs(title=paste("Distribution of", var_name),
           x=var_name,
           y="Frequency",
           fill="Dataset") +
      theme_minimal()
}

# Age distribution
plot_covariate_distribution("age", "Age")

# Packs distribution
plot_covariate_distribution("packs", "Packs")

# Drugs distribution
plot_covariate_distribution("drugs", "Drugs")

# Sex distribution
plot_covariate_distribution("sex", "Sex")

# CESD distribution
plot_covariate_distribution("cesd", "CESD")

