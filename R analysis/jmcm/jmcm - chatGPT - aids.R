# Load necessary libraries
library(jmcm)
library(ggplot2)

# Load the aids dataset
data("aids", package = "jmcm")

# Inspect the structure of the dataset
str(aids)

# Fit the joint model using jmcm function
fit.mcd <- jmcm(cd4 | id | time ~ 1 | 1, 
                data = aids, 
                triple = c(1, 1, 1), 
                cov.method = 'hpc', 
                control = jmcmControl(trace = TRUE))

# Print summary of the model
summary(fit.mcd)

# Extract mean coefficients from the fitted model
mean_coefs <- fit.mcd@opt$beta

# Use the same aids data for prediction
X_aids <- model.matrix(~ time, data=aids)

# Compute predicted values using the extracted mean coefficients
predictions <- X_aids %*% mean_coefs

# Combine aids data with predictions for analysis
aids$predictions <- predictions

# Print the first few rows to check
head(aids)

# Plot the actual vs predicted values
ggplot(aids, aes(x=time, y=cd4, group=id)) +
  geom_line(alpha=0.3) +
  geom_point(aes(color="Actual"), alpha=0.3) +
  geom_line(aes(y=predictions, color="Predicted"), linetype="dashed") +
  labs(title="Actual vs Predicted Values",
       x="Time",
       y="CD4 Count",
       color="Legend") +
  theme_minimal()
