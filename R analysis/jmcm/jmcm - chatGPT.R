# Load necessary libraries
library(jmcm)
library(ggplot2)

# Simulate some longitudinal data
set.seed(123)
n <- 100  # number of subjects
t <- 5    # number of time points
beta <- c(1, 0.5)  # mean structure coefficients
sigma2 <- 0.5      # variance
phi <- 0.1         # correlation parameter

# Create design matrix for mean structure
time <- rep(1:t, each=n)
subject <- rep(1:n, times=t)
y <- 1 + 0.5 * time + rnorm(n*t, sd=sqrt(sigma2))

# Convert to data frame
data <- data.frame(subject=subject, time=time, y=y)

# Fit the joint model using jmcm function
fit.mcd <- jmcm(y | subject | time ~ 1 | 1,
                data = data, 
                triple = c(1, 1, 1), 
                cov.method = 'mcd', 
                control = jmcmControl(trace = TRUE))

# Print summary of the model
summary(fit.mcd)

# Extract mean coefficients from the fitted model
mean_coefs <- fit.mcd@opt$beta

# Simulate some new data points for prediction
new_subjects <- 5  # number of new subjects
new_times <- 5     # number of new time points

new_data <- data.frame(subject=rep(1:new_subjects, each=new_times), 
                       time=rep(1:new_times, new_subjects))

# Create design matrix for new data
X_new <- model.matrix(~ time, data=new_data)

# Compute predicted values using the extracted mean coefficients
predictions <- X_new %*% mean_coefs

# Print the predictions
print(predictions)

# Combine new data with predictions for plotting
new_data$predictions <- predictions

# Plot the predicted values for the new data points
ggplot(new_data, aes(x=time, y=predictions, group=subject, color=as.factor(subject))) +
  geom_line() +
  geom_point() +
  labs(title="Predicted Values for New Data Points",
       x="Time",
       y="Predicted Values") +
  theme_minimal()
