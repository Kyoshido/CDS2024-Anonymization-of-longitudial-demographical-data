###############################################################################
###############################################################################
###############################################################################

# GAN
# Did not work

###############################################################################

library(keras)
library(reticulate)
library(NHANES)
library(dplyr)
library(ggplot2)

# install_keras()

# Load NHANES dataset
set.seed(123)
NHANES_sample <- NHANES[sample(nrow(NHANES), 1000), ]
NHANES_sample$Year <- sample(2000:2020, nrow(NHANES_sample), replace = TRUE)

# Select relevant columns and preprocess the data
demo_vars <- NHANES_sample %>% 
  select(Year, Age, Gender, Race1, Education, HHIncome)

# Convert categorical variables to numeric
demo_vars$Gender <- as.numeric(as.factor(demo_vars$Gender))
demo_vars$Race1 <- as.numeric(as.factor(demo_vars$Race1))
demo_vars$Education <- as.numeric(as.factor(demo_vars$Education))
demo_vars$HHIncome <- as.numeric(as.factor(demo_vars$HHIncome))

# Scale the data
demo_vars_scaled <- scale(demo_vars)

###############################################################################

# Define the generator model
generator <- keras_model_sequential() %>%
  layer_dense(units = 128, input_shape = 10) %>%
  layer_activation_leaky_relu(alpha = 0.2) %>%
  layer_dense(units = 256) %>%
  layer_activation_leaky_relu(alpha = 0.2) %>%
  layer_dense(units = 512) %>%
  layer_activation_leaky_relu(alpha = 0.2) %>%
  layer_dense(units = ncol(demo_vars_scaled), activation = "tanh")

# Define the discriminator model
discriminator <- keras_model_sequential() %>%
  layer_dense(units = 512, input_shape = ncol(demo_vars_scaled)) %>%
  layer_activation_leaky_relu(alpha = 0.2) %>%
  layer_dense(units = 256) %>%
  layer_activation_leaky_relu(alpha = 0.2) %>%
  layer_dense(units = 1, activation = "sigmoid")

discriminator %>% compile(
  optimizer = optimizer_adam(learning_rate = 0.0002, beta_1 = 0.5),
  loss = "binary_crossentropy",
  metrics = "accuracy"
)

# Define the combined GAN model
discriminator$trainable <- FALSE

gan_input <- layer_input(shape = 10)
gan_output <- discriminator(generator(gan_input))

gan <- keras_model(gan_input, gan_output)

gan %>% compile(
  optimizer = optimizer_adam(learning_rate = 0.0002, beta_1 = 0.5),
  loss = "binary_crossentropy"
)

# Train the GAN model
epochs <- 10000
batch_size <- 128

for (epoch in 1:epochs) {
  # Generate random noise
  noise <- matrix(runif(batch_size * 10, -1, 1), nrow = batch_size, ncol = 10)
  
  # Generate synthetic data
  generated_data <- generator %>% predict(noise)
  
  # Get a random batch of real data
  idx <- sample(1:nrow(demo_vars_scaled), batch_size)
  real_data <- demo_vars_scaled[idx, ]
  
  # Combine real and synthetic data
  combined_data <- rbind(generated_data, real_data)
  labels <- c(rep(0, batch_size), rep(1, batch_size))
  
  # Train the discriminator
  d_loss <- discriminator %>% train_on_batch(combined_data, labels)
  
  # Train the generator via the GAN model
  noise <- matrix(runif(batch_size * 10, -1, 1), nrow = batch_size, ncol = 10)
  misleading_labels <- rep(1, batch_size)
  g_loss <- gan %>% train_on_batch(noise, misleading_labels)
  
  if (epoch %% 1000 == 0) {
    cat("Epoch:", epoch, "D loss:", d_loss, "G loss:", g_loss, "\n")
  }
}

###############################################################################

# Generate synthetic data
num_synthetic <- nrow(demo_vars)
noise <- matrix(runif(num_synthetic * 10, -1, 1), nrow = num_synthetic, ncol = 10)
synthetic_data <- generator %>% predict(noise)

# Rescale the synthetic data back to original scale
synthetic_data <- sweep(synthetic_data, 2, attr(demo_vars_scaled, "scaled:scale"), "*")
synthetic_data <- sweep(synthetic_data, 2, attr(demo_vars_scaled, "scaled:center"), "+")
synthetic_data <- as.data.frame(synthetic_data)
colnames(synthetic_data) <- colnames(demo_vars)

###############################################################################

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
  geom_bar(data = synthetic_data, aes(x = Race1, fill = "Synthetic"), position = "dodge", alpha = 0.7) +
  labs(title = "Race Distribution Comparison", x = "Race1", y = "Count")

# HHIncome distribution comparison
ggplot() +
  geom_density(data = demo_vars, aes(x = HHIncome, color = "Original")) +
  geom_density(data = synthetic_data, aes(x = HHIncome, color = "Synthetic")) +
  labs(title = "HHIncome Distribution Comparison", x = "HHIncome", y = "Density")

###############################################################################
###############################################################################
###############################################################################