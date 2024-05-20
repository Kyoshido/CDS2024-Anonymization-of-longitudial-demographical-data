###############################################################################
###############################################################################
###############################################################################

# Synthetization

###############################################################################

# Packages --------------------------------------------------------------------
library(xgboost)
library(tsibble)
library(feasts)
library(dplyr)
library(ggplot2)

# Load data -------------------------------------------------------------------
df <- read.csv2(
  paste0(
    c(unlist(strsplit(getwd(), "/"))[1:(length(unlist(strsplit(getwd(), "/")))
                                        -1)], # removing n levels in files hierarchy
      "0_Data",
      "data.csv"
      ),
    collapse = '/'
  )
)

str(df)


# Assuming your dataset is loaded into a data frame named df
# Make sure to load your actual data frame here
# df <- read.csv("your_data.csv") # Example if you were loading from a CSV file

# Convert the dataset into a tsibble
df_tsibble <- as_tsibble(df, key = ID, index = YEAR)

# Feature engineering: create lagged features and ensure all columns used are numeric
df_features <- df_tsibble %>%
  mutate(
    lag_EARNINDN = lag(EARNINDN, 1),
    diff_EARNINDN = difference(EARNINDN, 1)
  ) %>%
  na.omit()

# Create matrix for XGBoost
data_matrix <- as.matrix(df_features %>% select(-ID, -YEAR, -EARNINDN))
label_vector <- df_features$EARNINDN

dtrain <- xgb.DMatrix(data = data_matrix, label = label_vector)

# Set parameters and train the model with cross-validation
params <- list(
  objective = "reg:squarederror",
  eval_metric = "rmse"
)

cv <- xgb.cv(
  params = params,
  data = dtrain,
  nfold = 5,
  nrounds = 100,
  verbose = 0,
  early_stopping_rounds = 10
)

# Train final model
bst <- xgboost(
  params = params,
  data = dtrain,
  nrounds = cv$best_iteration
)

# Make predictions
preds <- predict(bst, data_matrix)

# Add predictions to the original dataframe for comparison
df_features <- df_features %>%
  mutate(Predicted_EARNINDN = preds)

# Print first few rows to compare
print(df_features %>% select(YEAR, EARNINDN, Predicted_EARNINDN) %>% head(10))

# Visualize the comparison
ggplot(df_features, aes(x = YEAR)) +
  geom_line(aes(y = EARNINDN, color = "Original")) +
  geom_line(aes(y = Predicted_EARNINDN, color = "Predicted")) +
  labs(title = "Original vs Predicted EARNINDN", x = "Year", y = "EARNINDN") +
  scale_color_manual(values = c("Original" = "blue", "Predicted" = "red"))









