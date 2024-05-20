###############################################################################
###############################################################################
###############################################################################

# The synthesizer utilized the XGBoost algorithm, which was adapted 
# for longitudinal data.

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

# df$RACE <- as.factor(df$RACE)
# df$EDULEVEL <- as.factor(df$EDULEVEL)

# Prepare your longitudinal data ----------------------------------------------

# Convert the dataset into a tsibble with LINEAGE and PNUM as keys
df_tsibble <- as_tsibble(df, key = c(LINEAGE, 
                                     PNUM), 
                         index = YEAR)

# Feature engineering: create lagged features for the variables to be synthesized
df_features <- df_tsibble %>%
  group_by(LINEAGE, PNUM) %>%
  mutate(
    lag_SEX = lag(SEX, 1),
    lag_AGE = lag(AGEREP, 1),
    lag_RACE = lag(RACE, 1),
    lag_EDULEVEL = lag(EDULEVEL, 1)
  ) %>%
  ungroup() %>%
  na.omit()

# Create DMatrix for XGBoost --------------------------------------------------

# Function to train and predict using XGBoost for a given target variable
synthesize_variable <- function(df, target) {
  data_matrix <- as.matrix(df %>% select(-ID, -YEAR, -LINEAGE, -PNUM, -SEX, -AGEREP, -RACE, -EDULEVEL, -!!sym(target)))
  label_vector <- df[[target]]
  dtrain <- xgb.DMatrix(data = data_matrix, label = label_vector)
  
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
  
  bst <- xgboost(
    params = params,
    data = dtrain,
    nrounds = cv$best_iteration
  )
  
  preds <- predict(bst, data_matrix)
  return(preds)
}

# Train XGBoost Model with Cross-Validation -----------------------------------

# Train models and synthesize data for each variable
df_synthetic <- df_features
df_synthetic$Synthetic_SEX <- synthesize_variable(df_features, "SEX")
df_synthetic$Synthetic_AGE <- synthesize_variable(df_features, "AGEREP")
df_synthetic$Synthetic_RACE <- synthesize_variable(df_features, "RACE")
df_synthetic$Synthetic_EDULEVEL <- synthesize_variable(df_features, "EDULEVEL")
df_synthetic$Synthetic_EARNIND <- synthesize_variable(df_features, "EARNINDN")

# Compare Original and Synthetic Data -----------------------------------------

# Convert df_features and df_synthetic to tibbles
df_features_tibble <- as_tibble(df_features)
df_synthetic_tibble <- as_tibble(df_synthetic)

# Combine original and synthetic data for comparison
comparison_df <- df_features_tibble %>%
  select(ID, YEAR, SEX, AGEREP, RACE, EDULEVEL) %>%
  mutate(Type = "Original") %>%
  bind_rows(
    df_synthetic_tibble %>%
      select(ID, YEAR, Synthetic_SEX, Synthetic_AGE, Synthetic_RACE, 
             Synthetic_EDULEVEL) %>%
      rename(SEX = Synthetic_SEX, 
             AGEREP = Synthetic_AGE, 
             RACE = Synthetic_RACE, 
             EDULEVEL = Synthetic_EDULEVEL) %>%
      mutate(Type = "Synthetic")
  )


# Save the synthetic data ----------------------------------------------------

write.csv2(comparison_df, 
           file = "synthetic_data.csv")

# -----------------------------------------------------------------------------

###############################################################################
###############################################################################
###############################################################################

# Visualize the comparison ---------------------------------------------------
 

# Visualize the comparison
ggplot(comparison_df, 
       aes(x = YEAR)) +
  geom_line(aes(y = AGEREP,
                color = "AGE",
                linetype = Type))


 
ggplot(comparison_df, 
       aes(x = YEAR)) +
  geom_line(aes(y = AGEREP,
                color = "AGE",
                linetype = Type)) +
  geom_line(aes(y = EDULEVEL, color = "EDULEVEL", 
                linetype = Type)) +
  labs(title = "Original vs Synthetic Variables", 
       x = "Year", 
       y = "Value") +
  scale_color_manual(values = c("AGE" = "green")) +
  scale_linetype_manual(values = c("Original" = "solid", 
                                   "Synthetic" = "dashed"))


geom_ribbon(aes(ymin = ifelse(Type == "Original", value, NA), 
                ymax = ifelse(Type == "Synthetic", value, NA)), 
            alpha = 0.1, fill = "grey70") +


# Reshape the data for plotting
library(tidyr)
comparison_long <- comparison_df %>%
  pivot_longer(cols = c(SEX, AGEREP, RACE, EDULEVEL), 
               names_to = "variable", 
               values_to = "value")


# Improved visualization
ggplot(comparison_long, aes(x = YEAR, y = value, color = variable, linetype = Type)) +
  geom_line() +
  labs(title = "Original vs Synthetic Variables", x = "Year", y = "Value") +
  scale_color_manual(values = c("SEX" = "blue", "AGEREP" = "green", "RACE" = "red", "EDULEVEL" = "purple")) +
  scale_linetype_manual(values = c("Original" = "solid", "Synthetic" = "dashed")) +
  facet_wrap(~ variable, scales = "free_y") +
  theme_minimal() +
  theme(legend.position = "bottom")

# Visualize the comparison
ggplot(comparison_long, aes(x = YEAR, y = value, color = Type, group = interaction(Type, ID))) +
  geom_line(size = 1) +
  geom_ribbon(aes(ymin = ifelse(Type == "Original", value, NA), 
                  ymax = ifelse(Type == "Synthetic", value, NA)), 
              alpha = 0.1, fill = "grey70") +
  facet_wrap(~ variable, scales = "free_y") +
  labs(title = "Original vs Synthetic Variables", 
       x = "Year", 
       y = "Value") +
  scale_color_manual(values = c("Original" = "blue",
                                "Synthetic" = "red")) +
  theme_minimal() +
  theme(legend.position = "bottom")



###############################################################################
# Reshape the data for plotting, focusing only on AGE
comparison_age <- comparison_df %>%
  select(ID, YEAR, AGEREP, Type)

# Visualize the comparison
ggplot(comparison_age, 
       aes(x = YEAR, 
           y = AGEREP, 
           color = Type,
           shape = Type)
       ) +
  geom_point(size = 2, 
             alpha = 0.6) +
  labs(title = "Original vs Synthetic AGE", 
       x = "Year",
       y = "AGE") +
  scale_color_manual(values = c("Original" = "blue", "Synthetic" = "red")) +
  scale_shape_manual(values = c("Original" = 16, "Synthetic" = 17)) +
  theme_minimal() +
  theme(legend.position = "bottom")
###############################################################################

# Reshape the data for plotting, focusing only on AGE
comparison_age <- comparison_df %>%
  select(YEAR, AGEREP, Type)

# Visualize the comparison using boxplot
ggplot(comparison_age, aes(x = as.factor(YEAR), y = AGEREP, fill = Type)) +
  geom_boxplot(alpha = 0.6) +
  labs(title = "Original vs Synthetic AGE by Year", x = "Year", y = "AGE") +
  scale_fill_manual(values = c("Original" = "blue", "Synthetic" = "red")) +
  theme_minimal() +
  theme(legend.position = "bottom")

###############################################################################

# Reshape the data for plotting, focusing only on AGE
comparison_age <- comparison_df %>%
  select(YEAR, AGEREP, Type)

# Define colors for the plot
background_color <- "#f0f0f0"  # Replace with your presentation background color
boxplot_fill_colors <- c("Original" = "#1f77b4", "Synthetic" = "#ff7f0e")  # Blue and Orange for contrast
text_color <- "#000000"  # Black text for readability

# Visualize the comparison using boxplot
ggplot(comparison_age, aes(x = as.factor(YEAR), y = AGEREP, fill = Type)) +
  geom_boxplot(alpha = 0.7, color = text_color) +
  labs(title = "Original vs Synthetic AGE by Year", x = "Year", y = "AGE") +
  scale_fill_manual(values = boxplot_fill_colors) +
  theme_minimal(base_size = 15) +
  theme(
    plot.background = element_rect(fill = NA, color = NA),
    panel.background = element_rect(fill = NA, color = NA),
    legend.background = element_rect(fill = NA, color = NA),
    legend.key = element_rect(fill = NA, color = NA),
    legend.text = element_text(color = text_color),
    legend.title = element_text(color = text_color),
    plot.title = element_text(color = text_color, face = "bold"),
    axis.title = element_text(color = text_color),
    axis.text = element_text(color = text_color)
  ) +
  theme(legend.position = "bottom")

# Visualize the comparison using boxplot with transparent background
text_color <- "#000000"  # Black text for readability
# text_color <- "white"  # white text for poster
ggplot(comparison_age, aes(x = as.factor(YEAR), y = AGEREP, fill = Type)) +
  geom_boxplot(alpha = 0.7, color = "white") +
  labs(title = "Original vs Synthetic AGE by Year", x = "Year", y = "AGE") +
  scale_fill_manual(values = c("Original" = "#84E516", "Synthetic" = "#ff7f0e")) +
  theme_minimal(base_size = 15) +
  theme(
    plot.background = element_rect(fill = NA, color = NA),
    panel.background = element_rect(fill = NA, color = NA),
    legend.background = element_rect(fill = NA, color = NA),
    legend.key = element_rect(fill = NA, color = NA),
    legend.text = element_text(color = text_color),
    legend.title = element_text(color = text_color),
    plot.title = element_text(color = text_color, face = "bold"),
    axis.title = element_text(color = text_color),
    axis.text = element_text(color = text_color),
    legend.position = "bottom"
  )

# Save the plot with transparent background
ggsave("comparison_age_boxplot.png", 
       plot = last_plot(), 
       bg = "transparent", 
       units = "cm",
       width = 20, 
       height = 14)


###############################################################################

# Reshape the data for plotting, focusing only on AGE
comparison_age <- comparison_df %>%
  select(YEAR, AGEREP, Type)

# Visualize the comparison using density plots faceted by year
text_color <- "#000000"  # Black text for readability
# text_color <- "white"  # white text for poster
ggplot(comparison_age, aes(x = AGEREP, fill = Type)) +
  geom_density(alpha = 0.6) +
  labs(title = "Distribution of AGE by Year", x = "AGE", y = "Density") +
  scale_fill_manual(values = c("Original" = "#84E516", 
                               "Synthetic" = "#ff7f0e")) +
  facet_wrap(~ YEAR, scales = "free") +
  theme_minimal(base_size = 15) +
  theme(
    plot.background = element_rect(fill = NA, color = NA),
    panel.background = element_rect(fill = NA, color = NA),
    legend.background = element_rect(fill = NA, color = NA),
    legend.key = element_rect(fill = NA, color = NA),
    legend.text = element_text(color = text_color),
    legend.title = element_text(color = text_color),
    plot.title = element_text(color = text_color, face = "bold"),
    axis.title = element_text(color = text_color),
    axis.text = element_text(color = text_color),
    legend.position = "right"
  )

###############################################################################

# Visualize the comparison using histograms faceted by year
text_color <- "#000000"  # Black text for readability
text_color <- "white"  # white text for poster
ggplot(comparison_age, aes(x = AGEREP, fill = Type)) +
  geom_histogram(alpha = 0.8, 
                 position = "identity", 
                 binwidth = 1) +
  labs(title = "Distribution of AGE by Year", x = "AGE", y = "Count") +
  scale_fill_manual(values = c("Original" = "#84E516", 
                               "Synthetic" = "#ff7f0e")) +
  facet_wrap(~ YEAR, scales = "free") +
  theme_minimal(base_size = 15) +
  theme(
    plot.background = element_rect(fill = NA, color = NA),
    panel.background = element_rect(fill = NA, color = NA),
    legend.background = element_rect(fill = NA, color = NA),
    legend.key = element_rect(fill = NA, color = NA),
    legend.text = element_text(color = text_color),
    legend.title = element_text(color = text_color),
    plot.title = element_text(color = text_color, face = "bold"),
    axis.title = element_text(color = text_color),
    axis.text = element_text(color = text_color),
    legend.position = "bottom"
  )

# Save the plot with transparent background
ggsave("comparison_age_histogram.png", 
       plot = last_plot(), 
       bg = "transparent", 
       units = "cm",
       width = 20, 
       height = 14)

###############################################################################

# Check the unique values in the EDULEVEL variable
unique_education_levels <- unique(df$EDULEVEL)
print(unique_education_levels)

# Count the number of unique categories in the EDULEVEL variable
num_education_levels <- length(unique_education_levels)
print(num_education_levels)

# Load necessary packages
library(ggplot2)
library(dplyr)

# Ensure there is no existing EDULEVEL column in synthetic data before renaming
df_synthetic_tibble <- df_synthetic_tibble %>%
  select(-EDULEVEL) %>%
  rename(EDULEVEL = Synthetic_EDULEVEL) %>%
  mutate(Type = "Synthetic")

# Add the 'Type' column to original data
df_features_tibble <- df_features_tibble %>%
  mutate(Type = "Original")

# Combine original and synthetic data for comparison
comparison_edu <- bind_rows(df_features_tibble, df_synthetic_tibble)

# Convert EDULEVEL to factor
comparison_edu <- comparison_edu %>%
  mutate(EDULEVEL = as.factor(EDULEVEL))

# Count the frequency of each education level by year and type
education_levels_count <- comparison_edu %>%
  group_by(YEAR, EDULEVEL, Type) %>%
  summarise(Count = n(), .groups = 'drop')

# Visualize the distribution of education levels using a faceted bar plot
Original_color <- "#84E516"
Synthetic_color <- "#ff7f0e"
text_color <- "#000000"  # Black text for readability
# text_color <- "white"  # white text for poster
ggplot(education_levels_count,
       aes(x = EDULEVEL, 
           y = Count, 
           fill = Type)) +
  geom_bar(stat = "identity", 
           position = "dodge",
           alpha = 0.7,
           color = "white") +
  labs(title = "Distribution of Education Levels by Year", x = "Education Level", y = "Count") +
  scale_fill_manual(values = c("Original" = Original_color, 
                               "Synthetic" = Synthetic_color)) +
  facet_wrap(~ YEAR, scales = "free_y") +
  theme_minimal(base_size = 15) +
  theme(
    plot.background = element_rect(fill = NA, color = NA),
    panel.background = element_rect(fill = NA, color = NA),
    legend.background = element_rect(fill = NA, color = NA),
    legend.key = element_rect(fill = NA, color = NA),
    legend.text = element_text(color = text_color),
    legend.title = element_text(color = text_color),
    plot.title = element_text(color = text_color, face = "bold"),
    axis.title = element_text(color = text_color),
    axis.text = element_text(color = text_color),
    legend.position = "right"
  )

# Save the plot with transparent background
ggsave("comparison_edu_bar.png", 
       plot = last_plot(), 
       bg = "transparent", 
       units = "cm",
       width = 20, 
       height = 14)








