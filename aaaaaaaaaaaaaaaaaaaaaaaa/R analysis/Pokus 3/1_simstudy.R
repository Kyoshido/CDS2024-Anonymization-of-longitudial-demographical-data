###############################################################################
###############################################################################
###############################################################################

# simstudy
# I am not sure if this is the best way to generate synthetic data

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
demo_vars <- NHANES_sample[, c("Year", "Age", "Gender", "Race1", "Education", "HHIncome")]
head(demo_vars)

###############################################################################

# Install and load the necessary packages
# install.packages("simstudy")
library(NHANES)
library(simstudy)
library(dplyr)
library(ggplot2)
library(data.table)

# Define baseline data for initial year with a unique identifier
def <- defData(varname = "unique_id", formula = "1", dist = "nonrandom", id = "unique_id")

# Generate baseline data
dt <- genData(1000, def)
head(dt)

# Add demographic variables from the original NHANES data using base R
dt$Age <- sample(demo_vars$Age, 1000, replace = TRUE)
dt$Gender <- sample(demo_vars$Gender, 1000, replace = TRUE)
dt$Race1 <- sample(demo_vars$Race1, 1000, replace = TRUE)
dt$Education <- sample(demo_vars$Education, 1000, replace = TRUE)
dt$HHIncome <- sample(demo_vars$HHIncome, 1000, replace = TRUE)
dt$Year <- sample(demo_vars$Year, 1000, replace = TRUE)

# Convert HHIncome to numeric (assuming HHIncome is a factor with levels representing income ranges)
# We need to handle the factor to numeric conversion properly if HHIncome is in ranges
income_levels <- c("0-9999", "10000-14999", "15000-19999", "20000-24999", "25000-34999",
                   "35000-44999", "45000-54999", "55000-64999", "65000-74999", "75000-99999", "100000+")

# Create a numeric mapping for the income levels
income_values <- seq(5000, 105000, by = 5000)
names(income_values) <- income_levels

# Convert HHIncome to numeric using the mapping
dt$HHIncome <- as.numeric(income_values[as.character(dt$HHIncome)])

head(dt)

# Define the longitudinal data without redefining Year
defLong <- defDataAdd(varname = "HHIncome_long", formula = "HHIncome + 2000*(Year-2000)", dist = "normal", variance = 5000)

# Add longitudinal data
dtLong <- genCluster(dt, "unique_id", numIndsVar = 10, "n")
dtLong <- addColumns(defLong, dtLong)

# Remove the original HHIncome to replace it with HHIncome_long
dtLong$HHIncome <- NULL
names(dtLong)[names(dtLong) == "HHIncome_long"] <- "HHIncome"

# Inspect the generated data
head(dtLong)
str(dtLong)
summary(dtLong)

# Compare original and synthetic data
original_demo_vars <- demo_vars[demo_vars$Year %in% unique(dtLong$Year), ]

# Plot age distribution by year
ggplot() +
  geom_density(data = original_demo_vars, aes(x = Age, color = "Original")) +
  geom_density(data = dtLong, aes(x = Age, color = "Synthetic")) +
  facet_wrap(~ Year) +
  labs(title = "Age Distribution Comparison by Year", x = "Age", y = "Density")

# Plot gender distribution by year
ggplot() +
  geom_bar(data = original_demo_vars, aes(x = Gender, fill = "Original"), position = "dodge") +
  geom_bar(data = dtLong, aes(x = Gender, fill = "Synthetic"), position = "dodge", alpha = 0.7) +
  facet_wrap(~ Year) +
  labs(title = "Gender Distribution Comparison by Year", x = "Gender", y = "Count")

# Plot race distribution by year
ggplot() +
  geom_bar(data = original_demo_vars, aes(x = Race1, fill = "Original"), position = "dodge") +
  geom_bar(data = dtLong, aes(x = Race1, fill = "Synthetic"), position = "dodge", alpha = 0.7) +
  facet_wrap(~ Year) +
  labs(title = "Race Distribution Comparison by Year", x = "Race1", y = "Count")

# Plot HHIncome distribution by year
ggplot() +
  geom_density(data = original_demo_vars, aes(x = HHIncome, color = "Original")) +
  geom_density(data = dtLong, aes(x = HHIncome, color = "Synthetic")) +
  facet_wrap(~ Year) +
  labs(title = "HHIncome Distribution Comparison by Year", x = "HHIncome", y = "Density")







###############################################################################
###############################################################################
###############################################################################