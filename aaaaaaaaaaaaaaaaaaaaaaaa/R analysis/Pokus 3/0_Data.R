###############################################################################
###############################################################################
###############################################################################

# Data

###############################################################################

# NHANES (National Health and Nutrition Examination Survey)
# Description: NHANES is a national survey that provides data on the health and
#              nutritional status of the U.S. population. It is conducted by 
#              the National Center for Health Statistics (NCHS), which is part 
#              of the Centers for Disease Control and Prevention (CDC).
# Variables: It includes demographic information (age, gender, race, 
#            socioeconomic status), health measurements (blood pressure, 
#            cholesterol levels), dietary information, physical activity, 
#            and other health indicators.
# Usage: The data is used to monitor trends in the health and nutritional status
#        of the population, identify risk factors, and help in the development 
#        of health policies and programs.

# NHANES ----------------------------------------------------------------------

# install.packages("NHANES")
library(NHANES)

# Load the NHANES data
data(NHANES)

# Explore the structure of the dataset
str(NHANES)

# Get a summary of the dataset
summary(NHANES)

# View the first few rows of the dataset
head(NHANES)

# Display the names of all variables in the dataset
names(NHANES)

# Select key demographic variables
var_dem <- NHANES[, c("Age", "Gender", "Race1", "Education", "MaritalStatus")]
head(demo_vars)

###############################################################################
###############################################################################
###############################################################################