###############################################################################
###############################################################################
###############################################################################

# Data preparation

############################################################################### 

# Load libraries --------------------------------------------------------------
library(haven)
library(dplyr)

# Load data from a .dta file --------------------------------------------------
data <- read_dta("PSIDSHELF_1968_2019_LONG.dta")
head(data)
str(data)

# Select variables ------------------------------------------------------------

df <- data %>% select(# Survey variables
  ID # Unique ID
  ,YEAR # Survey year
  ,LINEAGE # family ID
  ,PNUM # Person number
  ,FID # Family ID, wave-specific
  ,HHID # Household ID, wave-specific
  # Social characteristics
  ,SEX # Ind’s sex
  ,AGEREP # Ind’s reported age at time of interview
  ,FAMSIZE # FU’s total number of individuals
  ,FAMCHILD # FU’s total number of children
  ,FAMMARSTAT # RP’s marital status
  ,RACE # RACE Ind’s race/ethnicity, from first mention
  ,EDULEVEL # Ind’s highest level of education
  # Economic characteristics
  ,EARNINDN # Ind’s earnings (nominal USD), tax year
  ,EARNINDNRP # RP’s earnings (nominal USD), tax year 
)
rm(data)

# Converting a list to a data.frame
df <- as.data.frame(df)
df <- na.omit(df)
df <- df %>%
  mutate_all(~as.vector(.))
str(df)

df <- df %>% filter(YEAR >= 2000)

# Save data -------------------------------------------------------------------
write.csv2(df, "data.csv")

# -----------------------------------------------------------------------------

###############################################################################
###############################################################################
###############################################################################