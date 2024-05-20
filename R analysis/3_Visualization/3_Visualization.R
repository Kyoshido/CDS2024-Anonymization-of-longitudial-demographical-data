###############################################################################
###############################################################################
###############################################################################

# Visualization

###############################################################################

# Packages --------------------------------------------------------------------
library(ggplot2)
library(dplyr)

# Load data -------------------------------------------------------------------
df <- read.csv2(
  paste0(
    c(unlist(strsplit(getwd(), "/"))[1:(length(unlist(strsplit(getwd(), "/")))
                                        -1)], # removing n levels in files hierarchy
      "1_Synthetization",
      "synthetic_data.csv"
    ),
    collapse = '/'
  )
)

str(df)

# Visualize the comparison ---------------------------------------------------
 
# Reshape the data for plotting, focusing only on AGE
comparison_age <- df %>%
  select(YEAR, AGEREP, Type)

###############################################################################

# Visualize the comparison using boxplot with transparent background

# text_color <- "#000000"  # Black text for readability
text_color <- "white"  # white text for poster

ggplot(comparison_age, aes(x = as.factor(YEAR), y = AGEREP, fill = Type)) +
  geom_boxplot(alpha = 1, color = text_color) +
  labs(# title = "Original vs Synthetic AGE by Year"
       ,x = "Year" 
       ,y = "AGE") +
  scale_fill_manual(values = c("Original" = "#84E516", 
                               "Synthetic" = "#ff7f0e")) +
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

# Visualize the comparison using histograms faceted by year

# Function to create custom labels for facet wrap
year_labeller <- function(variable, value) {
  return(paste0(#"Year: ",
    value
  ))
}

# text_color <- "#000000"  # Black text for readability
text_color <- "white"  # white text for poster

ggplot(comparison_age, aes(x = AGEREP, 
                           fill = Type)) +
  geom_histogram(alpha = 0.8, 
                 position = "identity", 
                 binwidth = 1) +
  labs(#title = "Distribution of AGE by Year", 
       x = "AGE", 
       y = "Count") +
  scale_fill_manual(values = c("Original" = "#84E516", 
                               "Synthetic" = "#ff7f0e")) +
  facet_wrap(~ YEAR, 
             scales = "free",
             labeller = year_labeller) +
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
    strip.text = element_text(color = text_color), 
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
###############################################################################
###############################################################################