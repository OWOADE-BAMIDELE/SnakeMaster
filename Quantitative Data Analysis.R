# Quantitative Data Analysis

# Load the libraries
library(dplyr)
library(ggplot2)
library(scales)
library(lubridate)
library(stats)
library(car)
library(corrplot)
library(rstatix)
library(GGally)
library(gridExtra)
library(viridis)

# Load the datasets
Continent2 <- read.csv('C:\\Users\\HomePC\\Desktop\\Projects\\Orekoya\\1\\continent2.csv')
head(Continent2, 5)


# Information about the dataset
str(Continent2)

# Data types of each column.
sapply(Continent2, class)

# Descriptive statistics
summary(Continent2)

# Convert 'year' column to numeric
Continent2$year <- as.numeric(Continent2$year)

# Pivot the data for easier plotting
Continent2_long <- pivot_longer(Continent2, cols = africa:s.america, names_to = "Continent", values_to = "CO2_Emissions")

# Plot using ggplot2
ggplot(Continent2_long, aes(x = year, y = CO2_Emissions, color = Continent)) +
  geom_line(size = 1) +
  labs(title = "Annual CO2 Emissions by Continent (1750 - 2022)",
       x = "Year",
       y = "CO2 Emissions (tonnes)",
       color = "Continent") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_x_continuous(breaks = seq(1750, 2022, by = 50)) +
  scale_y_continuous(labels = scales::comma) +
  scale_color_brewer(palette = "Set1") +  # Change palette as needed
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(legend.position = "right") +
  theme(figure.size = c(12, 7))  # Adjust figure size as per your preference


# Load the datasets
Countries <- read.csv('C:\\Users\\HomePC\\Desktop\\Projects\\Orekoya\\1\\countries.csv')
head(Countries, 5)

# Information about the dataset
str(Countries)

# Descriptive statistics
summary(Countries)

# Data types of each column
sapply(Countries, class)

# 1. Box Plot with points
plot1 <- ggplot(Countries, aes(x = Country, y = CO2_Mt)) +
  geom_boxplot(aes(fill = Country), alpha = 0.7) +
  geom_jitter(alpha = 0.2, width = 0.2) +
  scale_fill_viridis_d() +
  labs(title = "Distribution of CO2 Emissions by Country",
       subtitle = "Box Plot with Data Points",
       x = "Country",
       y = "CO2 Emissions (Mt)") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 12, hjust = 0.5),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10),
    legend.position = "none"
  )

# 2. Violin Plot
plot2 <- ggplot(Countries, aes(x = Country, y = CO2_Mt)) +
  geom_violin(aes(fill = Country), alpha = 0.7) +
  geom_boxplot(width = 0.2, alpha = 0.7) +
  scale_fill_viridis_d() +
  labs(title = "Distribution of CO2 Emissions by Country",
       subtitle = "Violin Plot with Box Plot Overlay",
       x = "Country",
       y = "CO2 Emissions (Mt)") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 12, hjust = 0.5),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10),
    legend.position = "none"
  )

# Arrange plots side by side
combined_plots <- grid.arrange(plot1, plot2, ncol = 2)

# Save the combined plots
ggsave("co2_distributions.png", combined_plots, width = 12, height = 7, dpi = 300)

# Statistical Analysis
# First, check normality
shapiro_test <- Countries %>%
  group_by(Country) %>%
  summarise(p_value = shapiro.test(CO2_Mt)$p.value)

# Perform Kruskal-Wallis test (non-parametric alternative to ANOVA)
kruskal_test <- kruskal.test(CO2_Mt ~ Country, data = Countries)

# Perform pairwise Wilcoxon test with Bonferroni correction
pairwise_test <- pairwise.wilcox.test(
  Countries$CO2_Mt,
  Countries$Country,
  p.adjust.method = "bonferroni"
)

# Print statistical results
print("Shapiro-Wilk Test Results (Testing for Normality):")
print(shapiro_test)
print("\nKruskal-Wallis Test Results:")
print(kruskal_test)
print("\nPairwise Wilcoxon Test Results:")
print(pairwise_test)


# Create the boxplot with specific colors and styling
ggplot_box <- ggplot(Countries, aes(x = Country, y = CO2_Mt)) +
  geom_boxplot(aes(fill = Country)) +
  scale_fill_manual(values = c(
    "France" = "#96DED1",      # Turquoise
    "Germany" = "#FFFACD",     # Light yellow
    "Italy" = "#E6E6FA",       # Lavender
    "Spain" = "#FFB6B6",       # Light coral
    "United Kingdom" = "#B0C4DE"  # Light steel blue
  )) +
  labs(
    title = "Distribution of CO2 Emissions by Country (1901-1935)",
    x = "Country",
    y = "CO2 Emissions (Mt)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10),
    legend.position = "none",
    panel.grid.major.x = element_blank(),
    panel.border = element_rect(fill = NA, color = "black", linewidth = 0.5)
  ) +
  # Add one-way ANOVA results
  annotate("text", x = 1, y = max(Countries$CO2_Mt), 
           label = "One-way ANOVA:", hjust = 0) +
  annotate("text", x = 1, y = max(Countries$CO2_Mt) * 0.95, 
           label = "F-statistic: 268.83", hjust = 0) +
  annotate("text", x = 1, y = max(Countries$CO2_Mt) * 0.90, 
           label = "p-value: 0.0000", hjust = 0)

# Save the plot
ggsave("co2_distribution.png", ggplot_box, width = 12, height = 7, dpi = 300)

# Perform statistical analysis
# One-way ANOVA
aov_result <- aov(CO2_Mt ~ Country, data = Countries)
# Tukey's HSD test
tukey_result <- TukeyHSD(aov_result)

# Print descriptive statistics
desc_stats <- Countries %>%
  group_by(Country) %>%
  summarise(
    Mean = mean(CO2_Mt),
    STD = sd(CO2_Mt)
  )

# Print results
print("Descriptive Statistics:")
print(desc_stats)

print("\nTukey's HSD Test Results:")
print(tukey_result)

# Calculate descriptive statistics
desc_stats <- Countries %>%
  group_by(Country) %>%
  summarise(
    Mean = round(mean(CO2_Mt), 2),
    STD = round(sd(CO2_Mt), 2)
  )

# Create the plot with descriptive statistics
ggplot_box <- ggplot(Countries, aes(x = Country, y = CO2_Mt)) +
  geom_boxplot(aes(fill = Country)) +
  
  # Set specific colors
  scale_fill_manual(values = c(
    "France" = "#96DED1",      # Turquoise
    "Germany" = "#FFFACD",     # Light yellow
    "Italy" = "#E6E6FA",       # Lavender
    "Spain" = "#FFB6B6",       # Light coral
    "United Kingdom" = "#B0C4DE"  # Light steel blue
  )) +
  
  # Set y-axis scale with breaks
  scale_y_continuous(
    limits = c(0, 1100000),
    breaks = seq(0, 1000000, by = 200000),
    labels = function(x) format(x, big.mark = ",", scientific = FALSE)
  ) +
  
  # Labels
  labs(
    title = "Distribution of CO2 Emissions by Country (1901-1935)",
    x = "Country",
    y = "CO2 Emissions (Mt)"
  ) +
  
  # Theme customization
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10),
    legend.position = "none",
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(color = "gray90"),
    panel.border = element_rect(fill = NA, color = "black", linewidth = 0.5),
    plot.margin = margin(30, 120, 30, 30)  # Increase right margin for stats
  ) +
  
  # Add ANOVA results
  annotate("text", x = 0.7, y = 1050000, 
           label = "One-way ANOVA:", hjust = 0, size = 3.5) +
  annotate("text", x = 0.7, y = 1000000, 
           label = "F-statistic: 268.83", hjust = 0, size = 3.5) +
  annotate("text", x = 0.7, y = 950000, 
           label = "p-value: 0.0000", hjust = 0, size = 3.5) +
  
  # Add Descriptive Statistics
  annotate("text", x = 6.2, y = 1050000, 
           label = "Descriptive Statistics:", hjust = 0, size = 3.5) +
  
  # Add country statistics
  annotate("text", x = 6.2, y = c(950000, 850000, 750000, 650000, 550000), 
           label = c(
             sprintf("France:\nMean: %.2f\nSTD: %.2f", 
                     desc_stats$Mean[desc_stats$Country == "France"],
                     desc_stats$STD[desc_stats$Country == "France"]),
             sprintf("Germany:\nMean: %.2f\nSTD: %.2f", 
                     desc_stats$Mean[desc_stats$Country == "Germany"],
                     desc_stats$STD[desc_stats$Country == "Germany"]),
             sprintf("Italy:\nMean: %.2f\nSTD: %.2f", 
                     desc_stats$Mean[desc_stats$Country == "Italy"],
                     desc_stats$STD[desc_stats$Country == "Italy"]),
             sprintf("Spain:\nMean: %.2f\nSTD: %.2f", 
                     desc_stats$Mean[desc_stats$Country == "Spain"],
                     desc_stats$STD[desc_stats$Country == "Spain"]),
             sprintf("United Kingdom:\nMean: %.2f\nSTD: %.2f", 
                     desc_stats$Mean[desc_stats$Country == "United Kingdom"],
                     desc_stats$STD[desc_stats$Country == "United Kingdom"])
           ),
           hjust = 0, size = 3, lineheight = 0.8)

# Save the plot with exact dimensions
ggsave("co2_distribution_exact.png", ggplot_box, 
       width = 12, height = 7, dpi = 300)

# Display the plot
print(ggplot_box)


# Load the dataset
GHG_rel <- read.csv('C:\\Users\\HomePC\\Desktop\\Projects\\Orekoya\\1\\GHG_rel.csv')

# View the first 5 rows
head(GHG_rel, 5)


# Information about the dataset
str(GHG_rel)

# Descriptive statistics of the dataset
summary(GHG_rel)


# Load required libraries
library(ggplot2)
library(gridExtra)
library(dplyr)
library(stats)

# Read the data (assuming your data is in 'GHG_rel.csv')
df <- read.csv('C:\\Users\\HomePC\\Desktop\\Projects\\Orekoya\\1\\GHG_rel.csv')

# Create a function for correlation analysis
get_correlation_stats <- function(x, y) {
  cor_test <- cor.test(x, y)
  return(list(
    correlation = cor_test$estimate,
    p_value = cor_test$p.value
  ))
}

# Calculate correlations
co2_ch4_stats <- get_correlation_stats(df$CO2, df$CH4)
co2_n2o_stats <- get_correlation_stats(df$CO2, df$N2O)
ch4_n2o_stats <- get_correlation_stats(df$CH4, df$N2O)

# Create scatter plots with regression lines
plot1 <- ggplot(df, aes(x = CO2, y = CH4)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = TRUE) +
  labs(title = "CO2 vs CH4 Emissions",
       x = "CO2 Emissions (kt)",
       y = "CH4 Emissions (kt)") +
  theme_minimal()

plot2 <- ggplot(df, aes(x = CO2, y = N2O)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = TRUE) +
  labs(title = "CO2 vs N2O Emissions",
       x = "CO2 Emissions (kt)",
       y = "N2O Emissions (kt)") +
  theme_minimal()

plot3 <- ggplot(df, aes(x = CH4, y = N2O)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = TRUE) +
  labs(title = "CH4 vs N2O Emissions",
       x = "CH4 Emissions (kt)",
       y = "N2O Emissions (kt)") +
  theme_minimal()

# Create correlation text
corr_text <- sprintf(
  "Correlation Analysis:\n\nCO2 vs CH4: r = %.3f (p = %.2e)\nCO2 vs N2O: r = %.3f (p = %.2e)\nCH4 vs N2O: r = %.3f (p = %.2e)",
  co2_ch4_stats$correlation, co2_ch4_stats$p_value,
  co2_n2o_stats$correlation, co2_n2o_stats$p_value,
  ch4_n2o_stats$correlation, ch4_n2o_stats$p_value
)

# Create a blank plot with text
plot4 <- ggplot() +
  annotate("text", x = 0.5, y = 0.5, label = corr_text) +
  theme_void() +
  theme(plot.margin = unit(c(1, 1, 1, 1), "cm"))

# Arrange all plots in a grid
grid.arrange(plot1, plot2, plot3, plot4, ncol = 2,
             top = "Relationships Between Greenhouse Gas Emissions in the UK (1990-2014)")

# Calculate and print descriptive statistics
stats_summary <- df %>%
  select(CO2, CH4, N2O) %>%
  summary()
print("Descriptive Statistics:")
print(stats_summary)

# Create time series plot with normalized values
df_normalized <- df %>%
  mutate(
    CO2_norm = CO2 / max(CO2),
    CH4_norm = CH4 / max(CH4),
    N2O_norm = N2O / max(N2O)
  )

# Create time series plot
ggplot(df_normalized, aes(x = Year)) +
  geom_line(aes(y = CO2_norm, color = "CO2")) +
  geom_line(aes(y = CH4_norm, color = "CH4")) +
  geom_line(aes(y = N2O_norm, color = "N2O")) +
  labs(title = "Normalized Trends in Greenhouse Gas Emissions (1990-2014)",
       x = "Year",
       y = "Normalized Emissions",
       color = "Gas Type") +
  theme_minimal() +
  theme(legend.position = "bottom") +
  scale_color_manual(values = c("CO2" = "blue", "CH4" = "red", "N2O" = "green"))


