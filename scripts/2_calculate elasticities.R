# EMISSIONS-GDP ELASTICITY ANALYSIS ####
# Calculating Decoupling Metrics for US States (2012-2019)
# This script uses the data structure with per capita values already calculated
# Data frames needed (already loaded):
# - gdpc: GDP per capita
# - tec: Territorial emissions per capita
# - cbec: Consumption-based emissions per capita (needs cleaning)

# Following methodology from:
# Freire-González et al. (2024). "World economies' progress in decoupling 
# from CO2 emissions."

# Load required packages
library(tidyverse)   # Data manipulation and visualization
library(broom)       # Tidy regression outputs
library(sandwich)    # Robust standard errors
library(lmtest)      # Hypothesis testing for linear models
library(scales)      # For formatting numbers
library(gridExtra)   # For arranging multiple plots


# 1. CHECK DATA IS LOADED ####

# Check if data frames exist
if (!exists("gdpc")) stop("ERROR: 'gdpc' not found. Please load GDP per capita data.")
if (!exists("tec")) stop("ERROR: 'tec' not found. Please load territorial emissions per capita data.")
if (!exists("cbec")) stop("ERROR: 'cbec' not found. Please load consumption-based emissions per capita data.")

cat("GDP per capita (gdpc):", nrow(gdpc), "rows\n")
cat("Territorial emissions per capita (tec):", nrow(tec), "rows\n")
cat("Consumption emissions per capita (cbec):", nrow(cbec), "rows\n")


# 2. CLEAN AND PREPARE DATA ####

# Function to reshape and clean data
clean_and_reshape <- function(df, value_name) {
  # Clean the data frame
  df_clean <- df %>%
    # Remove USA aggregate row
    filter(`NA.` != "USA") %>%
    # Select only state code and year columns (X2012-X2019)
    select(`NA.`, starts_with("X20")) %>%
    # Remove any extra NA columns
    select_if(~!all(is.na(.))) %>%
    # Rename state column
    rename(state_code = `NA.`)
  
  # Convert X2012 to numeric if it's character (issue with cbec)
  if (is.character(df_clean$X2012)) {
    df_clean$X2012 <- as.numeric(df_clean$X2012)
  }
  
  # Reshape to long format
  df_long <- df_clean %>%
    pivot_longer(
      cols = starts_with("X"),
      names_to = "year",
      values_to = value_name
    ) %>%
    # Clean year column (remove "X" prefix)
    mutate(year = as.numeric(gsub("X", "", year))) %>%
    # Remove any NA values
    filter(!is.na(!!sym(value_name)))
  
  return(df_long)
}

# Reshape each dataset
gdp_long <- clean_and_reshape(gdpc, "gdp_per_capita")
terr_long <- clean_and_reshape(tec, "emissions_terr_pc")
cons_long <- clean_and_reshape(cbec, "emissions_cons_pc")

cat("Reshaped GDP data:", nrow(gdp_long), "observations\n")
cat("Reshaped territorial emissions:", nrow(terr_long), "observations\n")
cat("Reshaped consumption emissions:", nrow(cons_long), "observations\n")

# Merge all datasets
data_merged <- gdp_long %>%
  left_join(terr_long, by = c("state_code", "year")) %>%
  left_join(cons_long, by = c("state_code", "year"))

# Check for missing values
missing_check <- data_merged %>%
  summarise(
    missing_gdp = sum(is.na(gdp_per_capita)),
    missing_terr = sum(is.na(emissions_terr_pc)),
    missing_cons = sum(is.na(emissions_cons_pc))
  )

if (any(missing_check > 0)) {
  cat("\n⚠️ WARNING: Missing values detected:\n")
  print(missing_check)
  cat("\nProceeding with complete cases only...\n")
}

# Create clean dataset with log transformations
data_clean <- data_merged %>%
  filter(
    !is.na(gdp_per_capita) & 
      !is.na(emissions_terr_pc) & 
      !is.na(emissions_cons_pc)
  ) %>%
  mutate(
    # Create log-transformed variables (required for elasticity calculation)
    ln_gdp = log(gdp_per_capita),
    ln_terr = log(emissions_terr_pc),
    ln_cons = log(emissions_cons_pc)
  ) %>%
  # Remove any infinite values from log transformation
  filter(
    is.finite(ln_gdp) & is.finite(ln_terr) & is.finite(ln_cons)
  )

cat("\n✓ Data cleaned and prepared successfully!\n")
cat("Final sample:", nrow(data_clean), "observations\n")
cat("Number of states:", n_distinct(data_clean$state_code), "\n")
cat("Years covered:", min(data_clean$year), "to", max(data_clean$year), "\n")

# Preview the data ####
cat("\n=== SAMPLE OF PREPARED DATA ===\n")
print(head(data_clean %>% 
             select(state_code, year, gdp_per_capita, 
                    emissions_terr_pc, emissions_cons_pc), 10))


# 3. CALCULATE ELASTICITIES FOR EACH STATE ####

# Following Freire-González et al. (2024) methodology
# Using OLS with Newey-West HAC robust standard error

## Function to calculate elasticity with robust standard errors ####
calculate_elasticity <- function(data, emission_var) {
  # Check if we have enough observations
  if (nrow(data) < 5) {
    return(tibble(
      elasticity = NA,
      std_error = NA,
      p_value = NA,
      ci_lower = NA,
      ci_upper = NA,
      r_squared = NA,
      n_obs = nrow(data),
      significant = NA,
      status = "insufficient data"
    ))
  }
  
  # Run OLS regression: ln(Emissions) = α + β * ln(GDP) + ε
  tryCatch({
    model <- lm(as.formula(paste(emission_var, "~ ln_gdp")), data = data)
    
    # Get robust standard errors (Newey-West HAC estimator as in the paper)
    robust_se <- coeftest(model, vcov = vcovHAC(model))
    
    # Extract coefficients
    elasticity <- coef(model)[2]  # β coefficient
    std_error <- robust_se[2, 2]  # Robust standard error
    p_value <- robust_se[2, 4]    # P-value
    
    # Calculate 95% confidence interval
    ci_lower <- elasticity - 1.96 * std_error
    ci_upper <- elasticity + 1.96 * std_error
    
    # Model fit statistics
    r_squared <- summary(model)$r.squared
    n_obs <- nobs(model)
    
    # Return results
    return(tibble(
      elasticity = elasticity,
      std_error = std_error,
      p_value = p_value,
      ci_lower = ci_lower,
      ci_upper = ci_upper,
      r_squared = r_squared,
      n_obs = n_obs,
      significant = p_value < 0.05,
      status = "success"
    ))
  }, error = function(e) {
    return(tibble(
      elasticity = NA,
      std_error = NA,
      p_value = NA,
      ci_lower = NA,
      ci_upper = NA,
      r_squared = NA,
      n_obs = nrow(data),
      significant = NA,
      status = paste("error:", e$message)
    ))
  })
}

## Calculate territorial emissions elasticity for each state ####
# Calculating territorial emissions elasticities
territorial_elasticities <- data_clean %>%
  group_by(state_code) %>%
  do(calculate_elasticity(., "ln_terr")) %>%
  ungroup() %>%
  mutate(emission_type = "Territorial")

## Calculate consumption-based emissions elasticity for each state ####
# Calculating consumption-based emissions elasticities...
consumption_elasticities <- data_clean %>%
  group_by(state_code) %>%
  do(calculate_elasticity(., "ln_cons")) %>%
  ungroup() %>%
  mutate(emission_type = "Consumption-based")

# Check for any errors
errors_terr <- territorial_elasticities %>% filter(status != "success")
errors_cons <- consumption_elasticities %>% filter(status != "success")

if (nrow(errors_terr) > 0) {
  cat("\n⚠️ WARNING: Some territorial elasticity calculations failed:\n")
  print(errors_terr %>% select(state_code, status))
}
if (nrow(errors_cons) > 0) {
  cat("\n⚠️ WARNING: Some consumption elasticity calculations failed:\n")
  print(errors_cons %>% select(state_code, status))
}

# Combine results
all_elasticities <- bind_rows(territorial_elasticities, consumption_elasticities) %>%
  filter(status == "success")  # Keep only successful calculations

cat("\n✓ Elasticity calculations complete!\n")
cat("Successful calculations:", nrow(all_elasticities), "out of", 
    nrow(territorial_elasticities) + nrow(consumption_elasticities), "\n")

# 4. CLASSIFY DECOUPLING STATUS ####

classify_decoupling <- function(elasticity) {
  case_when(
    is.na(elasticity) ~ "Unknown",
    elasticity < 0 ~ "Absolute Decoupling",
    elasticity >= 0 & elasticity < 1 ~ "Relative Decoupling",
    elasticity >= 1 ~ "No Decoupling"
  )
}

all_elasticities <- all_elasticities %>%
  mutate(
    decoupling_status = classify_decoupling(elasticity),
    decoupling_category = factor(decoupling_status, 
                                 levels = c("Absolute Decoupling", 
                                            "Relative Decoupling", 
                                            "No Decoupling",
                                            "Unknown"))
  )

# 5. SUMMARY STATISTICS ####

cat("\n" , paste(rep("=", 70), collapse=""), "\n")
cat("SUMMARY: TERRITORIAL EMISSIONS ELASTICITIES (2012-2019)\n")
cat(paste(rep("=", 70), collapse=""), "\n\n")

territorial_summary <- territorial_elasticities %>%
  filter(status == "success") %>%
  summarise(
    n_states = n(),
    Mean = mean(elasticity, na.rm = TRUE),
    Median = median(elasticity, na.rm = TRUE),
    SD = sd(elasticity, na.rm = TRUE),
    Min = min(elasticity, na.rm = TRUE),
    Max = max(elasticity, na.rm = TRUE),
    `% Absolute (β<0)` = mean(elasticity < 0, na.rm = TRUE) * 100,
    `% Relative (0≤β<1)` = mean(elasticity >= 0 & elasticity < 1, na.rm = TRUE) * 100,
    `% No Decoupling (β≥1)` = mean(elasticity >= 1, na.rm = TRUE) * 100
  )

print(territorial_summary %>% mutate(across(where(is.numeric), ~round(., 3))))

cat("\n" , paste(rep("=", 70), collapse=""), "\n")
cat("SUMMARY: CONSUMPTION-BASED EMISSIONS ELASTICITIES (2012-2019)\n")
cat(paste(rep("=", 70), collapse=""), "\n\n")

consumption_summary <- consumption_elasticities %>%
  filter(status == "success") %>%
  summarise(
    n_states = n(),
    Mean = mean(elasticity, na.rm = TRUE),
    Median = median(elasticity, na.rm = TRUE),
    SD = sd(elasticity, na.rm = TRUE),
    Min = min(elasticity, na.rm = TRUE),
    Max = max(elasticity, na.rm = TRUE),
    `% Absolute (β<0)` = mean(elasticity < 0, na.rm = TRUE) * 100,
    `% Relative (0≤β<1)` = mean(elasticity >= 0 & elasticity < 1, na.rm = TRUE) * 100,
    `% No Decoupling (β≥1)` = mean(elasticity >= 1, na.rm = TRUE) * 100
  )

print(consumption_summary %>% mutate(across(where(is.numeric), ~round(., 3))))

# 6. DETAILED STATE-BY-STATE RESULTS ####

# Add decoupling status to territorial elasticities
territorial_elasticities <- territorial_elasticities %>%
  mutate(
    decoupling_status = classify_decoupling(elasticity),
    decoupling_category = factor(decoupling_status, 
                                 levels = c("Absolute Decoupling", 
                                            "Relative Decoupling", 
                                            "No Decoupling",
                                            "Unknown"))
  )

# Add decoupling status to consumption elasticities
consumption_elasticities <- consumption_elasticities %>%
  mutate(
    decoupling_status = classify_decoupling(elasticity),
    decoupling_category = factor(decoupling_status, 
                                 levels = c("Absolute Decoupling", 
                                            "Relative Decoupling", 
                                            "No Decoupling",
                                            "Unknown"))
  )

# Now create the comparison table (should work now!)
comparison_table <- territorial_elasticities %>%
  filter(status == "success") %>%
  select(state_code, territorial_elast = elasticity, 
         territorial_status = decoupling_status,
         territorial_pval = p_value,
         territorial_r2 = r_squared) %>%
  left_join(
    consumption_elasticities %>%
      filter(status == "success") %>%
      select(state_code, consumption_elast = elasticity,
             consumption_status = decoupling_status,
             consumption_pval = p_value,
             consumption_r2 = r_squared),
    by = "state_code"
  ) %>%
  mutate(
    difference = consumption_elast - territorial_elast,
    status_match = territorial_status == consumption_status
  ) %>%
  arrange(desc(difference))


# Create comparison table
comparison_table <- territorial_elasticities %>%
  filter(status == "success") %>%
  select(state_code, territorial_elast = elasticity, 
         territorial_status = decoupling_status,
         territorial_pval = p_value,
         territorial_r2 = r_squared) %>%
  left_join(
    consumption_elasticities %>%
      filter(status == "success") %>%
      select(state_code, consumption_elast = elasticity,
             consumption_status = decoupling_status,
             consumption_pval = p_value,
             consumption_r2 = r_squared),
    by = "state_code"
  ) %>%
  mutate(
    difference = consumption_elast - territorial_elast,
    status_match = territorial_status == consumption_status
  ) %>%
  arrange(desc(difference))

cat("\n" , paste(rep("=", 70), collapse=""), "\n")
cat("TOP 10 STATES: LARGEST POSITIVE DIFFERENCE (Consumption - Territorial)\n")
cat("= Consumption elasticity HIGHER (potential emission outsourcing)\n")
cat(paste(rep("=", 70), collapse=""), "\n\n")

print(comparison_table %>% 
        select(state_code, territorial_elast, consumption_elast, difference) %>%
        head(10) %>%
        mutate(across(where(is.numeric), ~round(., 3))), 
      n = 10)

cat("\n" , paste(rep("=", 70), collapse=""), "\n")
cat("TOP 10 STATES: LARGEST NEGATIVE DIFFERENCE (Consumption - Territorial)\n")
cat("= Territorial elasticity HIGHER (emission exporting states)\n")
cat(paste(rep("=", 70), collapse=""), "\n\n")

print(comparison_table %>% 
        select(state_code, territorial_elast, consumption_elast, difference) %>%
        tail(10) %>%
        mutate(across(where(is.numeric), ~round(., 3))), 
      n = 10)

# 7. VISUALIZATIONS ####

# Set up color scheme for decoupling categories
decoupling_colors <- c("Absolute Decoupling" = "#2ecc71",    # Green
                       "Relative Decoupling" = "#f39c12",     # Orange
                       "No Decoupling" = "#e74c3c")           # Red

# Plot 1: Distribution of elasticities
plot1 <- ggplot(all_elasticities, aes(x = elasticity, fill = emission_type)) +
  geom_histogram(alpha = 0.6, position = "identity", bins = 15, color = "white") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "black", linewidth = 1) +
  geom_vline(xintercept = 1, linetype = "dashed", color = "black", linewidth = 1) +
  annotate("text", x = 0, y = Inf, label = "β = 0\n(Absolute)", 
           vjust = 2, hjust = 1.1, size = 3, fontface = "bold") +
  annotate("text", x = 1, y = Inf, label = "β = 1\n(Relative)", 
           vjust = 2, hjust = -0.1, size = 3, fontface = "bold") +
  scale_fill_manual(values = c("Territorial" = "#3498db", 
                               "Consumption-based" = "#9b59b6")) +
  labs(title = "Distribution of Emission-GDP Elasticities (2012-2019)",
       subtitle = "US States: Territorial vs. Consumption-based Emissions",
       x = "Income Elasticity of Emissions (β)",
       y = "Number of States",
       fill = "Emission Type",
       caption = "Following Freire-González et al. (2024) methodology") +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom",
        plot.title = element_text(face = "bold", size = 14),
        plot.subtitle = element_text(size = 11),
        legend.title = element_text(face = "bold"))

plot2 <- ggplot(comparison_table,
                aes(x = territorial_elast, y = consumption_elast)) +

  # CHOOSE ONE OF THESE TWO SHADINGS:
#   (a) absolute decoupling in both: β_terr < 0 AND β_cons < 0
#   (b) weak+absolute decoupling in both: β_terr < 1 AND β_cons < 1

# (a) absolute decoupling zone (both β < 0)
annotate("rect",
         xmin = -Inf, xmax = 0,
         ymin = -Inf, ymax = 0,
         fill = "limegreen", alpha = 0.1) +
  
  # If instead you want weak+absolute decoupling (<1), comment the block above
  # and uncomment this one:
  # annotate("rect",
  #          xmin = -Inf, xmax = 1,
  #          ymin = -Inf, ymax = 1,
  #          fill = "limegreen", alpha = 0.1) +
  
  # 45° line = perfect agreement
  geom_abline(intercept = 0, slope = 1,
              linetype = "dashed", color = "gray50", linewidth = 1) +
  
  # Points and labels
  geom_point(size = 3, alpha = 0.7, color = "#3498db") +
  geom_text(aes(label = state_code),
            vjust = -0.7, size = 2.5,
            color = "gray30", fontface = "bold") +
  
  # Threshold lines: absolute (0) and weak-decoupling (1)
  geom_hline(yintercept = 0, linetype = "dotted",
             color = "black", alpha = 0.5) +
  geom_vline(xintercept = 0, linetype = "dotted",
             color = "black", alpha = 0.5) +
  geom_hline(yintercept = 1, linetype = "dotted",
             color = "red", alpha = 0.3) +
  geom_vline(xintercept = 1, linetype = "red",
             linetype = "dotted", alpha = 0.3) +
  
  labs(
    title    = "Territorial vs. Consumption-Based Elasticities",
    subtitle = "Each point represents one US state (2012–2019)",
    x        = "Territorial emissions elasticity (β)",
    y        = "Consumption-based emissions elasticity (β)",
    caption  = "Dashed 45° line = perfect agreement. Green shaded area = states with decoupling in both accounts (here: β < 0 in both)."
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title    = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 11)
  )
# (optional but helps avoid stale versions)
rm(plot2)

plot2 <- ggplot(comparison_table,
                aes(x = territorial_elast, y = consumption_elast)) +
  # Shaded zone: absolute decoupling in both accounts (β < 0)
  annotate("rect",
           xmin = -Inf, xmax = 0,
           ymin = -Inf, ymax = 0,
           fill = "limegreen", alpha = 0.1) +
  # If you instead want β < 1 in both accounts, use this:
  # annotate("rect",
  #          xmin = -Inf, xmax = 1,
  #          ymin = -Inf, ymax = 1,
  #          fill = "limegreen", alpha = 0.1) +
  
  # 45° line = perfect agreement
  geom_abline(intercept = 0, slope = 1,
              linetype = 2,  # dashed
              colour = "grey50", linewidth = 1) +
  
  # Points and labels
  geom_point(size = 3, alpha = 0.7, colour = "#3498db") +
  geom_text(aes(label = state_code),
            vjust = -0.7, size = 2.5,
            colour = "grey30", fontface = "bold") +
  
  # Threshold lines β = 0 (black dotted) and β = 1 (red dotted)
  geom_hline(yintercept = 0, linetype = 3, colour = "black", alpha = 0.5) +
  geom_vline(xintercept = 0, linetype = 3, colour = "black", alpha = 0.5) +
  geom_hline(yintercept = 1, linetype = 3, colour = "red",   alpha = 0.3) +
  geom_vline(xintercept = 1, linetype = 3, colour = "red",   alpha = 0.3) +
  
  labs(
    title    = "Territorial vs. Consumption-Based Elasticities",
    subtitle = "Each point represents one US state (2012–2019)",
    x        = "Territorial emissions elasticity (β)",
    y        = "Consumption-based emissions elasticity (β)",
    caption  = "45° dashed line = perfect agreement. Green shaded area = states with absolute decoupling in both accounts (β < 0)."
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title    = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 11)
  )

ggsave("territorial_vs_consumption.png", plot2,
       width = 10, height = 10, dpi = 300)


# Plot 3: Bar chart of decoupling categories
decoupling_counts <- all_elasticities %>%
  count(emission_type, decoupling_category) %>%
  group_by(emission_type) %>%
  mutate(percentage = n / sum(n) * 100)

plot3 <- ggplot(decoupling_counts, 
                aes(x = emission_type, y = percentage, fill = decoupling_category)) +
  geom_bar(stat = "identity", position = "stack", color = "white", linewidth = 0.5) +
  geom_text(aes(label = sprintf("%d states\n(%.1f%%)", n, percentage)),
            position = position_stack(vjust = 0.5), 
            color = "white", fontface = "bold", size = 3.5) +
  scale_fill_manual(values = decoupling_colors) +
  labs(title = "Decoupling Status by Emission Accounting Method",
       subtitle = "US States, 2012-2019",
       x = NULL,
       y = "Percentage of States (%)",
       fill = "Decoupling Status") +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom",
        plot.title = element_text(face = "bold", size = 14),
        plot.subtitle = element_text(size = 11),
        legend.title = element_text(face = "bold"))

# Plot 4: Difference plot (all states ranked)
plot4 <- ggplot(comparison_table, 
                aes(x = reorder(state_code, difference), y = difference)) +
  geom_hline(yintercept = 0, linetype = "solid", 
             color = "gray30", linewidth = 1) +
  geom_segment(aes(xend = state_code, y = 0, yend = difference), 
               color = "gray70", linewidth = 0.5) +
  geom_point(aes(color = difference > 0), size = 3) +
  scale_color_manual(
    values = c("TRUE" = "#e74c3c", "FALSE" = "#3498db"),
    labels = c("Territorial higher (exporting)", 
               "Consumption higher (outsourcing)"),
    name = NULL
  ) +
  coord_flip() +
  labs(title = "Elasticity Difference by State (Ranked)",
       subtitle = "Δ = β_consumption - β_territorial",
       x = "State",
       y = "Difference in Elasticity",
       caption = "Positive = Potential emission outsourcing | Negative = Potential emission exporting") +
  theme_minimal(base_size = 10) +
  theme(plot.title = element_text(face = "bold", size = 14),
        plot.subtitle = element_text(size = 11),
        legend.position = "bottom",
        legend.text = element_text(size = 9))

# Save plots
cat("Saving plots...\n")
ggsave("elasticity_distribution.png", plot1, width = 10, height = 6, dpi = 300)
ggsave("territorial_vs_consumption.png", plot2,
       width = 10, height = 10, dpi = 300)
ggsave("decoupling_categories.png", plot3, width = 8, height = 6, dpi = 300)
ggsave("state_differences.png", plot4, width = 10, height = 14, dpi = 300)

cat("✓ All plots saved successfully!\n")

# 8. EXPORT RESULTS ####

# Export full results table
write.csv(all_elasticities, "exports/elasticity_results_full.csv", row.names = FALSE)
write.csv(comparison_table, "exports/elasticity_comparison.csv", row.names = FALSE)
write.xlsx(all_elasticities, "exports/elasticity_results_full.xlsx")
write.xlsx(comparison_table, "exports/elasticity_comparison.xlsx")

# Create formatted summary report
summary_report <- bind_rows(
  territorial_summary %>% mutate(Emission_Type = "Territorial", .before = 1),
  consumption_summary %>% mutate(Emission_Type = "Consumption-based", .before = 1)
)

write.csv(summary_report, "exports/elasticity_summary_report.csv", row.names = FALSE)
write.xlsx(summary_report, "exports/elasticity_summary_report.xlsx")

# Also save the cleaned data
write.csv(data_clean, "exports/data_clean_long_format.csv", row.names = FALSE)
write.csv(data_clean, "exports/data_clean_long_format.xlsx")

# 9. KEY FINDINGS INTERPRETATION ####

cat("\n" , paste(rep("=", 70), collapse=""), "\n")
cat("KEY FINDINGS & INTERPRETATION\n")
cat(paste(rep("=", 70), collapse=""), "\n\n")

cat("TERRITORIAL EMISSIONS:\n")
cat("  • States analyzed:", territorial_summary$n_states, "\n")
cat("  • Average elasticity: β =", round(territorial_summary$Mean, 3), "\n")
if (territorial_summary$Mean < 0) {
  cat("  → On average: ABSOLUTE decoupling ✓✓✓\n")
  cat("     (Emissions falling as GDP grows)\n")
} else if (territorial_summary$Mean < 1) {
  cat("  → On average: RELATIVE (weak) decoupling ✓✓\n")
  cat("     (Emissions growing slower than GDP)\n")
} else {
  cat("  → On average: NO decoupling ✗\n")
  cat("     (Emissions growing as fast or faster than GDP)\n")
}
cat("  • States with absolute decoupling (β<0):", 
    round(territorial_summary$`% Absolute (β<0)`, 1), "%\n")
cat("  • States with relative decoupling (0≤β<1):", 
    round(territorial_summary$`% Relative (0≤β<1)`, 1), "%\n")

cat("\nCONSUMPTION-BASED EMISSIONS:\n")
cat("  • States analyzed:", consumption_summary$n_states, "\n")
cat("  • Average elasticity: β =", round(consumption_summary$Mean, 3), "\n")
if (consumption_summary$Mean < 0) {
  cat("  → On average: ABSOLUTE decoupling ✓✓✓\n")
  cat("     (Emissions falling as GDP grows)\n")
} else if (consumption_summary$Mean < 1) {
  cat("  → On average: RELATIVE (weak) decoupling ✓✓\n")
  cat("     (Emissions growing slower than GDP)\n")
} else {
  cat("  → On average: NO decoupling ✗\n")
  cat("     (Emissions growing as fast or faster than GDP)\n")
}
cat("  • States with absolute decoupling (β<0):", 
    round(consumption_summary$`% Absolute (β<0)`, 1), "%\n")
cat("  • States with relative decoupling (0≤β<1):", 
    round(consumption_summary$`% Relative (0≤β<1)`, 1), "%\n")

mean_diff <- consumption_summary$Mean - territorial_summary$Mean
cat("\nCOMPARISON:\n")
cat("  • Average difference: Δβ =", round(mean_diff, 3), "\n")
cat("    (consumption - territorial)\n")
if (mean_diff > 0.1) {
  cat("  → Consumption elasticity is notably HIGHER ⚠️\n")
  cat("  → This suggests emission OUTSOURCING may be occurring\n")
  cat("  → Territorial decoupling may be partly ILLUSORY\n")
  cat("  → States may be importing emission-intensive goods\n")
} else if (mean_diff < -0.1) {
  cat("  → Territorial elasticity is notably HIGHER\n")
  cat("  → States may be EXPORTING emissions through production\n")
  cat("  → (e.g., energy-producing states like WY, ND)\n")
} else {
  cat("  → Elasticities are SIMILAR (within ±0.1) ✓\n")
  cat("  → Decoupling appears GENUINE, not due to outsourcing\n")
  cat("  → Both accounting methods agree\n")
}

# States with mismatched status
mismatched <- comparison_table %>% filter(!status_match)
if (nrow(mismatched) > 0) {
  cat("\nSTATES WITH CONFLICTING STATUS:\n")
  cat("  ", nrow(mismatched), "states show different decoupling status\n")
  cat("  between territorial and consumption accounting.\n")
  cat("  These states warrant further investigation.\n\n")
  print(mismatched %>% 
          select(state_code, territorial_status, consumption_status, difference) %>%
          arrange(desc(abs(difference))))
}

# Highlight notable states
top_decouple <- comparison_table %>% 
  filter(territorial_elast < 0 & consumption_elast < 0) %>%
  arrange(territorial_elast)

if (nrow(top_decouple) > 0) {
  cat("\n🌟 STRONG PERFORMERS (Absolute decoupling in BOTH):\n")
  print(top_decouple %>% 
          select(state_code, territorial_elast, consumption_elast) %>%
          head(5) %>%
          mutate(across(where(is.numeric), ~round(., 3))))
}

# 10. T-TEST ####
paired_test <- t.test(comparison_table$territorial_elast, 
                      comparison_table$consumption_elast, 
                      paired = TRUE)
print(paired_test)

# Also test without outliers
comparison_no_outliers <- comparison_table %>%
  filter(!state_code %in% c("KY", "AK", "ND"))

paired_test_robust <- t.test(comparison_no_outliers$territorial_elast,
                             comparison_no_outliers$consumption_elast,
                             paired = TRUE)
print(paired_test_robust)

wilcox.test(comparison_table$consumption_elast,
            comparison_table$territorial_elast,
            paired = TRUE)
