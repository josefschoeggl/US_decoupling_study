# ROBUSTNESS CHECKS 

# This script provides additional analyses:
# 1. Model fit diagnostics (R-squared, residual plots)
# 2. Sensitivity to time period (first half vs second half)
# 3. Confidence interval visualization
# 4. Statistical power analysis
# 5. Outlier sensitivity
# 6. Alternative standard errors comparison
# Prerequisites: Run calculate_elasticities.R first

# load libaries
library(tidyverse)
library(sandwich)
library(lmtest)
library(gridExtra)
library(broom)

# Check that main analysis has been run
if (!exists("data_clean")) {
  stop("Please run calculate_elasticities_FINAL.R first!")
}

# 1. MODEL FIT DIAGNOSTICS ####

# Function to get detailed diagnostics for each state
get_diagnostics <- function(data, emission_var) {
  if (nrow(data) < 5) return(NULL)
  
  model <- lm(as.formula(paste(emission_var, "~ ln_gdp")), data = data)
  
  # Get various statistics
  tibble(
    r_squared = summary(model)$r.squared,
    adj_r_squared = summary(model)$adj.r.squared,
    rmse = sqrt(mean(residuals(model)^2)),
    max_abs_resid = max(abs(residuals(model))),
    durbin_watson = lmtest::dwtest(model)$statistic,
    n_obs = nobs(model)
  )
}

# Calculate diagnostics for each state
terr_diagnostics <- data_clean %>%
  group_by(state_code) %>%
  do(get_diagnostics(., "ln_terr")) %>%
  ungroup() %>%
  filter(!is.na(r_squared))

cons_diagnostics <- data_clean %>%
  group_by(state_code) %>%
  do(get_diagnostics(., "ln_cons")) %>%
  ungroup() %>%
  filter(!is.na(r_squared))

# Summary of model fit
cat("\nTERRITORIAL EMISSIONS - Model Fit Summary:\n")
cat("Mean R²:", round(mean(terr_diagnostics$r_squared), 3), "\n")
cat("Median R²:", round(median(terr_diagnostics$r_squared), 3), "\n")
cat("States with R² > 0.5:", sum(terr_diagnostics$r_squared > 0.5), "out of", nrow(terr_diagnostics), "\n")
cat("States with R² < 0.3:", sum(terr_diagnostics$r_squared < 0.3), "\n")

cat("\nCONSUMPTION EMISSIONS - Model Fit Summary:\n")
cat("Mean R²:", round(mean(cons_diagnostics$r_squared), 3), "\n")
cat("Median R²:", round(median(cons_diagnostics$r_squared), 3), "\n")
cat("States with R² > 0.5:", sum(cons_diagnostics$r_squared > 0.5), "out of", nrow(cons_diagnostics), "\n")
cat("States with R² < 0.3:", sum(cons_diagnostics$r_squared < 0.3), "\n")

# Identify states with poor fit
poor_fit_terr <- terr_diagnostics %>% 
  filter(r_squared < 0.3) %>%
  arrange(r_squared)

poor_fit_cons <- cons_diagnostics %>% 
  filter(r_squared < 0.3) %>%
  arrange(r_squared)

if (nrow(poor_fit_terr) > 0) {
  cat("\nStates with poor territorial model fit (R² < 0.3):\n")
  print(poor_fit_terr %>% select(state_code, r_squared))
}

if (nrow(poor_fit_cons) > 0) {
  cat("\nStates with poor consumption model fit (R² < 0.3):\n")
  print(poor_fit_cons %>% select(state_code, r_squared))
}

# 2. SENSITIVITY TO TIME PERIOD (Sub-period Analysis) ####

cat("\n=== SENSITIVITY TO TIME PERIOD ===\n")
cat("Comparing 2012-2015 vs 2016-2019\n")
cat("Note: With only 4 observations per period, results are exploratory\n\n")

# Function to calculate elasticity for sub-period (simplified)
calc_elasticity_simple <- function(data, emission_var) {
  if (nrow(data) < 4) return(NA_real_)
  
  tryCatch({
    model <- lm(as.formula(paste(emission_var, "~ ln_gdp")), data = data)
    return(as.numeric(coef(model)[2]))
  }, error = function(e) NA_real_)
}

# Split data into periods and calculate separately (avoids dplyr issues)
period1 <- data_clean %>% filter(year >= 2012 & year <= 2015)
period2 <- data_clean %>% filter(year >= 2016 & year <= 2019)

# Calculate elasticities for period 1 (2012-2015)
elast_p1 <- period1 %>%
  group_by(state_code) %>%
  summarise(
    terr_2012_2015 = calc_elasticity_simple(cur_data(), "ln_terr"),
    cons_2012_2015 = calc_elasticity_simple(cur_data(), "ln_cons"),
    .groups = "drop"
  )

# Calculate elasticities for period 2 (2016-2019)
elast_p2 <- period2 %>%
  group_by(state_code) %>%
  summarise(
    terr_2016_2019 = calc_elasticity_simple(cur_data(), "ln_terr"),
    cons_2016_2019 = calc_elasticity_simple(cur_data(), "ln_cons"),
    .groups = "drop"
  )

# Combine and calculate changes
subperiod_comparison <- elast_p1 %>%
  left_join(elast_p2, by = "state_code") %>%
  mutate(
    terr_change = terr_2016_2019 - terr_2012_2015,
    cons_change = cons_2016_2019 - cons_2012_2015
  )

cat("TERRITORIAL EMISSIONS - Period Comparison:\n")
cat("States with MORE negative elasticity in 2016-2019 (improving):",
    sum(subperiod_comparison$terr_change < 0, na.rm = TRUE), "\n")
cat("States with LESS negative elasticity in 2016-2019 (worsening):",
    sum(subperiod_comparison$terr_change > 0, na.rm = TRUE), "\n")

cat("\nCONSUMPTION EMISSIONS - Period Comparison:\n")
cat("States with MORE negative elasticity in 2016-2019 (improving):",
    sum(subperiod_comparison$cons_change < 0, na.rm = TRUE), "\n")
cat("States with LESS negative elasticity in 2016-2019 (worsening):",
    sum(subperiod_comparison$cons_change > 0, na.rm = TRUE), "\n")

# States with biggest improvements
cat("\nStates with largest territorial improvement (most negative change):\n")
print(subperiod_comparison %>% 
        filter(!is.na(terr_change)) %>%
        arrange(terr_change) %>%
        select(state_code, terr_2012_2015, terr_2016_2019, terr_change) %>%
        head(5))

# 3. CONFIDENCE INTERVAL VISUALIZATION ####

# Merge elasticities with confidence intervals
if (exists("territorial_elasticities") && exists("consumption_elasticities")) {
  
  # Plot territorial with confidence intervals
  plot_ci_terr <- territorial_elasticities %>%
    filter(status == "success") %>%
    arrange(elasticity) %>%
    mutate(state_rank = row_number()) %>%
    ggplot(aes(x = state_rank, y = elasticity)) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "black", linewidth = 1) +
    geom_hline(yintercept = 1, linetype = "dashed", color = "red", linewidth = 0.5) +
    geom_ribbon(aes(ymin = ci_lower, ymax = ci_upper), alpha = 0.2, fill = "#3498db") +
    geom_line(color = "#3498db", linewidth = 1) +
    geom_point(aes(color = significant), size = 2) +
    scale_color_manual(values = c("TRUE" = "#2ecc71", "FALSE" = "#e74c3c"),
                       labels = c("TRUE" = "Significant (p<0.05)", "FALSE" = "Not significant"),
                       name = NULL) +
    labs(title = "Territorial Emissions Elasticities with 95% Confidence Intervals",
         subtitle = "States ranked by elasticity (most negative to most positive)",
         x = "State (ranked)",
         y = "Elasticity (β)",
         caption = "Shaded area shows 95% confidence interval. Green = significant, Red = not significant.") +
    theme_minimal(base_size = 14) +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, size=11),
          # axis.line.x = element_line(color = "grey", linewidth = 0.5),
          # axis.line.y = element_line(color = "grey", linewidth = 0.5),
          # panel.grid.major.x = element_blank(),
          # panel.grid.minor = element_blank(),
          plot.title = element_text( hjust = 0.5,face = "bold", size = 18),
          plot.subtitle = element_text(hjust = 0.5),
          legend.text = element_text(size = 12),
          legend.position = "bottom")
  
  print(plot_ci_terr)
  
  # Plot consumption with confidence intervals
  plot_ci_cons <- consumption_elasticities %>%
    filter(status == "success") %>%
    arrange(elasticity) %>%
    mutate(state_rank = row_number()) %>%
    ggplot(aes(x = state_rank, y = elasticity)) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "black", linewidth = 1) +
    geom_hline(yintercept = 1, linetype = "dashed", color = "red", linewidth = 0.5) +
    geom_ribbon(aes(ymin = ci_lower, ymax = ci_upper), alpha = 0.2, fill = "#9b59b6") +
    geom_line(color = "#9b59b6", linewidth = 1) +
    geom_point(aes(color = significant), size = 2) +
    scale_color_manual(values = c("TRUE" = "#2ecc71", "FALSE" = "#e74c3c"),
                       labels = c("TRUE" = "Significant (p<0.05)", "FALSE" = "Not significant"),
                       name = NULL) +
    labs(title = "Consumption-Based Emissions Elasticities with 95% Confidence Intervals",
         subtitle = "States ranked by elasticity (most negative to most positive)",
         x = "State (ranked)",
         y = "Elasticity (β)",
         caption = "Shaded area shows 95% confidence interval. Green = significant, Red = not significant.") +
    theme_minimal() +
    theme(legend.position = "bottom",
          plot.title = element_text(face = "bold"))
  
  print(plot_ci_cons)
  
  ggsave("territorial_elasticity_with_CI.pdf", plot_ci_terr, width = 12, height = 6)
  ggsave("consumption_elasticity_with_CI.pdf", plot_ci_cons, width = 12, height = 6)
  
  # ggsave("territorial_elasticity_with_CI.png", plot_ci_terr, width = 12, height = 6, dpi = 300)
  # ggsave("consumption_elasticity_with_CI.png", plot_ci_cons, width = 12, height = 6, dpi = 300)
  
  cat("✓ Confidence interval plots saved\n")
}

# 4. STATISTICAL SIGNIFICANCE ANALYSIS ####

cat("\n=== STATISTICAL SIGNIFICANCE SUMMARY ===\n")

if (exists("territorial_elasticities")) {
  sig_terr <- territorial_elasticities %>%
    filter(status == "success") %>%
    summarise(
      n_total = n(),
      n_significant = sum(significant),
      pct_significant = mean(significant) * 100,
      n_sig_negative = sum(significant & elasticity < 0),
      n_sig_positive = sum(significant & elasticity > 0)
    )
  
  cat("TERRITORIAL EMISSIONS:\n")
  cat("  Total states:", sig_terr$n_total, "\n")
  cat("  Statistically significant (p<0.05):", sig_terr$n_significant, 
      "(", round(sig_terr$pct_significant, 1), "%)\n")
  cat("    - Significant & negative:", sig_terr$n_sig_negative, "\n")
  cat("    - Significant & positive:", sig_terr$n_sig_positive, "\n")
}

if (exists("consumption_elasticities")) {
  sig_cons <- consumption_elasticities %>%
    filter(status == "success") %>%
    summarise(
      n_total = n(),
      n_significant = sum(significant),
      pct_significant = mean(significant) * 100,
      n_sig_negative = sum(significant & elasticity < 0),
      n_sig_positive = sum(significant & elasticity > 0)
    )
  
  cat("\nCONSUMPTION EMISSIONS:\n")
  cat("  Total states:", sig_cons$n_total, "\n")
  cat("  Statistically significant (p<0.05):", sig_cons$n_significant, 
      "(", round(sig_cons$pct_significant, 1), "%)\n")
  cat("    - Significant & negative:", sig_cons$n_sig_negative, "\n")
  cat("    - Significant & positive:", sig_cons$n_sig_positive, "\n")
}

# 5. OUTLIER SENSITIVITY ANALYSIS ####

cat("\n=== OUTLIER SENSITIVITY ANALYSIS ===\n")

# Identify outliers (elasticity > 2 SD from mean)
if (exists("territorial_elasticities") && exists("consumption_elasticities")) {
  
  terr_mean <- mean(territorial_elasticities$elasticity, na.rm = TRUE)
  terr_sd <- sd(territorial_elasticities$elasticity, na.rm = TRUE)
  
  cons_mean <- mean(consumption_elasticities$elasticity, na.rm = TRUE)
  cons_sd <- sd(consumption_elasticities$elasticity, na.rm = TRUE)
  
  terr_outliers <- territorial_elasticities %>%
    filter(abs(elasticity - terr_mean) > 2 * terr_sd) %>%
    select(state_code, elasticity, p_value, r_squared)
  
  cons_outliers <- consumption_elasticities %>%
    filter(abs(elasticity - cons_mean) > 2 * cons_sd) %>%
    select(state_code, elasticity, p_value, r_squared)
  
  cat("TERRITORIAL OUTLIERS (>2 SD from mean):\n")
  if (nrow(terr_outliers) > 0) {
    print(terr_outliers)
  } else {
    cat("  No outliers detected\n")
  }
  
  cat("\nCONSUMPTION OUTLIERS (>2 SD from mean):\n")
  if (nrow(cons_outliers) > 0) {
    print(cons_outliers)
  } else {
    cat("  No outliers detected\n")
  }
  
  # Recalculate summary without outliers
  cat("\nSUMMARY WITHOUT OUTLIERS:\n")
  
  terr_no_outliers <- territorial_elasticities %>%
    filter(abs(elasticity - terr_mean) <= 2 * terr_sd) %>%
    summarise(
      n = n(),
      mean = mean(elasticity),
      median = median(elasticity),
      sd = sd(elasticity)
    )
  
  cons_no_outliers <- consumption_elasticities %>%
    filter(abs(elasticity - cons_mean) <= 2 * cons_sd) %>%
    summarise(
      n = n(),
      mean = mean(elasticity),
      median = median(elasticity),
      sd = sd(elasticity)
    )
  
  cat("Territorial (excluding", nrow(terr_outliers), "outliers):\n")
  cat("  Mean:", round(terr_no_outliers$mean, 3), 
      "(original:", round(terr_mean, 3), ")\n")
  
  cat("Consumption (excluding", nrow(cons_outliers), "outliers):\n")
  cat("  Mean:", round(cons_no_outliers$mean, 3), 
      "(original:", round(cons_mean, 3), ")\n")
}


# 6. ALTERNATIVE STANDARD ERRORS COMPARISON ####


cat("\n=== COMPARING STANDARD ERROR METHODS ===\n")

# Function to compare different SE methods
compare_se_methods <- function(data, emission_var, state_name) {
  if (nrow(data) < 5) return(NULL)
  
  model <- lm(as.formula(paste(emission_var, "~ ln_gdp")), data = data)
  
  # Classical OLS
  se_classical <- summary(model)$coefficients[2, 2]
  
  # Robust (HC1)
  se_robust <- coeftest(model, vcov = vcovHC(model, type = "HC1"))[2, 2]
  
  # Newey-West HAC
  se_hac <- coeftest(model, vcov = vcovHAC(model))[2, 2]
  
  tibble(
    state = state_name,
    classical = se_classical,
    robust_HC1 = se_robust,
    newey_west = se_hac,
    elasticity = coef(model)[2]
  )
}

# Compare for a few example states
example_states <- c("CA", "TX", "NY", "FL", "IL")
se_comparison <- data_clean %>%
  filter(state_code %in% example_states) %>%
  group_by(state_code) %>%
  do(compare_se_methods(., "ln_terr", first(.$state_code))) %>%
  ungroup()

cat("Standard Error Comparison (Territorial, Selected States):\n")
print(se_comparison %>%
        mutate(across(where(is.numeric), ~round(., 4))))

cat("\nNote: Our main analysis uses Newey-West (HAC) standard errors as in Freire-González et al.\n")

# 7. EXPORT ROBUSTNESS CHECK RESULTS ####


# Export diagnostics
write.csv(terr_diagnostics, "exports/diagnostics_territorial.csv", row.names = FALSE)
write.csv(cons_diagnostics, "exports/diagnostics_consumption.csv", row.names = FALSE)
write.csv(subperiod_comparison, "exports/subperiod_comparison.csv", row.names = FALSE)

# Create summary report
robustness_summary <- tibble(
  Check = c("Mean R² - Territorial", "Mean R² - Consumption",
            "% States with R² > 0.5 - Territorial", "% States with R² > 0.5 - Consumption",
            "% Significant elasticities - Territorial", "% Significant elasticities - Consumption",
            "Territorial outliers (>2 SD)", "Consumption outliers (>2 SD)",
            "States improving 2012-15 to 2016-19 (Territorial)", 
            "States improving 2012-15 to 2016-19 (Consumption)"),
  Value = c(
    round(mean(terr_diagnostics$r_squared), 3),
    round(mean(cons_diagnostics$r_squared), 3),
    round(sum(terr_diagnostics$r_squared > 0.5) / nrow(terr_diagnostics) * 100, 1),
    round(sum(cons_diagnostics$r_squared > 0.5) / nrow(cons_diagnostics) * 100, 1),
    round(sig_terr$pct_significant, 1),
    round(sig_cons$pct_significant, 1),
    nrow(terr_outliers),
    nrow(cons_outliers),
    sum(subperiod_comparison$terr_change < 0, na.rm = TRUE),
    sum(subperiod_comparison$cons_change < 0, na.rm = TRUE)
  )
)

write.csv(robustness_summary, "exports/robustness_summary.csv", row.names = FALSE)

cat("✓ Files created:\n")
cat("  1. diagnostics_territorial.csv\n")
cat("  2. diagnostics_consumption.csv\n")
cat("  3. subperiod_comparison.csv\n")
cat("  4. robustness_summary.csv\n")
cat("  5. territorial_elasticity_with_CI.png\n")
cat("  6. consumption_elasticity_with_CI.png\n")

