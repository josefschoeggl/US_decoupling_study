# DECOUPLING STATUS FIGURE - SIGNIFICANT RESULTS ONLY ####
# More conservative version showing only statistically significant elasticities

library(tidyverse)

# Check data exists
if (!exists("all_elasticities")) {
  stop("Please run calculate_elasticities_FINAL.R first!")
}

# Create plots directory if it doesn't exist
if (!dir.exists("plots")) {
  dir.create("plots")
}

# VERSION 1: SIGNIFICANT ONLY (Conservative) ####

# Filter to only significant results
sig_only <- all_elasticities %>%
  filter(significant == TRUE)

# Count by category
decoupling_counts_sig <- sig_only %>%
  count(emission_type, decoupling_category) %>%
  group_by(emission_type) %>%
  mutate(
    percentage = n / sum(n) * 100,
    total = sum(n)
  ) %>%
  ungroup()

# Create plot - significant only
decoupling_counts_sig$emission_type <- factor(
  decoupling_counts_sig$emission_type,
  levels = c("Territorial", "Consumption-based"),
  labels = c("Territorial Emissions", "Consumption-based Emissions"))

plot_sig_only <- ggplot(decoupling_counts_sig, 
                        aes(x = emission_type, y = percentage, fill = decoupling_category)) +
  geom_bar(stat = "identity", position = "stack", color = "white", linewidth = 0.5) +
  geom_text(aes(label = sprintf("%d states\n(%.1f%%)", n, percentage)),
            position = position_stack(vjust = 0.5), 
            color = "white", fontface = "bold", size = 4) +
  scale_fill_manual(values = c(
    "Absolute Decoupling" = "#2ecc71",
    "Relative Decoupling" = "#f39c12",
    "No Decoupling" = "#e74c3c"
  )) +
  labs(
    title = "Decoupling Status - Statistically Significant Results Only",
    subtitle = sprintf("U.S. States: 2012-2019 (Territorial: n=%d, Consumption: n=%d)",
                       sum(decoupling_counts_sig$emission_type == "Territorial Emissions" & decoupling_counts_sig$n > 0),
                       sum(decoupling_counts_sig$emission_type == "Consumption-based Emissions" & decoupling_counts_sig$n > 0)),
    x = NULL,
    y = "Percentage of States (%)",
    fill = "Decoupling Status"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "bottom",
    plot.title = element_text(face = "bold", size = 18, hjust = 0.5),
    plot.subtitle = element_text(size = 14, hjust = 0.5),
    legend.title = element_text(face = "bold", size = 12),
    plot.caption = element_text(size = 9, color = "gray30"),
    axis.title.y = element_text(size = 12, face = "bold"),
    axis.text.x = element_text(size = 12, face = "bold"),
    axis.text.y = element_text(size = 12)
  )

print(plot_sig_only)

ggsave("plots/decoupling_categories_SIGNIFICANT_ONLY.pdf", plot_sig_only, 
       width = 10, height = 7)
ggsave("plots/decoupling_categories_SIGNIFICANT_ONLY.png", plot_sig_only, 
       width = 8, height = 6, dpi = 300)

# VERSION 2: SIDE-BY-SIDE (All vs Significant) ####

# Prepare data for comparison
all_counts <- all_elasticities %>%
  count(emission_type, decoupling_category) %>%
  group_by(emission_type) %>%
  mutate(
    percentage = n / sum(n) * 100,
    type = "All States (n=50)"
  )

sig_counts <- sig_only %>%
  count(emission_type, decoupling_category) %>%
  group_by(emission_type) %>%
  mutate(
    percentage = n / sum(n) * 100,
    type = sprintf("Significant Only\n(Terr: n=%d, Cons: n=%d)",
                   sum(sig_only$emission_type == "Territorial"),
                   sum(sig_only$emission_type == "Consumption-based"))
  )

# Combine
comparison_data <- bind_rows(all_counts, sig_counts) %>%
  mutate(
    facet_label = paste(emission_type, type, sep = "\n")
  )

# Plot comparison
plot_comparison <- ggplot(comparison_data,
                          aes(x = type, y = percentage, fill = decoupling_category)) +
  geom_bar(stat = "identity", position = "stack", color = "white", linewidth = 0.5) +
  geom_text(aes(label = sprintf("%d\n(%.0f%%)", n, percentage)),
            position = position_stack(vjust = 0.5), 
            color = "white", fontface = "bold", size = 3) +
  scale_fill_manual(values = c(
    "Absolute Decoupling" = "#2ecc71",
    "Relative Decoupling" = "#f39c12",
    "No Decoupling" = "#e74c3c"
  )) +
  facet_wrap(~emission_type, nrow = 1) +
  labs(
    title = "Decoupling Status: All States vs. Significant Results Only",
    subtitle = "US States, 2012-2019",
    x = NULL,
    y = "Percentage of States (%)",
    fill = "Decoupling Status",
    caption = "Left bars show all 50 states. Right bars show only statistically significant results (p<0.05)."
  ) +
  theme_minimal(base_size = 11) +
  theme(
    legend.position = "bottom",
    plot.title = element_text(face = "bold", size = 13),
    plot.subtitle = element_text(size = 10),
    legend.title = element_text(face = "bold"),
    axis.text.x = element_text(size = 9),
    plot.caption = element_text(hjust = 0, size = 9, color = "gray30")
  )

print(plot_comparison)

ggsave("plots/decoupling_comparison_ALL_vs_SIGNIFICANT.png", plot_comparison, 
       width = 10, height = 6, dpi = 300)

# VERSION 3: WITH SIGNIFICANCE SHADING ####

# Add significance breakdown
detailed_counts <- all_elasticities %>%
  mutate(
    sig_label = ifelse(significant, "Significant", "Not Significant")
  ) %>%
  count(emission_type, decoupling_category, sig_label) %>%
  group_by(emission_type) %>%
  mutate(percentage = n / 50 * 100) %>%  # Use 50 as denominator for consistency
  ungroup()

plot_detailed <- ggplot(detailed_counts,
                        aes(x = emission_type, y = percentage, 
                            fill = decoupling_category, alpha = sig_label)) +
  geom_bar(stat = "identity", position = "stack", color = "white", linewidth = 0.3) +
  geom_text(
    data = detailed_counts %>% filter(sig_label == "Significant"),
    aes(label = sprintf("%d", n)),
    position = position_stack(vjust = 0.5),
    color = "white", fontface = "bold", size = 3.5
  ) +
  scale_fill_manual(values = c(
    "Absolute Decoupling" = "#2ecc71",
    "Relative Decoupling" = "#f39c12",
    "No Decoupling" = "#e74c3c"
  )) +
  scale_alpha_manual(values = c("Significant" = 1.0, "Not Significant" = 0.3),
                     name = "Statistical\nSignificance") +
  labs(
    title = "Decoupling Status with Statistical Significance Indicated",
    subtitle = "US States, 2012-2019",
    x = NULL,
    y = "Percentage of States (%)",
    fill = "Decoupling Status",
    caption = "Solid colors show statistically significant elasticities (p<0.05). Faded colors show non-significant results.\nNumbers show count of significant states in each category."
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "bottom",
    plot.title = element_text(face = "bold", size = 13),
    plot.subtitle = element_text(size = 10),
    legend.title = element_text(face = "bold", size = 9),
    plot.caption = element_text(hjust = 0, size = 9, color = "gray30")
  )

print(plot_detailed)

ggsave("plots/decoupling_with_significance_shading.png", plot_detailed, 
       width = 9, height = 7, dpi = 300)