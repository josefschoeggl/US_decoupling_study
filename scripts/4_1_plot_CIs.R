# CONFIDENCE INTERVAL PLOTS WITH STATE LABELS ####
# Enhanced version adding state abbreviations to the CI plots

library(tidyverse)
library(ggrepel)  # For better label positioning

# Check that elasticity results exist
if (!exists("territorial_elasticities") || !exists("consumption_elasticities")) {
  stop("Please run calculate_elasticities_FINAL.R first!")
}

# Create plots directory if it doesn't exist
if (!dir.exists("plots")) {
  dir.create("plots")
  cat("✓ Created 'plots' directory\n")
}

# VERSION 1: WITH ALL STATE LABELS (may be crowded) ####

# Territorial emissions with state labels
plot_ci_terr_labeled <- territorial_elasticities %>%
  filter(status == "success") %>%
  arrange(elasticity) %>%
  mutate(state_rank = row_number()) %>%
  ggplot(aes(x = state_rank, y = elasticity)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black", linewidth = 1) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "red", linewidth = 0.5) +
  geom_ribbon(aes(ymin = ci_lower, ymax = ci_upper), alpha = 0.2, fill = "#3498db") +
  geom_line(color = "#3498db", linewidth = 1) +
  geom_point(aes(color = significant), size = 2) +
  geom_text(aes(label = state_code), size = 2.5, angle = 90, hjust = -0.2, vjust = 0.3) +
  scale_color_manual(values = c("TRUE" = "#2ecc71", "FALSE" = "#e74c3c"),
                     labels = c("TRUE" = "Significant (p<0.05)", "FALSE" = "Not significant"),
                     name = NULL) +
  scale_y_continuous(expand = expansion(mult = c(0.05, 0.15))) +  # Add space for labels
  labs(title = "Territorial Emissions Elasticities with 95% Confidence Intervals",
       subtitle = "States ranked by elasticity (most negative to most positive)",
       x = "State (ranked)",
       y = "Elasticity (β)",
       caption = "Shaded area shows 95% confidence interval. Green = significant, Red = not significant.") +
  theme_minimal(base_size = 11) +
  theme(legend.position = "bottom",
        plot.title = element_text(face = "bold"),
        plot.margin = margin(10, 10, 10, 10))

print(plot_ci_terr_labeled)

# Consumption-based emissions with state labels
plot_ci_cons_labeled <- consumption_elasticities %>%
  filter(status == "success") %>%
  arrange(elasticity) %>%
  mutate(state_rank = row_number()) %>%
  ggplot(aes(x = state_rank, y = elasticity)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black", linewidth = 1) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "red", linewidth = 0.5) +
  geom_ribbon(aes(ymin = ci_lower, ymax = ci_upper), alpha = 0.2, fill = "#9b59b6") +
  geom_line(color = "#9b59b6", linewidth = 1) +
  geom_point(aes(color = significant), size = 2) +
  geom_text(aes(label = state_code), size = 2.5, angle = 90, hjust = -0.2, vjust = 0.3) +
  scale_color_manual(values = c("TRUE" = "#2ecc71", "FALSE" = "#e74c3c"),
                     labels = c("TRUE" = "Significant (p<0.05)", "FALSE" = "Not significant"),
                     name = NULL) +
  scale_y_continuous(expand = expansion(mult = c(0.15, 0.15))) +  # Add space for labels
  labs(title = "Consumption-Based Emissions Elasticities with 95% Confidence Intervals",
       subtitle = "States ranked by elasticity (most negative to most positive)",
       x = "State (ranked)",
       y = "Elasticity (β)",
       caption = "Shaded area shows 95% confidence interval. Green = significant, Red = not significant.") +
  theme_minimal(base_size = 11) +
  theme(legend.position = "bottom",
        plot.title = element_text(face = "bold"),
        plot.margin = margin(10, 10, 10, 10))

print(plot_ci_cons_labeled)

# Save labeled versions
ggsave("plots/territorial_CI_with_labels.png", plot_ci_terr_labeled, 
       width = 14, height = 7, dpi = 300)
ggsave("plots/consumption_CI_with_labels.png", plot_ci_cons_labeled, 
       width = 14, height = 7, dpi = 300)

# VERSION 2: LABEL ONLY SIGNIFICANT OR EXTREME STATES (cleaner) ####

# Territorial - label only significant states
plot_ci_terr_sig_only <- territorial_elasticities %>%
  filter(status == "success") %>%
  arrange(elasticity) %>%
  mutate(state_rank = row_number(),
         label = ifelse(significant | abs(elasticity) > 2, state_code, "")) %>%
  ggplot(aes(x = state_rank, y = elasticity)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black", linewidth = 1) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "red", linewidth = 0.5) +
  geom_ribbon(aes(ymin = ci_lower, ymax = ci_upper), alpha = 0.2, fill = "#3498db") +
  geom_line(color = "#3498db", linewidth = 1) +
  geom_point(aes(color = significant), size = 2.5) +
  geom_text_repel(aes(label = label), 
                  size = 5,
                  box.padding = 0.6,
                  point.padding = 0.4,
                  min.segment.length = 0,
                  segment.color = "grey50",
                  segment.size = 0.5,
                  force = 2,
                  max.overlaps = Inf) +
  scale_color_manual(values = c("TRUE" = "#2ecc71", "FALSE" = "#e74c3c"),
                     labels = c("TRUE" = "Significant (p<0.05)", "FALSE" = "Not significant"),
                     name = NULL) +
  labs(title = "Territorial Emissions Elasticities (Significant States Labeled)",
       subtitle = "States ranked by elasticity (most negative to most positive)",
       x = "State (ranked)",
       y = expression("Elasticity (" * beta * ")")
  ) +
  theme_minimal(base_size = 10) +
  theme(legend.position = "bottom",
        legend.text = element_text(size = 16),
        legend.box.margin = margin(t = -5),
        legend.margin = margin(t = -1),
        plot.title = element_text(face = "bold", hjust = 0.5, size = 20),
        plot.subtitle = element_text(hjust = 0.5, size = 16),
        axis.text = element_text(size = 16),
        axis.title = element_text(size = 16),
        plot.title.position = "plot")

print(plot_ci_terr_sig_only)

# Consumption - label only significant states
plot_ci_cons_sig_only <- consumption_elasticities %>%
  filter(status == "success") %>%
  arrange(elasticity) %>%
  mutate(state_rank = row_number(),
         label = ifelse(significant | abs(elasticity) > 2, state_code, "")) %>%
  ggplot(aes(x = state_rank, y = elasticity)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black", linewidth = 1) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "red", linewidth = 0.5) +
  geom_ribbon(aes(ymin = ci_lower, ymax = ci_upper), alpha = 0.2, fill = "#9b59b6") +
  geom_line(color = "#9b59b6", linewidth = 1) +
  geom_point(aes(color = significant), size = 2.5) +
  geom_text_repel(aes(label = label), 
                  size = 4,
                  box.padding = 0.5,
                  point.padding = 0.3,
                  segment.color = "grey50",
                  segment.size = 0.3,
                  max.overlaps = Inf) +
  scale_color_manual(values = c("TRUE" = "#2ecc71", "FALSE" = "#e74c3c"),
                     labels = c("TRUE" = "Significant (p<0.05)", "FALSE" = "Not significant"),
                     name = NULL) +
  labs(title = "Consumption-Based Emissions Elasticities (Significant States Labeled)",
       subtitle = "States ranked by elasticity (most negative to most positive)",
       x = "State (ranked)",
       y = "Elasticity (β)",
       caption = "Only statistically significant states are labeled. Shaded area = 95% CI.") +
  theme_minimal(base_size = 16) +
  theme(legend.position = "bottom",
        legend.text = element_text(size = 14),
        legend.box.margin = margin(t = -4),
        legend.margin = margin(t = -5),
        plot.title = element_text(face = "bold", hjust = 0.5, size = 18),
        plot.subtitle = element_text(hjust = 0.5),
        plot.caption = element_text(size = 12))

print(plot_ci_cons_sig_only)

# Save cleaner versions
ggsave("plots/territorial_CI_sig_labeled.pdf", plot = plot_ci_terr_sig_only, 
       width = 12, height = 8)
ggsave("plots/consumption_CI_sig_labeled.pdf", plot_ci_cons_sig_only, 
       width = 12, height = 7)
ggsave("plots/territorial_CI_sig_labeled.png", plot = plot_ci_terr_sig_only, 
       width = 12, height = 8, dpi = 300)
ggsave("plots/consumption_CI_sig_labeled.png", plot_ci_cons_sig_only, 
       width = 12, height = 7, dpi = 300)

# VERSION 3: X-AXIS WITH STATE LABELS (alternative approach) ####

# Territorial - states on x-axis
plot_ci_terr_xaxis <- territorial_elasticities %>%
  filter(status == "success") %>%
  arrange(elasticity) %>%
  mutate(state_code = fct_inorder(state_code)) %>%
  ggplot(aes(x = state_code, y = elasticity)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black", linewidth = 1) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "red", linewidth = 0.5) +
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper, color = significant), 
                width = 0.3, linewidth = 0.8) +
  geom_point(aes(color = significant), size = 2.5) +
  scale_color_manual(values = c("TRUE" = "#2ecc71", "FALSE" = "#e74c3c"),
                     labels = c("TRUE" = "Significant (p<0.05)", "FALSE" = "Not significant"),
                     name = NULL) +
  labs(title = "Territorial Emissions Elasticities with 95% Confidence Intervals",
       subtitle = "States ordered by elasticity magnitude",
       x = NULL,
       y = "Elasticity (β)",
       caption = "Error bars show 95% confidence intervals. Green = significant, Red = not significant.") +
  theme_minimal(base_size = 10) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, size = 8),
        legend.position = "bottom",
        plot.title = element_text(face = "bold"),
        panel.grid.major.x = element_blank())

print(plot_ci_terr_xaxis)

# Consumption - states on x-axis
plot_ci_cons_xaxis <- consumption_elasticities %>%
  filter(status == "success") %>%
  arrange(elasticity) %>%
  mutate(state_code = fct_inorder(state_code)) %>%
  ggplot(aes(x = state_code, y = elasticity)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black", linewidth = 1) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "red", linewidth = 0.5) +
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper, color = significant), 
                width = 0.3, linewidth = 0.8) +
  geom_point(aes(color = significant), size = 2.5) +
  scale_color_manual(values = c("TRUE" = "#2ecc71", "FALSE" = "#e74c3c"),
                     labels = c("TRUE" = "Significant (p<0.05)", "FALSE" = "Not significant"),
                     name = NULL) +
  labs(title = "Consumption-Based Emissions Elasticities with 95% Confidence Intervals",
       subtitle = "States ordered by elasticity magnitude",
       x = NULL,
       y = "Elasticity (β)",
       caption = "Error bars show 95% confidence intervals. Green = significant, Red = not significant.") +
  theme_minimal(base_size = 10) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, size = 8),
        legend.position = "bottom",
        plot.title = element_text(face = "bold"),
        panel.grid.major.x = element_blank())

print(plot_ci_cons_xaxis)

# Save x-axis versions
ggsave("plots/territorial_CI_xaxis_labels.png", plot_ci_terr_xaxis, 
       width = 16, height = 7, dpi = 300)
ggsave("plots/consumption_CI_xaxis_labels.png", plot_ci_cons_xaxis, 
       width = 16, height = 7, dpi = 300)
