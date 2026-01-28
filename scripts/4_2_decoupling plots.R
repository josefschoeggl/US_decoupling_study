# DECOUPLING FIGURES ####
# (1) Share of states by decoupling category
# (2) Share of 2019 emissions by decoupling category

library(tidyverse)

# Checks & preparation ####

if (!exists("all_elasticities")) {
  stop("Please run calculate_elasticities_FINAL.R first to create 'all_elasticities'.")
}
if (!exists("te") | !exists("cbe")) {
  stop("Objects 'te' (territorial totals) and 'cbe' (consumption totals) must be loaded.")
}

# Create plots directory if it doesn't exist
if (!dir.exists("plots")) {
  dir.create("plots")
}

# Clean elasticities: one row per state × emission_type
elasticities_clean <- all_elasticities %>%
  mutate(
    emission_type = factor(emission_type,
                           levels = c("Consumption-based", "Territorial")),
    decoupling_category = factor(decoupling_category,
                                 levels = c("No Decoupling",
                                            "Relative Decoupling",
                                            "Absolute Decoupling")),
    sig_label = if_else(significant, "Significant", "Not Significant")
  ) %>%
  distinct(state_code, emission_type, .keep_all = TRUE)

# FIGURE A: Share of states by decoupling category ####

state_share <- elasticities_clean %>%
  count(emission_type, decoupling_category, sig_label) %>%
  group_by(emission_type) %>%
  mutate(percentage_of_states = n / sum(n) * 100) %>%
  ungroup()

state_share$emission_type <- factor(
  state_share$emission_type,
  levels = c("Territorial", "Consumption-based"),
  labels = c("Territorial Emissions", "Consumption-Based Emissions"))

state_share$decoupling_category <- factor(
  state_share$decoupling_category,
  levels = c("Absolute Decoupling", "Relative Decoupling", "No Decoupling")
)

fig_states <- ggplot(state_share,
                     aes(x = emission_type,
                         y = percentage_of_states,
                         fill = decoupling_category,
                         alpha = sig_label)) +
  geom_col(color = "white", linewidth = 0.4, width = 0.6) +
  scale_fill_manual(
    values = c(
      "Absolute Decoupling" = "#2ecc71",
      "Relative Decoupling" = "#f39c12",
      "No Decoupling"       = "#e74c3c"
    ),
    name = "Decoupling Status"
  ) +
  scale_alpha_manual(
    values = c("Significant" = 1.0, "Not Significant" = 0.35),
    name = "Statistical\nSignificance",
    guide = guide_legend(override.aes = list(fill = "grey50"))
  ) +
  scale_y_continuous(limits = c(0, 100),
                     breaks = seq(0, 100, 25),
                     expand = c(0, 0)) +
  labs(
    title    = "Decoupling Status by Emission Accounting Method",
    subtitle = "U.S. States (2012–2019)",
    x        = NULL,
    y        = "Percentage of States (%)"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position   = "bottom",
    legend.box        = "vertical",
    plot.title        = element_text(face = "bold", size = 20, hjust = 0.5),
    plot.subtitle     = element_text(size = 16, hjust = 0.5),
    plot.caption      = element_text(hjust = 0.5, size = 10,
                                     margin = margin(t = 10),
                                     lineheight = 1.3),
    legend.title      = element_text(face = "bold", size = 16),
    legend.text       = element_text(size = 16),
    axis.title.y      = element_text(size = 16, face = "bold"),
    axis.text.x       = element_text(size = 16, face = "bold"),
    axis.text.y       = element_text(size = 16),
    panel.grid.major.x = element_blank(),
    panel.grid.minor   = element_blank(),
    plot.margin = margin(t = 4, r = 4, b = 4, l = 4)
  )

print(fig_states)

ggsave("plots/decoupling_share_of_states_style2.pdf", fig_states,
       width = 10, height = 8)
ggsave("plots/decoupling_share_of_states_style2.png", fig_states,
       width = 10, height = 7.5, dpi = 300, bg = "white")

# FIGURE B: Share of 2019 emissions by decoupling category ####

# Get latest (2019) total emissions per state and emission type
latest_emissions <- bind_rows(
  # Territorial totals
  te %>%
    rename(state_code = NA.) %>%
    select(state_code, X2019) %>%
    filter(state_code != "USA") %>%
    transmute(
      state_code,
      emission_type    = factor("Territorial",
                                levels = c("Consumption-based", "Territorial")),
      emissions_2019   = as.numeric(X2019)
    ),
  # Consumption-based totals
  cbe %>%
    rename(state_code = NA.) %>%
    select(state_code, X2019) %>%
    filter(state_code != "USA") %>%
    transmute(
      state_code,
      emission_type    = factor("Consumption-based",
                                levels = c("Consumption-based", "Territorial")),
      emissions_2019   = as.numeric(X2019)
    )
)

# Join elasticities (decoupling category) with latest emissions
elasticities_with_emis <- elasticities_clean %>%
  left_join(latest_emissions,
            by = c("state_code", "emission_type"))

# Aggregate: share of total 2019 emissions in each decoupling category
emission_share <- elasticities_with_emis %>%
  group_by(emission_type) %>%
  mutate(total_emissions = sum(emissions_2019, na.rm = TRUE)) %>%
  group_by(emission_type, decoupling_category) %>%
  summarise(
    emissions_cat    = sum(emissions_2019, na.rm = TRUE),
    total_emissions  = first(total_emissions),
    .groups          = "drop_last"
  ) %>%
  ungroup() %>%
  mutate(
    percentage_of_emissions = emissions_cat / total_emissions * 100
  )

# Plot emission-weighted decoupling
fig_emis <- ggplot(emission_share,
                   aes(x = emission_type,
                       y = percentage_of_emissions,
                       fill = decoupling_category)) +
  geom_col(color = "white", linewidth = 0.5) +
  scale_fill_manual(
    values = c(
      "Absolute Decoupling" = "#2ecc71",
      "Relative Decoupling" = "#f39c12",
      "No Decoupling"       = "#e74c3c"
    ),
    name = "Decoupling Status"
  ) +
  scale_y_continuous(limits = c(0, 100),
                     breaks = seq(0, 100, 25),
                     expand = c(0, 0)) +
  labs(
    title    = "Emission-Weighted Decoupling by Accounting Method",
    subtitle = "Share of 2019 state GHG emissions in each decoupling category",
    x        = NULL,
    y        = "Percentage of 2019 Emissions (%)",
    caption  = "Bars show the share of total 2019 GHG emissions (sum of states) falling into each decoupling category, based on the income elasticity (β) of per-capita emissions over 2012–2019."
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position   = "bottom",
    legend.box        = "horizontal",
    plot.title        = element_text(face = "bold", size = 15, hjust = 0),
    plot.subtitle     = element_text(size = 12, hjust = 0),
    plot.caption      = element_text(hjust = 0, size = 9,
                                     margin = margin(t = 10),
                                     lineheight = 1.3),
    legend.title      = element_text(face = "bold", size = 10),
    legend.text       = element_text(size = 9),
    axis.title.y      = element_text(size = 11, face = "bold"),
    axis.text.x       = element_text(size = 11, face = "bold"),
    axis.text.y       = element_text(size = 10),
    panel.grid.major.x = element_blank(),
    panel.grid.minor   = element_blank()
  )

print(fig_emis)

ggsave("plots/decoupling_share_of_emissions_2019.png", fig_emis,
       width = 10, height = 7.5, dpi = 300, bg = "white")