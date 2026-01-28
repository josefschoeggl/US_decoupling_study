# =======================================
# GDP per Capita vs Emissions (2019)
# =======================================

# Load libraries
library(readxl)
library(dplyr)
library(ggplot2)
#install.packages("patchwork")
library(patchwork)   # for side-by-side plots
#install.packages("ggrepel")
library(ggrepel)

# ---- 1. Read Excel sheets ----
file <- "State GHG Data Compilation.xlsx"

gdp <- read_excel(file, sheet = "GDP Per Cap")
te  <- read_excel(file, sheet = "TE Per Cap")
cbe <- read_excel(file, sheet = "CBE Per Cap")

# ---- 2. Select relevant columns for year 2019 ----
gdp_2019 <- gdp %>%
  select(State = `...1`, GDP_2019 = `2019`)

te_2019 <- te %>%
  select(State = `...1`, TE_2019 = `2019`)

cbe_2019 <- cbe %>%
  select(State = `...1`, CBE_2019 = `2019`)

# ---- 3. Merge datasets ----
df <- gdp_2019 %>%
  inner_join(te_2019, by = "State") %>%
  inner_join(cbe_2019, by = "State") %>%
  filter(State != "USA")

# ---- 4. Compute correlation (R²) values ----
r2_te  <- summary(lm(TE_2019 ~ GDP_2019, data = df))$r.squared
r2_cbe <- summary(lm(CBE_2019 ~ GDP_2019, data = df))$r.squared

# Round to 2 decimals for display
r2_te_label  <- paste0("R² = ", round(r2_te, 2))
r2_cbe_label <- paste0("R² = ", round(r2_cbe, 2))

# ---- 5. Determine common y-axis range ----
ymax <- max(df$TE_2019, df$CBE_2019, na.rm = TRUE)

# ---- 6. Plot GDP vs Territorial Emissions ----
p1 <-ggplot(df, aes(x = GDP_2019, y = TE_2019)) +
  geom_point(color = "orange", size = 3, alpha = 0.9) + 
  geom_label_repel(aes(label = State),max.overlaps = Inf, size = 5.5,segment.color = "grey40", segment.size = 0.5)+
  coord_cartesian(ylim = c(0, max(df$TE_2019, na.rm = TRUE)*1.015) ) + 
  geom_smooth(method = "lm", se = TRUE, color = "darkred", linetype = "dashed", linewidth=0.8) +
  annotate("text", x = Inf, y = Inf, label = r2_te_label,
           hjust = 1, vjust = 1.3, size = 7, color = "darkred") +
  scale_x_continuous(labels = scales::label_dollar())+
  labs(
    title = "GDP per Capita vs Territorial Emissions 
    by U.S. State (2019)",
    x = "GDP per Capita (USD)",
    y = expression("Territorial Emissions (t CO"[2]*"e per Capita)")
  ) +
  theme_minimal(base_size = 10) +
  theme(axis.title.x = element_text( margin = margin(t=14)),
        axis.title = element_text(size = 18),
        axis.text.x = element_text(size= 18),
        axis.text.y = element_text(size = 18),
        plot.title = element_text(face = "bold", hjust = 0.5, size = 20),
        panel.grid.minor = element_blank()
  ) 
  
# ---- 7. Plot GDP vs Consumption-based Emissions ----
p2 <- ggplot(df, aes(x = GDP_2019, y = CBE_2019)) +
  geom_point(color = "steelblue", size = 3, alpha = 0.8) +
  coord_cartesian(ylim = c(0, max(df$TE_2019, df$CBE_2019))) + 
  scale_x_continuous(labels = scales::label_dollar())+
  geom_label_repel(aes(label = State),max.overlaps = Inf, size = 5.5,segment.color = "grey40", segment.size = 0.5)+
  geom_smooth(method = "lm", se = TRUE, color = "navy", linetype = "dashed", linewidth=0.8) +
  annotate("text", x = Inf, y = Inf, label = r2_cbe_label,
           hjust = 1, vjust = 1.3, size = 7, color = "navy") +
  labs(
    title = "GDP per Capita vs Consumption-based Emissions 
    by U.S. State (2019)",
    x = "GDP per Capita (USD)",
    y = expression("Consumption-Based Emissions (t CO"[2]*"e per Capita)")
  ) +
  theme_minimal(base_size = 10) +
  theme(axis.title.x = element_text( margin = margin(t=12)),
        axis.title = element_text(size = 18),
        axis.text.x = element_text(size= 18),
        axis.text.y = element_text(size = 18),
        plot.title = element_text(face = "bold", hjust= 0.5, size = 20),
        panel.grid.minor = element_blank()
  )
  
# ---- 8. Combine both plots side-by-side ----
combined_plot <- p1 + p2 + plot_layout(ncol = 2)

# Display the combined plot
combined_plot

# ---- 9. (Optional) Save the figure ----
ggsave(filename = "GDP_vs_Emissions_2019_side_by_side.pdf",combined_plot, 
        width = 16, height = 9, units= "in")
ggsave(filename = "GDP_vs_Emissions_2019_side_by_side.png",combined_plot, 
       width = 16, height = 9, units= "in", dpi=400)

