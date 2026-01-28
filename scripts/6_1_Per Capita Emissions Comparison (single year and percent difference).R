# =======================================
# 2019 per Capita Emissions Comparison
# =======================================

# Load required libraries
library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)

# ---- 1. Read Excel sheets ----
file <- "State GHG Data Compilation.xlsx"

cbe <- read_excel(file, sheet = "CBE Per Cap")
te  <- read_excel(file, sheet = "TE Per Cap")

# ---- 2. Select 2019 data ----
# Use the correct first-column name ("...1") instead of "State"
cbe_2019 <- cbe %>%
  select(State = `...1`, CBE_2019 = `2019`)

te_2019 <- te %>%
  select(State = `...1`, TE_2019 = `2019`)

# ---- 3. Merge both datasets ----
df <- inner_join(cbe_2019, te_2019, by = "State")

# Remove any national or summary rows (like "USA")
df <- df %>% filter(State != "USA")

# ---- 3b. Compute percent difference ----
# Re-merge including USA
df_all <- full_join(cbe_2019, te_2019, by = "State") 

# Calculate percent difference: (CBE - TE) / TE * 100
df_diff <- df_all %>%
  mutate(
    Percent_Diff = ((CBE_2019 - TE_2019) / TE_2019) * 100
  )

# Optional: round for clarity
df_diff <- df_diff %>%
  mutate(Percent_Diff = round(Percent_Diff, 1))

# View the table
print(df_diff)


# ---- 4. Sort by CBE (descending) ----
df_sorted <- df %>%
  arrange(desc(CBE_2019))

# ---- 5. Reshape for ggplot ----
df_long <- df_sorted %>%
  pivot_longer(cols = c(CBE_2019, TE_2019),
               names_to = "Type",
               values_to = "Emissions")

# ---- 6. Plot ----
ggplot(df_long, aes(x = reorder(State, -Emissions), y = Emissions, fill = factor(Type, levels= c("TE_2019", "CBE_2019")))) +
  geom_col( position = position_dodge(width = 0.75),width = 0.7)+
  scale_fill_manual(
    values = c("orange","steelblue" ),
    labels = c("Territorial Emissions","Consumption-Based Emissions" )
  ) +
  scale_y_continuous(breaks= seq(0, max(df_long$Emissions, na.rm= TRUE), by= 25),
                     expand = expansion(mult = c(0, 0.05))) +
  labs(
    title = "Per Capita Greenhouse Gas Emissions by U.S. State (2019)",
    x = expression("State"),
    y = expression("Emissions (t CO"[2]*"e per Capita)"),
    fill = "Emission Type"
  ) +
  guides(fill = guide_legend(title = NULL)) +
  theme_minimal(base_size = 12) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, size= 16),
        axis.text.y = element_text(size = 16),
        axis.line.x = element_line(color = "grey", linewidth = 0.5),
        axis.line.y = element_line(color = "grey", linewidth = 0.5),
        axis.title = element_text(size = 16),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_text( hjust = 0.5,face = "bold", size = 20),
        plot.subtitle = element_text(hjust = 0.5),
        legend.text = element_text(size = 16),
        legend.position = "bottom",
         )

# ---- 7. (Optional) Save the chart ----
ggsave("State_Emissions_2019.png", width = 12, height = 8, dpi = 300)
ggsave("State_Emissions_2019.pdf", width = 12, height = 8)
