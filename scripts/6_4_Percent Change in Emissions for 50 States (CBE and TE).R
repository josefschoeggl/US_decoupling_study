# =======================================
# Percent Change in Per Capita Emissions (2012–2019)
# =======================================

library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)

##____Simple percent change____##

# ---- 1. Read Excel sheets ----
file <- "State GHG Data Compilation.xlsx"

cbe <- read_excel(file, sheet = "CBE Per Cap")
te  <- read_excel(file, sheet = "TE Per Cap")

# ---- 2. Select 2012 and 2019 data ----
cbe_change <- cbe %>%
  select(State = `...1`, `2012`, `2019`) %>%
  mutate(CBE_change = (`2019` - `2012`) / `2012` * 100)

te_change <- te %>%
  select(State = `...1`, `2012`, `2019`) %>%
  mutate(TE_change = (`2019` - `2012`) / `2012` * 100)

# ---- 3. Merge both datasets ----
df <- inner_join(cbe_change %>% select(State, CBE_change),
                 te_change %>% select(State, TE_change),
                 by = "State")

df <- df %>% filter(State != "USA") #Drop USA Totals

# ---- 4. Sort by CBE percent change (descending) ----
df_sorted <- df %>%
   arrange(desc(CBE_change))

# ---- 5. Reshape for ggplot ----
df_long <- df %>%
  pivot_longer(cols = c( TE_change, CBE_change),
               names_to = "Type",
               values_to = "PercentChange"
  )%>%
  mutate(
    Type = factor(
      Type,
      levels = c("TE_change","CBE_change" ),
      )
  )

# ---- 6. Plot ----
ggplot(df_long, aes(x = reorder(State, -PercentChange), y = PercentChange, fill = Type)) +
  geom_col( position = position_dodge(width = 0.75),width = 0.7)+
  geom_hline(yintercept = 0, color = "black", linewidth = 0.25) +
  scale_fill_manual(
    values = c( "orange", "steelblue"),
    labels = c("Territorial Emissions","Consumption-Based Emissions" )
  ) +
    labs(
    title = "Percent Change in per Capita Greenhouse Gas Emissions by U.S. State (2012–2019)",
    x = "State",
    y = "Percent Change (%)",
    fill = "Emission Type"
  ) +
    guides(fill = guide_legend(title = NULL)) +
      theme_minimal(base_size = 12) +
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, size=16),
            axis.text.y = element_text(size = 16),
            axis.line.x = element_line(color = "grey", linewidth = 0.5),
            axis.line.y = element_line(color = "grey", linewidth = 0.5),
            axis.title.x = element_text(size = 16, margin = margin(t=15), hjust = 0.5),
            axis.title.y = element_text(size = 16),
            panel.grid.major.x = element_blank(),
            panel.grid.minor = element_blank(),
            plot.title = element_text( hjust = 0.5,face = "bold", size = 20, margin = margin(t=5, b=10)),
            plot.caption.position = "plot",
            plot.caption = element_text(size = 12),
            legend.text = element_text(size = 16),
            legend.margin = margin(t=-5),
            legend.position = "bottom",
  )

# ---- 7. (Optional) Save the chart ----
ggsave("State_Emissions_PercentChange_2012_2019.pdf", width = 12, height = 8)
ggsave("State_Emissions_PercentChange_2012_2019.png", width = 12, height = 8, dpi = 300)

##_____ Percent change GDP____ ##

library(readxl)
library(dplyr)

file <- "State GHG Data Compilation.xlsx"

# ---- 1. Read only the required range ----
gdp <- read_excel(
  file,
  sheet = "GDP Per Cap",
  range = "A1:I52",      
  col_types = c("text", rep("numeric", 8))  # first col text, next 8 numeric
 ) #%>%

# ---- 2. Calculate percent change ----
gdp_change <- gdp %>%
  select(State = 1, `2012`, `2019`) %>%
  mutate(GDP_change = (`2019` - `2012`) / `2012` * 100) 

# ---- 3. Sort and plot ----
gdp_sorted <- gdp_change %>%
  arrange(desc(GDP_change))
gdp_sorted <- gdp_sorted %>% filter(State != "USA") #Drop USA Totals

gdp_sorted <- gdp_sorted %>%
  mutate(GDP_sign = ifelse(GDP_change >= 0, "Increase", "Decrease"))
ylim_val <- max(abs(gdp_sorted$GDP_change))

ggplot(gdp_sorted, aes(x = reorder(State, -GDP_change), y = GDP_change, fill = GDP_sign)) +
  geom_col( position = position_dodge(width = 0.75),width = 0.7, )+
  geom_hline(yintercept = 0, color = "black", linewidth = 0.4) +
  scale_y_continuous(
    limits = c(-ylim_val, ylim_val),
    breaks = scales::pretty_breaks(n = 7)) +
  scale_fill_manual(
    values = c("Increase" = "seagreen3",
               "Decrease" = "firebrick2"))+
  labs(
    title = "Percent Change in Real GDP per Capita for U.S. States (2012–2019)",
    x = "State",
    y = "Percent Change (%)"
  ) +
guides(fill = guide_legend(title = NULL)) +
  theme_minimal(base_size = 12) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, size=16),
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
ggsave("State_Emissions_GDP_PercentChange_2012_2019.png", width = 12, height = 6, dpi=300)
ggsave("State_Emissions_GDP_PercentChange_2012_2019.pdf", width = 12, height = 6)

##____ Rolling average following Vogel and Hickel 2023____##

library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)

# ---- 1. Read Excel sheets ----
file <- "State GHG Data Compilation.xlsx"
cbe <- read_excel(file, sheet = "CBE Per Cap")
te  <- read_excel(file, sheet = "TE Per Cap")

# Identify the first column (state code)
id_col <- names(cbe)[1]

# ---- 2. Compute rolling averages following Vogel & Hickel (2023) ----
# 3-year average for start (2012–2014) and end (2017–2019)

cbe_change <- cbe %>%
  select(State = all_of(id_col), matches("^\\d{4}$")) %>%
  mutate(
    avg_start = rowMeans(select(., `2012`:`2014`), na.rm = TRUE),
    avg_end   = rowMeans(select(., `2017`:`2019`), na.rm = TRUE),
    CBE_change = (avg_end - avg_start) / avg_start * 100
  ) %>%
  select(State, CBE_change)

te_change <- te %>%
  select(State = all_of(id_col), matches("^\\d{4}$")) %>%
  mutate(
    avg_start = rowMeans(select(., `2012`:`2014`), na.rm = TRUE),
    avg_end   = rowMeans(select(., `2017`:`2019`), na.rm = TRUE),
    TE_change = (avg_end - avg_start) / avg_start * 100
  ) %>%
  select(State, TE_change)

# ---- 3. Merge datasets ----
df <- inner_join(cbe_change, te_change, by = "State") %>%
  filter(State != "USA")

# ---- 4. Reshape for ggplot ----

df_long <- df %>%
  pivot_longer(cols = c( TE_change, CBE_change),
               names_to = "Type",
               values_to = "PercentChange"
  )%>%
  mutate(
    Type = factor(
      Type,
      levels = c("TE_change","CBE_change" ),
    )
  )

# ---- 5. Plot ----
p <- ggplot(df_long, aes(x = reorder(State, -PercentChange), y = PercentChange, fill = Type)) +
  geom_col( position = position_dodge(width = 0.75),width = 0.65)+
  geom_hline(yintercept = 0, color = "grey40", linewidth = 0.4) +
  
  scale_fill_manual(
    values = c( "orange", "steelblue"),
    labels = c("Territorial Emissions","Consumption-Based Emissions" )
   ) +
  scale_y_continuous(
    expand = expansion(mult = c(0.12, 0.35)),
    breaks = scales::pretty_breaks(n = 6)
  ) +
  labs(
    title = "Percent Change in per Capita GHG Emissions by U.S. State (2012–2019)",
    subtitle = "Compound Annual Growth Rate Moving Average", 
    x = "State",
    y = "Percent Change (%)",
    fill =  NULL, 
  )+ 
  guides(fill = guide_legend(title = NULL)) +
  theme_minimal(base_size = 12) +
 
   theme(
        plot.margin = margin(t=10, b= 5, l=10, r = 20),
        axis.title.x = element_text(margin = margin(t =10)),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, size=16),
        axis.text.y = element_text(size = 16),
        axis.line.x = element_line(color = "grey", linewidth = 0.5),
        axis.line.y = element_line(color = "grey", linewidth = 0.5),
        axis.title = element_text(size = 16),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_text( hjust = 0.5,face = "bold", size = 20),
        plot.subtitle = element_text(hjust = 0.5, size = 16),
        legend.text = element_text(size = 16),
        legend.position = "bottom",
        legend.margin = margin(t= -5),
        legend.justification = "center",
  )

print(p)
# ---- 6. Optional: Save ----
ggsave("State_Emissions_PercentChange_CAGR_VH2023.pdf", plot = p, width = 12, height = 8)
ggsave("State_Emissions_PercentChange_CAGR_VH2023.png", plot = p, width = 12, height = 8, dpi=300)

