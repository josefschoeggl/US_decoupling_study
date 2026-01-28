##Percent change projected into future for one state##

library(readxl)
library(dplyr)
library(ggplot2)
library(tidyr)
library(rlang)

# ---- 1. Read data ----
file <- "State GHG Data Compilation.xlsx"
cbe <- read_excel(file, sheet = "CBE Per Cap")
te  <- read_excel(file, sheet = "TE Per Cap")

id_col <- names(cbe)[1]

# ---- 2. Select a state's data ---- 
state_lookup <- tibble::tibble(
  state_code = state.abb,
  state_name = state.name)

state_code <- "NY" #Change codes for different states

state_name <- state_lookup %>%
  filter(state_code == !!state_code) %>%
  pull(state_name)

cbe_us <- cbe %>% filter(!!sym(id_col) == state_code)
te_us  <- te  %>% filter(!!sym(id_col) == state_code)

# ---- 3. Compute 3-year rolling averages (2012–2014 and 2017–2019) ----
cbe_start <- mean(as.numeric(cbe_us[, c("2012", "2013", "2014")]), na.rm = TRUE)
cbe_end   <- mean(as.numeric(cbe_us[, c("2017", "2018", "2019")]), na.rm = TRUE)
te_start  <- mean(as.numeric(te_us[,  c("2012", "2013", "2014")]), na.rm = TRUE)
te_end    <- mean(as.numeric(te_us[,  c("2017", "2018", "2019")]), na.rm = TRUE)

# ---- 4. Compute annual growth rates (CAGR) ----
years <- 2019 - 2012
cbe_rate <- (cbe_end / cbe_start)^(1/years) - 1
te_rate  <- (te_end / te_start)^(1/years) - 1

# ---- 5. Build projection to 2050 ----
future_years <- 2020:2050

# Project values forward using compound growth
cbe_proj <- cbe_end * (1 + cbe_rate)^(future_years - 2019)
te_proj  <- te_end  * (1 + te_rate)^(future_years - 2019)

# Combine all data into one frame
data_hist <- data.frame(
  Year = 2012:2019,
  CBE = as.numeric(cbe_us[, as.character(2012:2019)]),
  TE  = as.numeric(te_us[,  as.character(2012:2019)]),
  Period = "Historical"
)

data_future <- data.frame(
  Year = future_years,
  CBE = cbe_proj,
  TE  = te_proj,
  Period = "Projected"
)

data_all <- bind_rows(data_hist, data_future)

# ---- 6. Reshape for plotting ----
data_long <- data_all %>%
  pivot_longer(cols = c(CBE, TE), names_to = "Type", values_to = "Emissions")
data_long$Type
data_long$Period

# ---- 7. Plot ----
y_max <- max(data_long$Emissions, na.rm = TRUE) * 1.025  # 2.5% padding
y_min <- min(data_long$Emissions, na.rm = TRUE) * 0.975  # optional lower padding

labels_data <- data_long %>% 
  filter(Year %in% c(2012,2019, 2050))

ggplot(data_long, aes(x = Year, y = Emissions, color = Type, linetype = Period)) +
   geom_line(
    data = subset(data_long, Period == "Historical"),
    linewidth = 1.2,
    alpha = 1
  ) +
  geom_point(data = subset(data_long, Period == "Historical"), size = 2.5) +
  geom_line(
    data = subset(data_long, Period == "Projected"),
    linewidth = 1.2,
    alpha = 0.8
  ) +
  geom_text(
    data = labels_data,
    aes(x = Year, y = Emissions, label = round(Emissions, 1), color = Type),
    vjust = -1.2,
    size = 4.5,
    show.legend = FALSE )+
 
  scale_color_manual( values = c("steelblue", "orange" ),
                      labels = c("Consumption-Based Emissions", "Territorial Emissions" )) +
  scale_linetype_manual(values = c("Historical (smoothed)" = "solid", "Projected" = "dashed")) +
  scale_linetype_manual(
    name   = "Period",
    values = c(Historical = "solid", Projected = "dashed"),
    labels = c( "Historical (2012–2019)", "Projected (2020–2050)")
    )+

  scale_x_continuous(breaks = seq(2012, 2050, by = 4),
                      expand = expansion(mult = c(0.01, 0.02)))+
  labs(
    title = paste0(state_name," per Capita GHG Emissions: Historical (2012–2019) & Projected (till 2050)"),
    y = expression("Emissions (t CO"[2]*"e per Capita)"),
    x = expression("Year"),
    color = "Emission Type",
    linetype = "Period" ) +
  
  coord_cartesian(ylim = c(y_min, y_max)) +
  
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(hjust= 0.5, face = "bold", size = 20),
    plot.subtitle = element_text(size = 16, hjust= 0.5),
    plot.caption = element_text(size = 10),
    axis.title = element_text(size = 16),
    axis.text = element_text(size= 16),
    axis.title.x = element_text(margin = margin(t=5)),
    legend.position = "bottom",
    legend.text = element_text(size = 16),
    legend.title = element_text(size = 16),
    legend.justification = "center",
    legend.box = "vertical",
    legend.margin = margin(t=-5),
    panel.grid.minor = element_blank(),
    plot.margin = margin(t = 10, r = 20, b = 10, l = 10)
 
  )


##Percent change projected for all 50 U.S. states##

library(readxl)
library(dplyr)
library(ggplot2)
library(tidyr)

# ---- 1. Read data ----
file <- "State GHG Data Compilation.xlsx"
cbe <- read_excel(file, sheet = "CBE Per Cap")
te  <- read_excel(file, sheet = "TE Per Cap")

id_col <- names(cbe)[1]

# ---- 2. Create output folder ----
output_dir <- "State_Emissions_Projections"
if (!dir.exists(output_dir)) dir.create(output_dir)

# ---- 3. Loop over all states ----
state_lookup <- tibble::tibble( 
  state_code = state.abb,
  state_name = state.name)

states <- cbe[[id_col]]
states <- states[states != "USA"]  # exclude national row

for (state_code in states) {
  state_name <- state_lookup %>%    #Get complete state names for title
    filter(state_code == !!state_code) %>%
    pull(state_name)
  
  if (length(state_name) == 0) {
    warning(paste("State code not found:", state_code))
    next
  }
  
  # ---- Extract state data ----
  cbe_state <- cbe %>% filter(!!sym(id_col) == state_code)
  te_state  <- te  %>% filter(!!sym(id_col) == state_code)
  
  # ---- Compute 3-year rolling averages for start and end ----
  cbe_start <- mean(as.numeric(cbe_state[, c("2012","2013","2014")]), na.rm = TRUE)
  cbe_end   <- mean(as.numeric(cbe_state[, c("2017","2018","2019")]), na.rm = TRUE)
  te_start  <- mean(as.numeric(te_state[,  c("2012","2013","2014")]), na.rm = TRUE)
  te_end    <- mean(as.numeric(te_state[,  c("2017","2018","2019")]), na.rm = TRUE)
  
  # ---- Compute CAGR ----
  years <- 2019 - 2012
  cbe_rate <- (cbe_end / cbe_start)^(1/years) - 1
  te_rate  <- (te_end / te_start)^(1/years) - 1
  
  # ---- Project to 2050 ----
  future_years <- 2020:2050
  cbe_proj <- cbe_end * (1 + cbe_rate)^(future_years - 2019)
  te_proj  <- te_end  * (1 + te_rate)^(future_years - 2019)
  
  # ---- Combine historical and projected ----
  data_hist <- data.frame(
    Year = 2012:2019,
    CBE = as.numeric(cbe_state[, as.character(2012:2019)]),
    TE  = as.numeric(te_state[,  as.character(2012:2019)]),
    Period = "Historical"
  )
  
  data_future <- data.frame(
    Year = future_years,
    CBE = cbe_proj,
    TE  = te_proj,
    Period = "Projected"
  )
  
  data_all <- bind_rows(data_hist, data_future)
  
  # ---- Reshape for plotting ----
  data_long <- data_all %>%
    pivot_longer(cols = c(CBE, TE), names_to = "Type", values_to = "Emissions")
  
  # ---- Determine y-axis limits ----
  y_max <- max(data_long$Emissions, na.rm = TRUE) * 1.025  # 2.5% padding
  y_min <- min(data_long$Emissions, na.rm = TRUE) * 0.975  # optional lower padding
  
  # ---- Plot ----
  labels_data <- data_long %>% 
    filter(Year %in% c(2012,2019, 2050))
  
  p <- ggplot(data_long, aes(x = Year, y = Emissions, color = Type, linetype = Period)) +
    geom_line(
      data = subset(data_long, Period == "Historical"),
      linewidth = 1.2,
      alpha = 1
    ) +
    geom_point(data = subset(data_long, Period == "Historical"), size = 2.5) +
    geom_line(
      data = subset(data_long, Period == "Projected"),
      linewidth = 1.2,
      alpha = 0.8
    ) +
    geom_text(
      data = labels_data,
      aes(x = Year, y = Emissions, label = round(Emissions, 1), color = Type),
      vjust = -1.2,
      size = 4.5,
      nudge_x = 0.25,
      show.legend = FALSE )+
    
    scale_color_manual( values = c("steelblue", "orange"),
                        labels = c("Consumption-Based emissions", "Territorial Emissions")) +
    scale_linetype_manual(
      name   = "Period",
      values = c(Historical = "solid", Projected = "dashed"),
      labels = c( "Historical (2012–2019)", "Projected (2020–2050)")
    )+
    
    scale_x_continuous(breaks = seq(2012, 2050, by = 4),
                       expand = expansion(mult = c(0.01, 0.02)))+
    labs(
      title = paste0(state_name," per Capita GHG Emissions"),
      subtitle= "Historical (2012–2019) & Projected (through 2050)",
     # caption= "*Projection assumes continuation of historical annual growth rates (Following Vogel & Hickel, 2023)",
      y = expression("Emissions (t CO"[2]*"e per Capita)"),
      x = expression("Year"),
      color = "Emission Type",
      linetype = "Period" ) +
    
    theme_minimal(base_size = 12) +
    theme(
      plot.title = element_text(hjust= 0.5, face = "bold", size = 20),
      plot.subtitle = element_text(size = 16, hjust= 0.5),
      plot.caption = element_text(size = 10),
      axis.title = element_text(size = 16),
      axis.text = element_text(size= 16),
      axis.title.x = element_text(margin = margin(t=5)),
      legend.position = "bottom",
      legend.text = element_text(size = 16),
      legend.title = element_text(size = 16),
      legend.justification = "center",
      legend.box = "vertical",
      legend.margin = margin(t=-5),
      panel.grid.minor = element_blank(),
      plot.margin = margin(t = 10, r = 20, b = 10, l = 10)
      )+
  coord_cartesian(ylim = c(y_min, y_max))  # set limits so all values are inside
  
  # ---- Save ----
  ggsave(filename = file.path(output_dir, paste0(state_code, "_Emissions_Projection_2050.pdf")),
    plot = p, width = 12, height = 7)
  ggsave(filename = file.path(output_dir, paste0(state_code, "_Emissions_Projection_2050.png")),
    plot = p, width = 12, height = 7, dpi = 300)
        
}


##When will drop below 2 tonnes##

library(readxl)
library(dplyr)
library(tidyr)

# ---- 1. Read data ----
file <- "State GHG Data Compilation.xlsx"
cbe <- read_excel(file, sheet = "CBE Per Cap")
te  <- read_excel(file, sheet = "TE Per Cap")

id_col <- names(cbe)[1]

# ---- 2. Create output folder ----
output_dir <- "State_Emissions_Projections"
if (!dir.exists(output_dir)) dir.create(output_dir)

# ---- 3. Exclude "Total", keep "USA" ----
non_states <- c("Total")
cbe <- cbe %>% filter(!get(id_col) %in% non_states)
te  <- te  %>% filter(!get(id_col) %in% non_states)

states <- cbe[[id_col]]  # includes USA
threshold <- 2
max_year <- 5000

results <- list()

# ---- 4. Helper function ----
solve_cross <- function(end_val, rate, start_year, threshold, max_year) {
  if (is.na(end_val) || is.na(rate) || end_val <= 0) return(NA)
  if (rate >= 0) return(NA)
  
  if ((1 + rate) > 0) {
    t <- log(threshold / end_val) / log(1 + rate)
    if (!is.nan(t) && !is.infinite(t) && t >= 0) {
      year_int <- ceiling(start_year + t)
      if (year_int <= max_year) return(year_int)
    }
  }
  
  # fallback iterative
  year <- start_year
  val <- end_val
  while (val > threshold && year <= max_year) {
    val <- val * (1 + rate)
    year <- year + 1
  }
  if (year <= max_year) return(year) else return(NA)
}

# ---- 5. Main loop ----
for (state_code in states) {
  cbe_state <- cbe %>% filter(!!sym(id_col) == state_code)
  te_state  <- te  %>% filter(!!sym(id_col) == state_code)
  
  cbe_start <- mean(as.numeric(cbe_state[, c("2012","2013","2014")]), na.rm = TRUE)
  cbe_end   <- mean(as.numeric(cbe_state[, c("2017","2018","2019")]), na.rm = TRUE)
  te_start  <- mean(as.numeric(te_state[,  c("2012","2013","2014")]), na.rm = TRUE)
  te_end    <- mean(as.numeric(te_state[,  c("2017","2018","2019")]), na.rm = TRUE)
  
  years <- 2019 - 2012
  cbe_rate <- (cbe_end / cbe_start)^(1/years) - 1
  te_rate  <- (te_end / te_start)^(1/years) - 1
  
  year_cbe <- solve_cross(cbe_end, cbe_rate, 2019, threshold, max_year)
  year_te  <- solve_cross(te_end, te_rate, 2019, threshold, max_year)
  
  results[[state_code]] <- data.frame(
    State = state_code,
    CBE_start = cbe_start,
    CBE_end = cbe_end,
    CBE_rate = cbe_rate,
    Year_CBE_below2 = year_cbe,
    TE_start = te_start,
    TE_end = te_end,
    TE_rate = te_rate,
    Year_TE_below2 = year_te
  )
}

# ---- 6. Combine & add "Years to reach <2 t" ----
results_df <- bind_rows(results) %>%
  mutate(
    Years_to_2_CBE = ifelse(!is.na(Year_CBE_below2), Year_CBE_below2 - 2019, NA),
    Years_to_2_TE  = ifelse(!is.na(Year_TE_below2),  Year_TE_below2 - 2019, NA)
  ) %>%
  arrange(Years_to_2_CBE)  # sort by soonest to reach <2 t for CBE

# ---- 7. View ----
results_df



