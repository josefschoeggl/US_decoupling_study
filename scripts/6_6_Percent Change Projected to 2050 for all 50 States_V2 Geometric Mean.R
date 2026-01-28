## Percent-change projection with 5y rolling (middle) + 3y endpoints
## Using geometric mean of YoY growth on the smoothed series (Vogel & Hickel style)

library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(rlang)

# ---- 1. Read data ----
file <- "State GHG Data Compilation.xlsx"
cbe <- read_excel(file, sheet = "CBE Per Cap")
te  <- read_excel(file, sheet = "TE Per Cap")

id_col <- names(cbe)[1]      # first column = state identifier
years_hist   <- 2012:2019
future_years <- 2020:2050

# ---- 2. Pick state/region ----
state_lookup <- tibble::tibble(
  state_code = state.abb,
  state_name = state.name)

state_code <- "AZ"          # e.g., "KY", "USA", etc.

state_name <- state_lookup %>%
  filter(state_code == !!state_code) %>%
  pull(state_name)

cbe_st <- cbe %>% filter(!!sym(id_col) == state_code)
te_st  <- te  %>% filter(!!sym(id_col) == state_code)

# Extract numeric vectors for historical years
cbe_hist_raw <- as.numeric(cbe_st[, as.character(years_hist)])
te_hist_raw  <- as.numeric(te_st[,  as.character(years_hist)])

# Make sure names are characters so window indexing works
names(cbe_hist_raw) <- names(te_hist_raw) <- as.character(years_hist)

# ---- 3. Build smoothed historical series ----
# Rule: 5-year centered rolling mean for interior years (2014–2017);
#       asymmetric 5-year at 2013 (2012–2016) and 2018 (2016–2019);
#       3-year at the endpoints: 2012 (2012–2014) and 2019 (2017–2019).

roll_mean_custom <- function(x, yrs) {
  out <- rep(NA_real_, length(yrs))
  names(out) <- as.character(yrs)
  for (t in yrs) {
    if (t == 2012)      win <- 2012:2014
    else if (t == 2013) win <- 2012:2016
    else if (t %in% 2014:2017) win <- (t-2):(t+2)
    else if (t == 2018) win <- 2016:2019
    else if (t == 2019) win <- 2017:2019
    vals <- x[as.character(win)]
    out[as.character(t)] <- mean(vals, na.rm = TRUE)
  }
  out
}

cbe_hist_smooth <- roll_mean_custom(cbe_hist_raw, years_hist)
te_hist_smooth  <- roll_mean_custom(te_hist_raw,  years_hist)

# ---- 4. Average annual % change = geometric mean of YoY growth (on smoothed series) ----
rate_from_geom_mean <- function(x_smooth) {
  ok <- is.finite(x_smooth) & x_smooth > 0
  x  <- x_smooth[ok]
  if (length(x) < 2) return(NA_real_)
  r <- exp(mean(diff(log(x)))) - 1
  as.numeric(r)
}

cbe_rate <- rate_from_geom_mean(cbe_hist_smooth)
te_rate  <- rate_from_geom_mean(te_hist_smooth)

# ---- 5. Project forward to 2050 from the *smoothed 2019* level ----
cbe_2019 <- cbe_hist_smooth["2019"]
te_2019  <- te_hist_smooth["2019"]

cbe_proj <- cbe_2019 * (1 + cbe_rate)^(future_years - 2019)
te_proj  <- te_2019  * (1 + te_rate)^(future_years - 2019)

# ---- 6. Combine data ----
data_hist <- tibble(
  Year = years_hist,
  CBE  = as.numeric(cbe_hist_smooth),
  TE   = as.numeric(te_hist_smooth),
  Period = "Historical (smoothed)"
)

data_future <- tibble(
  Year = future_years,
  CBE  = cbe_proj,
  TE   = te_proj,
  Period = "Projected"
)

data_all <- bind_rows(data_hist, data_future) |>
  pivot_longer(cols = c(CBE, TE), names_to = "Type", values_to = "Emissions")

# ---- 7. Plot ----

y_max <- max(data_all$Emissions, na.rm = TRUE) * 1.025  # 2.5% padding
y_min <- min(data_all$Emissions, na.rm = TRUE) * 0.975  # optional lower padding

labels_data <- data_all %>% 
  filter(Year %in% c(2012,2019, 2050))

ggplot(data_all, aes(x = Year, y = Emissions, color = Type, linetype = Period)) +
  
  geom_line(
    data = subset(data_all, Period == "Historical (smoothed)"),
    linewidth = 1.2,
    alpha = 1
  ) +
  geom_point(data = subset(data_all, Period == "Historical"), size = 2.5) +
  geom_line(
    data = subset(data_all, Period == "Projected"),
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
                      labels = c("Consumption-based emissions", "Territorial emissions")) +
  
  scale_linetype_manual(values = c("Historical (smoothed)" = "solid", "Projected" = "dashed")) +
  scale_x_continuous(breaks = seq(2012, 2050, by = 4),
                     expand = expansion(mult = c(0.01, 0.02)))+
  labs(
    title = paste0(state_name," per Capita GHG Emissions"),
    subtitle= "Historical (2012–2019 smoothed) & Projected (through 2050)",
    #caption= "*Projection uses avgerage geometric mean annual percent change computed on smoothed series (Following Vogel & Hickel, 2023)",
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

## Percent change projected for all states (Vogel & Hickel-style smoothing + geometric mean rate)

library(readxl)
library(dplyr)
library(ggplot2)
library(tidyr)
library(rlang)

# ---- 1) Read data ----
file <- "State GHG Data Compilation.xlsx"
cbe <- read_excel(file, sheet = "CBE Per Cap")
te  <- read_excel(file, sheet = "TE Per Cap")

id_col <- names(cbe)[1]
years_hist   <- 2012:2019
future_years <- 2020:2050
year_cols    <- as.character(years_hist)

state_lookup <- tibble::tibble(
  state_code = state.abb,
  state_name = state.name)

# ---- 2) Output folder ----
output_dir <- "State_Emissions_Projections_GMAR"
if (!dir.exists(output_dir)) dir.create(output_dir)

# ---- 3) Symmetric 5y moving average (3y at ends), per Vogel & Hickel (2023) ----
vogel_hickel_smooth <- function(x, yrs) {
  out <- numeric(length(yrs)); names(out) <- as.character(yrs)
  for (t in yrs) {
    if (t == min(yrs))            win <- t:(t+2)                 # 3-year at start
    else if (t == min(yrs) + 1)   win <- (t-1):(t+3)             # asymmetric 5-year near start
    else if (t %in% (min(yrs)+2):(max(yrs)-2)) win <- (t-2):(t+2) # centered 5-year
    else if (t == max(yrs) - 1)   win <- (t-2):max(yrs)          # asymmetric 5-year near end
    else if (t == max(yrs))       win <- (t-2):t                 # 3-year at end
    out[as.character(t)] <- mean(x[as.character(win)], na.rm = TRUE)
  }
  out
}

# ---- 4) Average annual % change as geometric mean of YoY growth (on smoothed series) ----
rate_from_geom_mean <- function(x_smooth) {
  ok <- is.finite(x_smooth) & x_smooth > 0
  x  <- x_smooth[ok]
  if (length(x) < 2) return(NA_real_)
  r <- exp(mean(diff(log(x)))) - 1
  as.numeric(r)
}

# ---- 5) Loop over all states (exclude USA if desired) ----
states <- cbe[[id_col]]
states <- states[states != "USA"]  # exclude national row if present

for (state_code in states) {
  state_name <- state_lookup %>%
    filter(state_code == !!state_code) %>%
    pull(state_name)
  
  if (length(state_name) == 0) {
    warning(paste("State code not found:", state_code))
    next
  }
  # -- Extract state rows
  cbe_state <- cbe %>% filter(!!sym(id_col) == state_code)
  te_state  <- te  %>% filter(!!sym(id_col) == state_code)
  
  # -- Raw numeric series (2012–2019)
  cbe_hist_raw <- as.numeric(cbe_state[, year_cols])
  te_hist_raw  <- as.numeric(te_state[,  year_cols])
  names(cbe_hist_raw) <- names(te_hist_raw) <- year_cols
  
  # -- Smooth with Vogel & Hickel rule
  cbe_hist_smooth <- vogel_hickel_smooth(cbe_hist_raw, years_hist)
  te_hist_smooth  <- vogel_hickel_smooth(te_hist_raw,  years_hist)
  
  # -- Compute average annual % change (geom. mean of YoY growth) from SMOOTHED series
  cbe_rate <- rate_from_geom_mean(cbe_hist_smooth)
  te_rate  <- rate_from_geom_mean(te_hist_smooth)
  
  # -- If rate can't be computed (NA), skip plotting for this state
  if (is.na(cbe_rate) || is.na(te_rate)) next
  
  # -- Project to 2050 from smoothed 2019 level
  cbe_2019 <- cbe_hist_smooth["2019"]
  te_2019  <- te_hist_smooth["2019"]
  
  cbe_proj <- cbe_2019 * (1 + cbe_rate)^(future_years - 2019)
  te_proj  <- te_2019  * (1 + te_rate)^(future_years - 2019)
  
  # -- Combine smoothed historical + projected
  data_hist <- tibble(
    Year = years_hist,
    CBE  = as.numeric(cbe_hist_smooth),
    TE   = as.numeric(te_hist_smooth),
    Period = "Historical (smoothed)"
  )
  
  data_future <- tibble(
    Year = future_years,
    CBE  = cbe_proj,
    TE   = te_proj,
    Period = "Projected"
  )
  
  data_all <- bind_rows(data_hist, data_future) |>
    pivot_longer(c(CBE, TE), names_to = "Type", values_to = "Emissions")
  
  # -- Axis padding
  y_max <- max(data_all$Emissions, na.rm = TRUE) * 1.05
  y_min <- min(data_all$Emissions, na.rm = TRUE) * 0.95
  
  # -- Plot
  labels_data <- data_all %>% 
    filter(Year %in% c(2012,2019, 2050))
  
  p <- ggplot(data_all, aes(x = Year, y = Emissions, color = Type, linetype = Period)) +
    
    geom_line(
      data = subset(data_all, Period == "Historical (smoothed)"),
      linewidth = 1.2,
      alpha = 1
    ) +
    geom_point(data = subset(data_all, Period == "Historical"), size = 2.5) +
    geom_line(
      data = subset(data_all, Period == "Projected"),
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
                        labels = c("Consumption-based emissions", "Territorial emissions")) +
    
    scale_linetype_manual(values = c("Historical (smoothed)" = "solid", "Projected" = "dashed")) +
    scale_x_continuous(breaks = seq(2012, 2050, by = 4),
                       expand = expansion(mult = c(0.01, 0.02)))+
    labs(
      title = paste0(state_name," per Capita GHG Emissions"),
      subtitle= "Historical (2012–2019 smoothed) & Projected (through 2050)",
      #caption= "*Projection uses avgerage geometric mean annual percent change computed on smoothed series (Following Vogel & Hickel, 2023)",
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
  
    
  # -- Save
  ggsave(filename = file.path(output_dir, paste0(state_code, "_Emissions_Projection_GMAR_2050.pdf")),
    plot = p, width = 12, height = 7)
  ggsave(filename = file.path(output_dir, paste0(state_code, "_Emissions_Projection_GMAR_2050.png")),
         plot = p, width = 12, height = 7, dpi = 300)
}


## When will states drop below 2 tCO2e per capita? (stable + fast)
## - Symmetric 5y MA (3y ends) smoothing (Vogel & Hickel, 2023)
## - Rate = geometric mean of YoY growth on smoothed series
## - Closed-form crossing year (no while loops)

library(readxl)
library(dplyr)
library(tidyr)
library(rlang)

# ---- 1) Read data ----
file <- "State GHG Data Compilation.xlsx"
cbe <- read_excel(file, sheet = "CBE Per Cap")
te  <- read_excel(file, sheet = "TE Per Cap")

id_col <- names(cbe)[1]
years_hist <- 2012:2019
year_cols  <- as.character(years_hist)

# ---- 2) Output folder ----
output_dir <- "State_Emissions_Projections"
if (!dir.exists(output_dir)) dir.create(output_dir)

# ---- 3) Keep USA, drop "Total" if present ----
non_states <- c("Total")
cbe <- cbe %>% filter(!.data[[id_col]] %in% non_states)
te  <- te  %>% filter(!.data[[id_col]] %in% non_states)

states <- cbe[[id_col]]  # includes USA if present

# ---- 4) Smoothing helper: symmetric 5y MA (3y at ends) ----
vogel_hickel_smooth <- function(x, yrs) {
  out <- numeric(length(yrs)); names(out) <- as.character(yrs)
  for (t in yrs) {
    if (t == min(yrs))            win <- t:(t+2)                 # 3-year at start
    else if (t == min(yrs) + 1)   win <- (t-1):(t+3)             # asymmetric 5-year near start
    else if (t %in% (min(yrs)+2):(max(yrs)-2)) win <- (t-2):(t+2) # centered 5-year
    else if (t == max(yrs) - 1)   win <- (t-2):max(yrs)          # asymmetric 5-year near end
    else if (t == max(yrs))       win <- (t-2):t                 # 3-year at end
    out[as.character(t)] <- mean(x[as.character(win)], na.rm = TRUE)
  }
  out
}

# ---- 5) Average annual % change = geometric mean of YoY growth (on smoothed series) ----
rate_from_geom_mean <- function(x_smooth) {
  ok <- is.finite(x_smooth) & x_smooth > 0
  x  <- x_smooth[ok]
  if (length(x) < 2) return(NA_real_)
  exp(mean(diff(log(x)))) - 1
}

# ---- 6) Closed-form threshold crossing (no loops) ----
cross_year_closed <- function(level_2019, rate, start_year = 2019, threshold = 2, max_year = 5000) {
  if (is.na(level_2019) || is.na(rate) || level_2019 <= 0) return(NA_integer_)
  if (rate >= 0) return(NA_integer_)            # not declining, won't cross
  if ((1 + rate) <= 0) return(NA_integer_)      # invalid growth factor
  
  t <- log(threshold / level_2019) / log(1 + rate)  # years after start_year
  if (!is.finite(t) || t < 0) return(NA_integer_)
  yr <- ceiling(start_year + t)
  if (yr > max_year) NA_integer_ else yr
}

# ---- 7) Main loop ----
threshold <- 2
base_year_end <- 2019
results <- vector("list", length(states)); names(results) <- states

message("Processing ", length(states), " regions...")

for (state_code in states) {
  # Extract state rows efficiently
  cbe_state <- cbe %>% filter(.data[[id_col]] == state_code)
  te_state  <- te  %>% filter(.data[[id_col]] == state_code)
  
  # Get raw numeric vectors for 2012–2019 (use unlist to flatten tibble cols)
  cbe_raw <- suppressWarnings(as.numeric(unlist(cbe_state[, year_cols])))
  te_raw  <- suppressWarnings(as.numeric(unlist(te_state[,  year_cols])))
  names(cbe_raw) <- names(te_raw) <- year_cols
  
  # Skip if no data
  if (all(is.na(cbe_raw)) && all(is.na(te_raw))) next
  
  # Smooth (Vogel & Hickel)
  cbe_sm <- vogel_hickel_smooth(cbe_raw, years_hist)
  te_sm  <- vogel_hickel_smooth(te_raw,  years_hist)
  
  # Rate from geometric mean of YoY growth (smoothed series)
  cbe_rate <- rate_from_geom_mean(cbe_sm)
  te_rate  <- rate_from_geom_mean(te_sm)
  
  # Levels at 2019 (smoothed)
  cbe_2019 <- cbe_sm["2019"]
  te_2019  <- te_sm["2019"]
  
  # Crossing years (closed-form)
  year_cbe <- cross_year_closed(cbe_2019, cbe_rate, start_year = base_year_end, threshold = threshold)
  year_te  <- cross_year_closed(te_2019,  te_rate,  start_year = base_year_end, threshold = threshold)
  
  results[[state_code]] <- tibble(
    State             = state_code,
    CBE_2019_smoothed = cbe_2019,
    CBE_rate_geom     = cbe_rate,
    Year_CBE_below2   = year_cbe,
    TE_2019_smoothed  = te_2019,
    TE_rate_geom      = te_rate,
    Year_TE_below2    = year_te
  )
}

# ---- set a target year once ----
target_year <- 2050

# ---- 8) Combine & compute "Years to reach < 2 t" + 2050 predictions ----
results_df <- bind_rows(results) %>%
  mutate(
    Years_to_2_CBE = ifelse(!is.na(Year_CBE_below2), Year_CBE_below2 - base_year_end, NA),
    Years_to_2_TE  = ifelse(!is.na(Year_TE_below2),  Year_TE_below2  - base_year_end, NA),
    
    # 2050 predictions (guard against invalid rates)
    CBE_2050_pred = ifelse(
      is.finite(CBE_2019_smoothed) & is.finite(CBE_rate_geom) & (1 + CBE_rate_geom) > 0,
      CBE_2019_smoothed * (1 + CBE_rate_geom)^(target_year - base_year_end),
      NA_real_
    ),
    TE_2050_pred = ifelse(
      is.finite(TE_2019_smoothed) & is.finite(TE_rate_geom) & (1 + TE_rate_geom) > 0,
      TE_2019_smoothed * (1 + TE_rate_geom)^(target_year - base_year_end),
      NA_real_
    )
  ) %>%
  arrange(Years_to_2_CBE)

print(results_df)

# ---- 9) Save CSV ----
out_csv <- file.path(output_dir, "State_Years_Below_2t_fast.csv")  # keep filename or rename if you like
suppressWarnings(write.csv(results_df, out_csv, row.names = FALSE))
cat("Saved results to:", out_csv, "\n")


##when will states reduce emissions by 90% compared to 2019 levels

library(readxl)
library(dplyr)
library(tidyr)
library(rlang)

# ---- 1) Read data (TOTALS, not per capita) ----
file <- "State GHG Data Compilation.xlsx"
cbe <- read_excel(file, sheet = "CBE Total")  # CHANGED
te  <- read_excel(file, sheet = "TE Total")  # CHANGED

id_col <- names(cbe)[1]
years_hist <- 2012:2019
year_cols  <- as.character(years_hist)

# ---- 2) Output folder ----
output_dir <- "State_Emissions_Projections"
if (!dir.exists(output_dir)) dir.create(output_dir)

# ---- 3) Keep USA, drop "Total" if present ----
non_states <- c("Total")
cbe <- cbe %>% filter(!.data[[id_col]] %in% non_states)
te  <- te  %>% filter(!.data[[id_col]] %in% non_states)

states <- cbe[[id_col]]  # includes USA if present

# ---- 4) Smoothing helper: symmetric 5y MA (3y at ends) ----
vogel_hickel_smooth <- function(x, yrs) {
  out <- numeric(length(yrs)); names(out) <- as.character(yrs)
  for (t in yrs) {
    if (t == min(yrs))            win <- t:(t+2)                 # 3-year at start
    else if (t == min(yrs) + 1)   win <- (t-1):(t+3)             # asymmetric 5-year near start
    else if (t %in% (min(yrs)+2):(max(yrs)-2)) win <- (t-2):(t+2) # centered 5-year
    else if (t == max(yrs) - 1)   win <- (t-2):max(yrs)          # asymmetric 5-year near end
    else if (t == max(yrs))       win <- (t-2):t                 # 3-year at end
    out[as.character(t)] <- mean(x[as.character(win)], na.rm = TRUE)
  }
  out
}

# ---- 5) Average annual % change = geometric mean of YoY growth (on smoothed series) ----
rate_from_geom_mean <- function(x_smooth) {
  ok <- is.finite(x_smooth) & x_smooth > 0
  x  <- x_smooth[ok]
  if (length(x) < 2) return(NA_real_)
  exp(mean(diff(log(x)))) - 1
}

# ---- 6) Closed-form threshold crossing (no loops) ----
# Same function, but we'll now feed it a threshold that is 10% of the 2019 level
cross_year_closed <- function(level_2019, rate, start_year = 2019, threshold, max_year = 5000) {
  if (is.na(level_2019) || is.na(rate) || level_2019 <= 0) return(NA_integer_)
  if (rate >= 0) return(NA_integer_)            # not declining, won't cross
  if ((1 + rate) <= 0) return(NA_integer_)      # invalid growth factor
  
  t <- log(threshold / level_2019) / log(1 + rate)  # years after start_year
  if (!is.finite(t) || t < 0) return(NA_integer_)
  yr <- ceiling(start_year + t)
  if (yr > max_year) NA_integer_ else yr
}

# ---- 7) Main loop ----
reduction_fraction <- 0.10    # 90% below 2019 => 10% of 2019 level
base_year_end     <- 2019
results <- vector("list", length(states)); names(results) <- states

message("Processing ", length(states), " regions...")

for (state_code in states) {
  # Extract state rows efficiently
  cbe_state <- cbe %>% filter(.data[[id_col]] == state_code)
  te_state  <- te  %>% filter(.data[[id_col]] == state_code)
  
  # Get raw numeric vectors for 2012–2019
  cbe_raw <- suppressWarnings(as.numeric(unlist(cbe_state[, year_cols])))
  te_raw  <- suppressWarnings(as.numeric(unlist(te_state[,  year_cols])))
  # Assign names only if lengths match
  if (length(cbe_raw) == length(year_cols)) names(cbe_raw) <- year_cols
  if (length(te_raw)  == length(year_cols)) names(te_raw)  <- year_cols
  
  
  # Skip if no data
  if (all(is.na(cbe_raw)) && all(is.na(te_raw))) next
  
  # Smooth (Vogel & Hickel)
  cbe_sm <- vogel_hickel_smooth(cbe_raw, years_hist)
  te_sm  <- vogel_hickel_smooth(te_raw,  years_hist)
  
  # Rate from geometric mean of YoY growth (smoothed series)
  cbe_rate <- rate_from_geom_mean(cbe_sm)
  te_rate  <- rate_from_geom_mean(te_sm)
  
  # Levels at 2019 (smoothed TOTAL emissions)
  cbe_2019 <- cbe_sm["2019"]
  te_2019  <- te_sm["2019"]
  
  # Thresholds: 10% of 2019 levels (i.e. 90% reduction)
  cbe_threshold <- cbe_2019 * reduction_fraction
  te_threshold  <- te_2019  * reduction_fraction
  
  # Crossing years (closed-form) for reaching 90% reduction
  year_cbe_90 <- cross_year_closed(
    level_2019 = cbe_2019,
    rate       = cbe_rate,
    start_year = base_year_end,
    threshold  = cbe_threshold
  )
  year_te_90 <- cross_year_closed(
    level_2019 = te_2019,
    rate       = te_rate,
    start_year = base_year_end,
    threshold  = te_threshold
  )
  
  results[[state_code]] <- tibble(
    State                 = state_code,
    CBE_2019_total_sm     = cbe_2019,
    CBE_rate_geom         = cbe_rate,
    Year_CBE_90pct_red    = year_cbe_90,   # NEW NAME
    TE_2019_total_sm      = te_2019,
    TE_rate_geom          = te_rate,
    Year_TE_90pct_red     = year_te_90    # NEW NAME
  )
}

# ---- set a target year once ----
target_year <- 2050

# ---- 8) Combine & compute "Years to 90% reduction" + 2050 predictions ----
results_df <- bind_rows(results) %>%
  mutate(
    Years_to_90pct_red_CBE = ifelse(!is.na(Year_CBE_90pct_red),
                                    Year_CBE_90pct_red - base_year_end, NA),
    Years_to_90pct_red_TE  = ifelse(!is.na(Year_TE_90pct_red),
                                    Year_TE_90pct_red  - base_year_end, NA),
    
    # 2050 predictions (guard against invalid rates) – now in TOTAL emissions
    CBE_2050_total_pred = ifelse(
      is.finite(CBE_2019_total_sm) & is.finite(CBE_rate_geom) & (1 + CBE_rate_geom) > 0,
      CBE_2019_total_sm * (1 + CBE_rate_geom)^(target_year - base_year_end),
      NA_real_
    ),
    TE_2050_total_pred = ifelse(
      is.finite(TE_2019_total_sm) & is.finite(TE_rate_geom) & (1 + TE_rate_geom) > 0,
      TE_2019_total_sm * (1 + TE_rate_geom)^(target_year - base_year_end),
      NA_real_
    )
  ) %>%
  arrange(Years_to_90pct_red_CBE)

print(results_df)

# ---- 9) Save CSV ----
out_csv <- file.path(output_dir, "State_Years_90pct_Reduction_fast.csv")  # NEW FILENAME
suppressWarnings(write.csv(results_df, out_csv, row.names = FALSE))
cat("Saved results to:", out_csv, "\n")

