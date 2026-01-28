####DF for 50 states####

library(readxl)
library(dplyr)
library(ggplot2)
library(tidyr)

# ---- Load the data ----
file_path <- "State GHG Data Compilation.xlsx"

gdp_pc <- read_excel(file_path, sheet = "GDP Per Cap")
te_pc  <- read_excel(file_path, sheet = "TE Per Cap")
cbe_pc <- read_excel(file_path, sheet = "CBE Per Cap")

# ---- Identify state code column ----
id_col <- names(gdp_pc)[1]  

# ---- List of all 50 states ----
state_codes <- gdp_pc %>%
  filter(!!sym(id_col) != "USA") %>%
  pull(!!sym(id_col))

print(state_codes)
state_codes <- head(state_codes, length(state_codes) - 9) #Remove unwanted state/region data like Midwest
print(state_codes)
state_lookup <- setNames(state.name, state.abb)


# ---- Function to create a tidy dataframe per state ----

create_state_df <- function(state_code) {
  
  # Helper function for one dataset
  extract_series <- function(df, value_name) {
    df %>%
      filter(!!sym(id_col) == state_code) %>%
      pivot_longer(
        cols = matches("^X?\\d{4}$"),
        names_to = "Year",
        values_to = value_name
      ) %>%
      mutate(
        Year = as.numeric(gsub("X", "", Year))
      ) %>%
      arrange(Year) %>%
      select(Year, !!sym(value_name))
  }
  
  gdp <- extract_series(gdp_pc, "GDP_per_cap")
  te  <- extract_series(te_pc,  "TE_per_cap")
  cbe <- extract_series(cbe_pc, "CBE_per_cap")
  
  # Combine safely
  df <- gdp %>%
    left_join(te,  by = "Year") %>%
    left_join(cbe, by = "Year") %>%
    mutate(State = state_code,
           State_name = state_lookup[state_code]) %>%
    select(State,State_name, Year, GDP_per_cap, TE_per_cap, CBE_per_cap)
  
  return(df)
}

# ---- Combine all states into one dataframe ----
data_all_states <- lapply(state_codes, create_state_df) %>%
  bind_rows()

# ---- Check ----
head(data_all_states)
tail(data_all_states)

#### ---- Save Plot ----####
#png("All_States_GDP_Emissions.png", width = 3000, height = 4500, res = 200)
pdf("All_States_GDP_Emissions.pdf", width = 15, height = 22.5)

# Layout: 10 rows x 5 columns + 1 extra row for legend
layout(matrix(c(1:50, rep(51,5)), nrow=11, byrow=TRUE), heights=c(rep(1,10), 0.4))
par(mar=c(3,4,2,4),          # margins for individual plots
    mgp = c(1.5, 0.4, 0),      # axis label positioning
    tcl = -0.175  )  
par(oma = c(2, 2, 5, 2))

for (st in state_codes) {
  df <- data_all_states[data_all_states$State == st, ]

  # Plot GDP (left axis)
  plot(df$Year, df$GDP_per_cap, type="b", pch=16, col="#FFD700", lty=3,
       ylab="GDP per Capita (USD)", xlab="", main=state_lookup[st], xaxt="n")
  axis(1, at=df$Year)

  # Plot emissions (right axis) with independent scale
  par(new=TRUE)
  plot(df$Year, df$TE_per_cap, type="b", pch=16, col="orange",
       axes=FALSE, xlab="", ylab="",
       ylim=c(0, max(df$TE_per_cap, df$CBE_per_cap, na.rm=TRUE)))
  lines(df$Year, df$CBE_per_cap, type="b", pch=16, col="steelblue", lty=1)
  axis(4, cex.axis=1)
  mtext(expression("Emissions per Capita (t CO"[2]*"e)"), side=4, line=1.5, cex=0.7)
  
}

# Overall title
mtext("Per Capita Consumption-Based Emissions, Territorial Emissions, and GDP by U.S. State (2012 - 2019)", outer=TRUE, cex=1.6, line=2, font = 2)

# ---- Draw shared legend in reserved row ----
par(mar=c(0,0,0,0))
plot.new()
legend("center",
       legend = c("GDP per Capita", "Territorial Emissions", "Consumption-Based Emissions"),
       col = c("#FFD700", "orange", "steelblue"),
       lty = c(3,1,1),
       pch = 16,
       lwd = 2,
       bty = "n",
       horiz = TRUE,
       cex = 1.5)

dev.off()

