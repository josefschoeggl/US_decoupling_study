# User Parameters -------------------------------------------------------------
years <- 2012:2019
spec <- "v1.1-GHG"     # Only "v1.1-GHGc" is limited to NY, VT, ME
saveresultdata <- FALSE
indicator <- "Greenhouse Gases"

# Determine model file prefix
modelstub <- if (spec == "v1.1-GHGc") {
  'EEIOv1.1-GHG-'
} else {
  'EEIOv1.1-GHGc-'
}

# Function to process a single state ------------------------------------------
process_state <- function(state) {
  # Skip if not supported by spec
  if (spec == "v1.1-GHGc" && !state %in% c("VT", "NY", "ME")) return(NULL)
  
  models <- list()
  for (y in years) {
    modelname <- paste0(state, modelstub, substr(y, 3, 4))
    file_path <- file.path("models", paste0(modelname, ".rds"))
    if (!file.exists(file_path)) {
      message(paste("Skipping", state, "Year", y, "- Model file not found."))
      return(NULL)
    }
    models[[paste0(state, "-", y)]] <- readRDS(file_path)
  }
  
  # Calculate Total Consumption-Based Emissions
  cbe_all <- sapply(models, calculateStateCBE, simplify=TRUE, USE.NAMES=TRUE)
  row.names(cbe_all) <- row.names(calculateStateCBE(models[[1]]))
  
  # Adjust results with household shares
  households_all <- sapply(models, calculateHouseholdShares, indicator=indicator, simplify=TRUE, USE.NAMES=TRUE)
  row.names(households_all) <- row.names(calculateHouseholdShares(models[[1]], indicator))
  cbe_all <- applyHouseholdSharestoResult(cbe_all, households_all)
  
  # Summarize to total emissions
  cbe_totals <- colSums(cbe_all)
  
  return(cbe_totals / 1e9)  # Convert to MMT
}

# Apply to all states ---------------------------------------------------------
state_results <- lapply(state.abb, process_state)
names(state_results) <- state.abb

# Remove NULLs (e.g. due to missing models)
state_results <- state_results[!sapply(state_results, is.null)]

# Combine into data frame: States x Years -------------------------------------
library(dplyr)
cbe_df <- do.call(rbind, state_results)  # rows = states, cols = years
cbe_df <- as.data.frame(cbe_df)
cbe_df$State <- rownames(cbe_df)

# Reorder columns: State, then years
cbe_df <- cbe_df[, c("State", as.character(years))]

# Final output ----------------------------------------------------------------
print("Total Consumption-Based Emissions (MMT) for All States:")
print(cbe_df)

# Optional: Save to CSV
if (saveresultdata) {
  write.csv(cbe_df, file = "CBE_AllStates.csv", row.names = FALSE)
}
