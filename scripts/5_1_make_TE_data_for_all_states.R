## make TE (teritorial emissions) data for all states
#  prereq: have all 50 Greenhouse Gas Inventory data saved as 'state'_TE.csv


# Define path as location with 'state'_TE.csv' for all states
path <- "..."


# Get all *_TE.csv files in the folder
files <- list.files(path, pattern = "_TE\\.csv$", full.names = TRUE)

# Extract state abbreviations from filenames
states <- sort(sub("_TE\\.csv$", "", basename(files)))  # alphabetical order

# Create empty dataframe with correct dimensions
TE <- data.frame(matrix(NA, nrow = length(states), ncol = 11))
rownames(TE) <- states
colnames(TE) <- 2012:2022

# Fill TE with Gross Total data
for (file in files) {
  state <- sub("_TE\\.csv$", "", basename(file))
  df <- read.csv(file)
  values <- as.numeric(as.character(df[df[,1] == "Gross total", paste0("X", 2012:2022)]))
  TE[state, ] <- values}

View(TE)
TE <- cbind(State = rownames(TE), TE)


# download as excel
# library(writexl)
# write_xlsx(TE, path)





