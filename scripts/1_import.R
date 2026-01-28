# import
library(xlsx)

# GDP per state
gdp <- read.xlsx("data/State GHG Data Compilation.xlsx", sheetIndex = 1)
head(gdp)
str(gdp)

# GDP per state and capity
gdpc <- read.xlsx("data/State GHG Data Compilation.xlsx", sheetIndex = 2)
head(gdpc)
str(gdpc)

# Territorial emissions total
te <- read.xlsx("data/State GHG Data Compilation.xlsx", sheetIndex = 3)
head(te)
str(te)

# Territorial emissions per capity
tec <- read.xlsx("data/State GHG Data Compilation.xlsx", sheetIndex = 4)
head(tec)
str(tec)

# Consumption based emissions
cbe <- read.xlsx("data/State GHG Data Compilation.xlsx", sheetIndex = 5)
head(cbe)
str(cbe)

# Consumption based emissions per capita
cbec <- read.xlsx("data/State GHG Data Compilation.xlsx", sheetIndex = 6)
head(cbec)
str(cbec)


