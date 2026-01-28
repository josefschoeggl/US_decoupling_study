# import
#library(xlsx)
library(readxl) ##### Eneded up using readxl library instead of the xlsx one due to java import error. 

# GDP per state
gdp <- read_excel("data/State GHG Data Compilation.xlsx", sheet = "GDP Total") #sheetIndex = 1)
head(gdp)
str(gdp)

# GDP per state and capacity
gdpc <-read_excel("data/State GHG Data Compilation.xlsx", sheet = "GDP Per Cap") #read.xlsx("data/State GHG Data Compilation.xlsx", sheetIndex = 2)
head(gdpc)
str(gdpc)

# Territorial emissions total
te <- read_excel("data/State GHG Data Compilation.xlsx", sheet = "TE Total")
head(te)
str(te)

# Territorial emissions per capity
tec <- read_excel("data/State GHG Data Compilation.xlsx", sheet = "TE Per Cap")
head(tec)
str(tec)

# Consumption based emissions
cbe <- read_excel("data/State GHG Data Compilation.xlsx", sheet = "CBE Total") 
head(cbe)
str(cbe)

# Consumption based emissions per capita
cbec <- read_excel("data/State GHG Data Compilation.xlsx", sheet = "CBE Per Cap")
head(cbec)
str(cbec)


