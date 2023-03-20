# Title: global.R
# Author: Euisuk (Robin) Han
# Date: 19/Mar/2023
# Description: This is the global variables/computations script for the R-shiny application "TempoSpatial SNP Tracker"
#              All of the variables defined in this script is defined in the global scope and is made accessible to the 
#              entire application.
# Dependencies: This script requires the following packages:
#               RSQLite (version 2.3.0)
# ** Note: Newer versions of the packages may not be compatible **
# =======================================================================================================================================

library(RSQLite)

# Set working directory to the root folder
setwd(".")

# Create SQL database connection 
dbPath = "./reich_v50.sqlite"
db = dbConnect(SQLite(), dbPath)

# Query max/min dates from the database 
res = dbSendQuery(db, "SELECT MAX(CAST(DateMean AS INTEGER)), MIN(CAST(DateMean AS INTEGER)) FROM sample_meta")

# Define global variable for the time range of the dataset
time_range = as.numeric(dbFetch(res))
dbClearResult(res)

# Retrieve unique chromosomes from SNP data
res = dbSendQuery(db, "SELECT DISTINCT CHR FROM SNP_meta")
CHR_selection = dbFetch(res)
dbClearResult(res )

# Retrieve country centroid coordinates from SQL database
res = dbSendQuery(db, "SELECT * FROM country_cords")
country_list = dbFetch(res)
dbClearResult(res)

# Retrieve distance matrix from SQL database
res = dbSendQuery(db, "SELECT * FROM distM")
distM = dbFetch(res)
dbClearResult(res)

# Disconnect SQL connection
dbDisconnect(db)

# Define global variable for timestep and set default 
time_step = 500

# List of RColorBrewer palettes
Sequential = c("YlOrRd", "YlOrBr", "YlGnBu", "YlGn", "Reds", "RdPu", "Purples", "PuRd", "PuBuGn", "PuBu", "OrRd", "Oranges",
                  "Greys", "Greens", "GnBu", "BuPu", "BuGn", "Blues")
Qualitative = c("Set1", "Set2", "Set3", "Pastel1", "Pastel2", "Paired","Dark2", "Accent")

Diverging = c("Spectral", "RdYlGn", "RdGy", "RdBu", "PuOr", "PRGn", "PiYG", "BrBG")

color_palettes = list("Sequential"=Sequential, "Qualitative"=Qualitative, "Divergent"=Diverging)

# ColumnIDs that can be disregarded when pivoting dataframes
disregard = c("Country", "Lat", "Long", "Cluster", "MasterID")

# List of database files (for future expansion with ability to accept user input data)
sql_datasets = c("reich_v50")