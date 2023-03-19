library(RSQLite)

setwd("../")
db = dbConnect(SQLite(), "data/reich_v50.sqlite")
res = dbSendQuery(db, "SELECT MAX(CAST(DateMean AS INTEGER)), MIN(CAST(DateMean AS INTEGER)) FROM sample_meta")
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
dbDisconnect(db)

time_step = 500

Sequential = c("YlOrRd", "YlOrBr", "YlGnBu", "YlGn", "Reds", "RdPu", "Purples", "PuRd", "PuBuGn", "PuBu", "OrRd", "Oranges",
                  "Greys", "Greens", "GnBu", "BuPu", "BuGn", "Blues")
Qualitative = c("Set1", "Set2", "Set3", "Pastel1", "Pastel2", "Paired","Dark2", "Accent")

Diverging = c("Spectral", "RdYlGn", "RdGy", "RdBu", "PuOr", "PRGn", "PiYG", "BrBG")

color_palettes = list("Sequential"=Sequential, "Qualitative"=Qualitative, "Divergent"=Diverging)

disregard = c("Country", "Lat", "Long", "Cluster", "MasterID")

sql_datasets = c("reich_v50")