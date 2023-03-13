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