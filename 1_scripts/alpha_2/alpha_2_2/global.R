library(RSQLite)

setwd("~/Dev/school/BINP29/popgen")
db = dbConnect(SQLite(), "0_data/popgen_SNP.sqlite")
res = dbSendQuery(db, "SELECT MAX(CAST(DateMean AS INTEGER)), MIN(CAST(DateMean AS INTEGER)) FROM sample_meta")
time_range = as.numeric(dbFetch(res))
dbClearResult(res)

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

time_step = 500