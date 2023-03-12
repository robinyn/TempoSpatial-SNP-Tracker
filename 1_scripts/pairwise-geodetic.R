library(tidyverse)
library(RSQLite)

setwd("~/Dev/school/BINP29/popgen")

db = dbConnect(SQLite(), "0_data/popgen_SNP.sqlite")
res = dbSendQuery(db, "SELECT MasterID, Long, Lat FROM sample_meta")
sample_dat = dbFetch(res)
dbClearResult(res)

