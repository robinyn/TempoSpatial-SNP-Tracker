library(tidyverse)
library(readxl)
library(leaflet)
library(RSQLite)

setwd("~/Dev/school/BINP29/popgen")

db = dbConnect(SQLite(), "0_data/popgen_SNP.sqlite")

res = dbSendQuery(db, "SELECT MAX(CAST(DateMean AS INTEGER)), MIN(CAST(DateMean AS INTEGER)) FROM sample_meta")

time_range = as.numeric(dbFetch(res))

dbClearResult(res)

timestep = 1000
SNP_ID = "rs3094315"

# m = leaflet() %>% 
#   addTiles() %>% 
#   setView(20, 30, zoom = 2)
# print(m)

for(start_time in seq(time_range[1], time_range[2], by=-timestep)){
  end_time = start_time-timestep
  query = sprintf("SELECT MasterID, Long, Lat, Country FROM sample_meta WHERE CAST(DateMean AS INTEGER) <= %s AND CAST(DateMean AS INTEGER) >=%s", start_time, end_time)
  res = dbSendQuery(db, query)
  samples_to_plot = dbFetch(res)
  dbClearResult(res)
  
  if(nrow(samples_to_plot)>0){
    sample_list = paste0(sprintf("'%s'", samples_to_plot$MasterID), collapse=",")
    
    print(sample_list)
    
    query = sprintf("SELECT %s FROM main WHERE SNP_ID='%s'", sample_list, SNP_ID)
    res = dbSendQuery(db, query)
    SNP_dat = dbFetch(res)
    dbClearResult(res)
    
    #print(SNP_dat) 
  }
}

dbDisconnect(db)