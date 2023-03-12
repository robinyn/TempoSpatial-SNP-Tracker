library(tidyverse)
library(RSQLite)

make_distMatrix = function(data_matrix){
  sample_ids = data_matrix$MasterID
  distM = matrix(0, nrow=length(sample_ids), ncol=length(sample_ids),
                 dimnames = list(sample_ids, sample_ids))
  
  for(x in sample_ids){
    p1 = list(lon=as.numeric(sample_dat[sample_dat$MasterID==x,]$Long), 
              lat=as.numeric(sample_dat[sample_dat$MasterID==x,]$Lat))
    for(y in sample_ids){
      p2 = list(lon=as.numeric(sample_dat[sample_dat$MasterID==y,]$Long), 
                lat=as.numeric(sample_dat[sample_dat$MasterID==y,]$Lat))
      distM[x,y] = geodetic_calc(p1, p2)
    }
  }
  
  return(distM)
}

geodetic_calc = function(p1, p2){
  lon1 = p1$lon
  lat1 = p1$lat
  
  lon2 = p2$lon
  lat2 = p2$lat
  
  # Mean radius of Earth in meters (6371km)
  R = 6371*10^3
  
  # Longitude and Latitude values in radians
  lat1_radian = lat1 * (pi/180)
  lat2_radian = lat2 * (pi/180)
  delta_lat = (lat2-lat1) * (pi/180)
  delta_lon = (lon2-lon1) * (pi/180)
  
  # Haversine formula
  a = (sin(delta_lat/2))^2 + cos(lat1_radian) * cos(lat2_radian) * (sin(delta_lon/2))^2
  c = 2 * atan2(sqrt(a), sqrt(1-a))
  
  # Distance in meters
  d = R * c 
  
  return(d)
}

main = function(){
  setwd("~/Dev/school/BINP29/popgen")
  
  db = dbConnect(SQLite(), "0_data/popgen_SNP.sqlite")
  res = dbSendQuery(db, "SELECT MasterID, Long, Lat FROM sample_meta")
  sample_dat = dbFetch(res)
  dbClearResult(res)
  dbDisconnect(db)
  
  distM = make_distMatrix(sample_dat)
  
  distM_database = distM %>% 
    as.data.frame() %>% 
    rownames_to_column(var="id1") %>% 
    pivot_longer(!"id1", names_to = "id2", values_to = "dist")
  
  write.table(distM_database, file="0_data/distanceMatrix.tsv", sep="\t", row.names = FALSE, quote=FALSE)
}




