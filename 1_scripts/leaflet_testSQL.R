library(tidyverse)
library(readxl)
library(leaflet)
library(RSQLite)

setwd("~/Dev/school/BINP29/popgen")

db = dbConnect(SQLite(), "0_data/popgen_SNP.sqlite")

res = dbSendQuery(db, "SELECT MAX(CAST(DateMean AS INTEGER)), MIN(CAST(DateMean AS INTEGER)) FROM sample_meta")

time_range = as.numeric(dbFetch(res))

dbClearResult(res)

timestep = 500
SNP_ID = c("rs3094315", "rs6696609")
twoSNPs = FALSE

m = leaflet() %>%
  addTiles() %>%
  setView(20, 30, zoom = 2)
print(m)

for(start_time in seq(time_range[1], time_range[2], by=-timestep)){
  end_time = start_time-timestep
  query = sprintf("SELECT MasterID, Long, Lat, Country FROM sample_meta WHERE CAST(DateMean AS INTEGER) <= %s AND CAST(DateMean AS INTEGER) >=%s", start_time, end_time)
  res = dbSendQuery(db, query)
  samples_to_plot = dbFetch(res)
  dbClearResult(res)
  
  if(nrow(samples_to_plot)>0){
    sample_list = paste0(sprintf("`%s`", samples_to_plot$MasterID), collapse=",")
    
    if(twoSNPs){
      query = sprintf("SELECT %s FROM main WHERE SNP_ID='%s' AND SNP_ID='%s'", sample_list, SNP_ID[1], SNP_ID[2])
      res = dbSendQuery(db, query)
      SNP_dat = dbFetch(res)
      dbClearResult(res)
      
      SNP1 = SNP_dat[1,]
      SNP2 = SNP_dat[2,] 
      
      SNP1 = SNP1 %>% pivot_longer(everything(), names_to = "MasterID", values_to = "SNP1")
      SNP2 = SNP2 %>% pivot_longer(everything(), names_to = "MasterID", values_to = "SNP2")
      
      plot_dat = merge(samples_to_plot, SNP1, by="MasterID") %>% 
        merge(SNP2, by="MasterID")
    }else{
      query = sprintf("SELECT %s FROM main WHERE SNP_ID='%s'", sample_list, SNP_ID[1])
      res = dbSendQuery(db, query)
      SNP_dat = dbFetch(res)
      dbClearResult(res) 
      
      SNP_dat = SNP_dat %>% pivot_longer(everything(), names_to = "MasterID", values_to = "SNP1")
      
      plot_dat = merge(samples_to_plot, SNP_dat, by="MasterID")
    }
    print(SNP_dat)
    
    plot_dat = plot_dat[plot_dat$SNP1!="00",]
    
    print(m %>% addCircleMarkers(lng=as.numeric(plot_dat$Long), lat=as.numeric(plot_dat$Lat), color=as.factor(plot_dat$SNP1)))
  }else{
    print(m)
  }
  
  Sys.sleep(2)
}

dbDisconnect(db)