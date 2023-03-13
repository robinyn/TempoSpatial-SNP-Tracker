library(tidyverse)
library(readxl)
library(leaflet)
library(RSQLite)
library(leaflet.minicharts)

setwd("~/Dev/school/BINP29/popgen")

db = dbConnect(SQLite(), "0_data/popgen_SNP.sqlite")

res = dbSendQuery(db, "SELECT MAX(CAST(DateMean AS INTEGER)), MIN(CAST(DateMean AS INTEGER)) FROM sample_meta")

time_range = as.numeric(dbFetch(res))

dbClearResult(res)

res = dbSendQuery(db, "SELECT * FROM country_cords")
country_list = dbFetch(res)
dbClearResult(res)

res = dbSendQuery(db, "SELECT * FROM distM")
distM = dbFetch(res)
dbClearResult(res)

timestep = 500
SNP_ID = c("rs3094315", "rs6696609")
twoSNPs = TRUE
groupby = "distance"
dist_threshold = 1000

m = leaflet() %>%
  addTiles() %>%
  setView(20, 30, zoom = 2)
print(m)

dist_Matrix = function(dat){
  sample_dist= dat %>% 
    expand(MasterID, MasterID) %>% 
    rename("ID1"="MasterID...1", "ID2"="MasterID...2") %>% 
    left_join(distM, by=c("ID1"="id1", "ID2"="id2")) %>% 
    pivot_wider(everything(), names_from =ID2, values_from = dist) %>% 
    column_to_rownames(var ="ID1")
  
  rnames = rownames(sample_dist)
  
  sample_dist %>% 
    sapply(as.numeric) %>% 
    data.matrix
  
  rownames(sample_dist) = rnames
  
  return(sample_dist)
}

cluster_Samples = function(dat){
  if(nrow(dat)>1){
    distances = dist_Matrix(dat)
    cluster_tree = hclust(as.dist(distances))
    clusters = cutree(cluster_tree, h=dist_threshold*1000)
    
    clusters = clusters %>% 
      as.list() %>% 
      data.frame() %>% 
      pivot_longer(everything(), names_to = "MasterID", values_to = "Cluster")
    
    dat = dat %>% 
      merge(clusters, by="MasterID")
    
    print(dat)
    
  }else{
    dat$Cluster = 1
  }
  
  return(dat)
}

calc_Centroid = function(dat){
  
}

for(start_time in seq(time_range[1], time_range[2], by=-timestep)){
  end_time = start_time-timestep
  query = sprintf("SELECT MasterID, Long, Lat, Country FROM sample_meta WHERE CAST(DateMean AS INTEGER) <= %s AND CAST(DateMean AS INTEGER) >=%s", start_time, end_time)
  res = dbSendQuery(db, query)
  samples_to_plot = dbFetch(res)
  dbClearResult(res)
  
  if(nrow(samples_to_plot)>0){
    sample_list = paste0(sprintf("`%s`", samples_to_plot$MasterID), collapse=",")
    
    if(twoSNPs){
      query = sprintf("SELECT %s FROM main WHERE SNP_ID='%s' OR SNP_ID='%s'", sample_list, SNP_ID[1], SNP_ID[2])
      res = dbSendQuery(db, query)
      SNP_dat = dbFetch(res)
      dbClearResult(res)
      
      print(SNP_dat)
      
      SNP1 = SNP_dat[1,]
      SNP2 = SNP_dat[2,] 
      
      if(ncol(SNP_dat)==1){
        SNP1 = c(noquote(colnames(SNP_dat))) %>% 
          cbind(SNP1) %>% 
          as.data.frame() %>% 
          rename("MasterID"=".")
        
        SNP2 = c(noquote(colnames(SNP_dat))) %>% 
          cbind(SNP2) %>% 
          as.data.frame() %>% 
          rename("MasterID"=".")
      }else{
        SNP1 = SNP1 %>% pivot_longer(everything(), names_to = "MasterID", values_to = "SNP1")
        SNP2 = SNP2 %>% pivot_longer(everything(), names_to = "MasterID", values_to = "SNP2")
      }
      
      plot_dat = merge(samples_to_plot, SNP1, by="MasterID") %>% 
        merge(SNP2, by="MasterID")
      
      if(nrow(plot_dat)==0){
        next
      }

    }else{
      query = sprintf("SELECT %s FROM main WHERE SNP_ID='%s'", sample_list, SNP_ID[1])
      res = dbSendQuery(db, query)
      SNP_dat = dbFetch(res)
      dbClearResult(res) 
      
      SNP_dat = SNP_dat %>% pivot_longer(everything(), names_to = "MasterID", values_to = "SNP1")
      
      plot_dat = merge(samples_to_plot, SNP_dat, by="MasterID")
      
      if(nrow(plot_dat)==0){
        next
      }
      
    }
    
    switch(groupby,
           "none"={
             plot_dat = plot_dat %>%
               pivot_longer(-c(Long, Lat, MasterID, Country)) %>%
               count(Long, Lat, name, value) %>%
               pivot_wider(names_from = c(name, value), values_from = n) %>%
               replace(is.na(.), 0) %>% 
               relocate(sort(names(.)))
             print(plot_dat)
           },
           "country"={
             plot_dat = plot_dat %>%
               select(-c(Long, Lat)) %>% 
               pivot_longer(-c(MasterID, Country)) %>%
               count(Country, name, value) %>%
               pivot_wider(names_from = c(name, value), values_from = n) %>%
               replace(is.na(.), 0) %>% 
               relocate(sort(names(.))) %>% 
               group_by(Country) %>% 
               summarise(across(everything(), sum)) %>% 
               merge(country_list, by="Country")
             print(plot_dat)
           },
           "distance"={
             plot_dat = cluster_Samples(plot_dat)
             
           },
           "testing"={
             print(plot_dat)
             next
           })

    print(m %>% addMinicharts(
      lng=plot_dat$Long, lat=plot_dat$Lat,
      type="pie",
      chartdata=plot_dat[, !(colnames(plot_dat) %in% c("Country", "Lat", "Long", "MasterID"))]
    ))
    
    #print(m %>% addCircleMarkers(lng=as.numeric(plot_dat$Long), lat=as.numeric(plot_dat$Lat), color=as.factor(plot_dat$SNP1)))
  }else{
    print(m)
  }
  
  Sys.sleep(2)
}

dbDisconnect(db)