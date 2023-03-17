library(RSQLite)
library(tidyverse)

cluster_by = "Distance"
dist_threshold = 1000

setwd(dirname(rstudioapi::getSourceEditorContext()$path))

# Retrieve country centroid coordinates from SQL database
db = dbConnect(SQLite(), "../data/reich_v50.sqlite")
res = dbSendQuery(db, "SELECT * FROM country_cords")
country_list = dbFetch(res)
dbClearResult(res)

# Retrieve distance matrix from SQL database
db = dbConnect(SQLite(), "../data/reich_v50.sqlite")
res = dbSendQuery(db, "SELECT * FROM distM")
distM = dbFetch(res)
dbClearResult(res)
dbDisconnect(db)

sum_clusters=function(){
  cluster_groups = create_clusters()
  
    
}

create_clusters=function(){
  db = dbConnect(SQLite(), "../data/reich_v50.sqlite")
  query = sprintf("SELECT MasterID, Long, Lat, Country FROM sample_meta")
  res = dbSendQuery(db, query)
  clusters = dbFetch(res)
  dbClearResult(res)
  dbDisconnect(db)
  
  switch(cluster_by,
         "None"={
           clusters = clusters %>% 
             mutate(Lat = as.numeric(Lat), Long = as.numeric(Long)) %>% 
             group_by(Lat, Long) %>% 
             mutate(Cluster = cur_group_id())
         },
         "Country"={
           clusters = clusters %>% 
             mutate(Lat = as.numeric(Lat), Long = as.numeric(Long)) %>% 
             group_by(Country) %>% 
             mutate(Cluster = cur_group_id())
         },
         "Distance"={
           clusters = cluster_Samples(clusters)
           centroids = calc_Centroid(clusters)
           
           clusters = clusters %>%
             select(c(MasterID, Country, Cluster)) %>% 
             merge(centroids, by="Cluster")
         })
  
  plot_group = clusters %>% 
    select(Lat, Long, Cluster) %>% 
    distinct()
  
  return(clusters)
}

# Create distance matrix from distM
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

# Hierarchical clustering based on distance threshold
cluster_Samples = function(dat){
  if(nrow(dat)>1){
    distances = dist_Matrix(dat)
    cluster_tree = hclust(as.dist(distances))
    clusters = cutree(cluster_tree, h=dist_threshold*1000)
    
    clusters = clusters %>% 
      as.list() %>% 
      data.frame() %>% 
      pivot_longer(everything(), names_to = "MasterID", values_to = "Cluster") %>% 
      group_by(Cluster) 
    
    dat = dat %>% 
      merge(clusters, by="MasterID")
    
  }else{
    dat$Cluster = 1
  }
  
  return(dat)
}

# Calculate centroids of clusters
calc_Centroid = function(dat){
  if(length(unique(dat$Cluster))==1){
    cluster_centroids = data.frame(matrix(NA, nrow=1,ncol=3))
    colnames(cluster_centroids) = c("Cluster", "Lat", "Long")
    
    cluster_centroids$Cluster = 1
    cluster_centroids$Lat = dat$Lat[1]
    cluster_centroids$Long = dat$Long[1]
    
    return(cluster_centroids)
  }
  
  cluster_groups = dat %>% 
    group_by(Cluster, Lat, Long) %>% 
    summarise() %>% 
    sapply(as.numeric) %>% 
    data.frame()
  
  cluster_centroids = data.frame(matrix(NA, nrow=length(unique(cluster_groups$Cluster)),ncol=3))
  colnames(cluster_centroids) = c("Cluster", "Lat", "Long")
  
  cluster_centroids$Cluster = unique(cluster_groups$Cluster)
  
  for(cluster in unique(cluster_groups$Cluster)){
    lat_c = sum(cluster_groups$Lat[cluster_groups$Cluster==cluster])/length(cluster_groups$Lat[cluster_groups$Cluster==cluster])
    long_c = sum(cluster_groups$Long[cluster_groups$Cluster==cluster])/length(cluster_groups$Long[cluster_groups$Cluster==cluster])
    
    cluster_centroids$Lat[cluster_centroids$Cluster==cluster] = lat_c
    cluster_centroids$Long[cluster_centroids$Cluster==cluster] = long_c
  }
  
  return(cluster_centroids)
}

sum_of_clusters = sum_clusters()