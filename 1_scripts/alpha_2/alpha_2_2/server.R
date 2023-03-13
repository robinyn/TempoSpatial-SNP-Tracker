library(shiny)
library(shinythemes)
library(tidyverse)
library(leaflet)
library(leaflet.minicharts)
library(RSQLite)

# Main server function
server = function(input, output, session){
  # Server-wise global reactive variable for number of SNPs
  twoSNPs = reactive({
    if(input$num_SNP==1){
      FALSE
    }else{
      TRUE
    }
  })
  
  dist_threshold = reactive({
    as.numeric(input$group_dist_input)
  })
  
  # Initial map render
  output$map = renderLeaflet({
    leaflet() %>% 
      addTiles(options=providerTileOptions(minZoom=2)) %>% 
      setView(30, 50, zoom = 2) %>% 
      setMaxBounds(-90,-180,90,180)
  })
  
  # Listen to CHR1 choice 
  observeEvent(input$CHR_choice1, {
    CHR_ID = input$CHR_choice1
    res = dbSendQuery(db, sprintf("SELECT SNP_ID FROM SNP_meta WHERE CHR=%s", CHR_ID))
    SNP_selection = dbFetch(res)
    dbClearResult(res)
    
    updateSelectizeInput(session,"SNP_choice1",choices=unlist(SNP_selection$SNP_ID), server=TRUE)
  })
  
  # Listen to CHR2 choice
  observeEvent(input$CHR_choice2, {
    CHR_ID = input$CHR_choice2
    res = dbSendQuery(db, sprintf("SELECT SNP_ID FROM SNP_meta WHERE CHR=%s", CHR_ID))
    SNP_selection = dbFetch(res)
    dbClearResult(res)
    
    updateSelectizeInput(session,"SNP_choice2",choices=unlist(SNP_selection$SNP_ID), server=TRUE)
  })
  
  # Listen to time step change
  observeEvent(input$timestep, {
    time_step=input$timestep
    updateSliderTextInput(session, inputId="timeline", 
                          choices=seq(from=time_range[1], to=time_range[2], by=-time_step))
  })
  
  # Filter data reactive to timeline change
  filteredData = reactive({
    start_time = input$timeline
    end_time = input$timeline-time_step
    query = sprintf("SELECT MasterID, Long, Lat, Country FROM sample_meta WHERE CAST(DateMean AS INTEGER) <= %s AND CAST(DateMean AS INTEGER) >=%s", start_time, end_time)
    res = dbSendQuery(db, query)
    samples_to_plot = dbFetch(res)
    dbClearResult(res)
    
    if(nrow(samples_to_plot)==0){
      return(NULL)
    }
    
    sample_list = paste0(sprintf("`%s`", samples_to_plot$MasterID), collapse=",")
    if(twoSNPs()){
      query = sprintf("SELECT %s FROM main WHERE SNP_ID='%s' OR SNP_ID='%s'", sample_list, input$SNP_choice1, input$SNP_choice2)
      res = dbSendQuery(db, query)
      SNP_dat = dbFetch(res)
      dbClearResult(res)
      
      SNP1 = SNP_dat[1,]
      SNP2 = SNP_dat[2,] 
      
      if(input$SNP_choice1==input$SNP_choice2){
        SNP2 = SNP1
      }
      
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
      
    }else{
      query = sprintf("SELECT %s FROM main WHERE SNP_ID='%s'", sample_list, input$SNP_choice1)
      res = dbSendQuery(db, query)
      SNP_dat = dbFetch(res)
      dbClearResult(res) 
      
      SNP_dat = SNP_dat %>% pivot_longer(everything(), names_to = "MasterID", values_to = "SNP1")
      
      plot_dat = merge(samples_to_plot, SNP_dat, by="MasterID")
      plot_dat = plot_dat[plot_dat$SNP1!="00",]
    }
    
    switch(input$grouping_var,
           "None"={
             plot_dat = plot_dat %>%
               pivot_longer(-c(Long, Lat, MasterID, Country)) %>%
               count(Long, Lat, name, value) %>%
               pivot_wider(names_from = c(name, value), values_from = n) %>%
               replace(is.na(.), 0) %>% 
               relocate(sort(names(.)))
           },
           "Country"={
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
           },
           "Distance"={
             plot_dat = cluster_Samples(plot_dat)
             centroids = calc_Centroid(plot_dat)
             
             plot_dat = plot_dat %>% 
               select(-c(MasterID, Long, Lat, Country)) %>% 
               pivot_longer(-c(Cluster)) %>% 
               count(Cluster, name, value) %>% 
               pivot_wider(names_from = c(name, value), values_from = n) %>%
               replace(is.na(.), 0) %>% 
               relocate(sort(names(.))) %>% 
               merge(centroids, by="Cluster")
           })
    
    return(plot_dat)
  })
  
  observe({
    subset_dat = filteredData()
    if(is.null(subset_dat)){
      leafletProxy("map") %>% 
        clearMinicharts()
      return()
    }
    if(nrow(subset_dat)==0){
      leafletProxy("map") %>% 
        clearMinicharts()
      return()
    }
    
    leafletProxy("map") %>% 
      clearMinicharts() %>% 
      addMinicharts(
        lng=subset_dat$Long, lat=subset_dat$Lat,
        type="pie",
        chartdata=subset_dat[, !(colnames(subset_dat) %in% c("Country", "Lat", "Long", "Cluster", "MasterID"))]
      )
  })
  
  # Functions for distance based clustering
  
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
      clusters = cutree(cluster_tree, h=dist_threshold()*1000)
      
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
    if(nrow(dat)==1){
      cluster_centroids = data.frame(matrix(NA, nrow=1,ncol=3))
      colnames(cluster_centroids) = c("Cluster", "Lat", "Long")
      
      cluster_centroids$Cluster = 1
      cluster_centroids$Lat = dat$Lat
      cluster_centroids$Long = dat$Long
      
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
}
