library(shiny)
library(shinythemes)
library(tidyverse)
library(leaflet)
library(leaflet.minicharts)
library(RSQLite)
library(RColorBrewer)

setwd("../")

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
  
  # Server-wise global reactive variable for distance threshold for clustering
  dist_threshold = reactive({
    as.numeric(input$group_dist_input)
  })
  
  # Palette choice (Qual, Seq, Div)
  plot_palette = reactive({
    input$palette_choice
  })
  
  # Plot type (Pie, Bar)
  plot_type = reactive({
  switch(input$plot_type,
         "Pie"=return("pie"),
         "Bar"=return("bar")
         )
  })
  
  # Opacity
  opacity_val = reactive({
    as.numeric(input$opacity)/100
  })
  
  plot_size = reactive({
    subset_dat = filteredData()
    num_rows = nrow(subset_dat)
    minimum_size = 10
    maximum_size = 100
    multiplier = (maximum_size-minimum_size)/100
    return((minimum_size+(multiplier*as.numeric(input$plot_size))))
  })
  
  legend_show = reactive({
    if(input$legend=="None"){
      return(FALSE)
    }else{
      return(TRUE)
    }
  })
  
  legend_pos = reactive({
    switch(input$legend,
           "TR"="topright",
           "TL"="topleft",
           "BR"="bottomright",
           "BL"="bottomleft")
  })
  
  label_show = reactive({
    if(!input$showlabel){
      leafletProxy("map") %>% 
        clearMarkers()
    }
    return(input$showlabel)
  })
  
  data_type = reactive({
    input$data_type
  })
  
  observeEvent(input$SNP_choice1,{
      SNP1_choice = input$SNP_choice1
      db = dbConnect(SQLite(), "data/reich_v50.sqlite")
      res = dbSendQuery(db, sprintf("SELECT REF, ALT FROM SNP_meta WHERE SNP_ID='%s'", SNP1_choice))
      SNP1_alleles = dbFetch(res)
      dbClearResult(res)
      dbDisconnect(db)
      
      SNP1_alleles[SNP1_alleles=="1"]="A"
      SNP1_alleles[SNP1_alleles=="2"]="C"
      SNP1_alleles[SNP1_alleles=="3"]="G"
      SNP1_alleles[SNP1_alleles=="4"]="T"
      
      output$SNP1_alleles=renderText(sprintf("REF: %s<br/>ALT: %s", SNP1_alleles[1], SNP1_alleles[2]))
  })
  
  observeEvent(input$SNP_choice2,{
    SNP2_choice = input$SNP_choice2
    db = dbConnect(SQLite(), "data/reich_v50.sqlite")
    res = dbSendQuery(db, sprintf("SELECT REF, ALT FROM SNP_meta WHERE SNP_ID='%s'", SNP2_choice))
    SNP2_alleles = dbFetch(res)
    dbClearResult(res)
    dbDisconnect(db)
    
    SNP2_alleles[SNP2_alleles=="1"]="A"
    SNP2_alleles[SNP2_alleles=="2"]="C"
    SNP2_alleles[SNP2_alleles=="3"]="G"
    SNP2_alleles[SNP2_alleles=="4"]="T"
    
    output$SNP2_alleles=renderText(sprintf("REF: %s<br/>ALT: %s", SNP2_alleles[1], SNP2_alleles[2]))
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
    db = dbConnect(SQLite(), "data/reich_v50.sqlite")
    res = dbSendQuery(db, sprintf("SELECT SNP_ID FROM SNP_meta WHERE CHR=%s", CHR_ID))
    SNP_selection = dbFetch(res)
    dbClearResult(res)
    dbDisconnect(db)
    
    updateSelectizeInput(session,"SNP_choice1",choices=unlist(SNP_selection$SNP_ID), server=TRUE)
  })
  
  # Listen to CHR2 choice
  observeEvent(input$CHR_choice2, {
    CHR_ID = input$CHR_choice2
    db = dbConnect(SQLite(), "data/reich_v50.sqlite")
    res = dbSendQuery(db, sprintf("SELECT SNP_ID FROM SNP_meta WHERE CHR=%s", CHR_ID))
    SNP_selection = dbFetch(res)
    dbClearResult(res)
    dbDisconnect(db)
    
    updateSelectizeInput(session,"SNP_choice2",choices=unlist(SNP_selection$SNP_ID), server=TRUE)
  })
  
  # Listen to time step change
  observeEvent(input$timestep, {
    time_step=input$timestep
    updateSliderTextInput(session, inputId="timeline", 
                          choices=seq(from=time_range[1], to=time_range[2], by=-time_step))
  })
  
  # Update color scheme choices based on palette choice
  observeEvent(input$palette_type, {
    palette_type_choice = input$palette_type
    updateSelectInput(session, "palette_choice", choices=color_palettes[[palette_type_choice]])
  }, ignoreInit=TRUE)
  
  # Filter data reactive to timeline change
  filteredData = reactive({
    db = dbConnect(SQLite(), "data/reich_v50.sqlite")
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
      
      plot_dat = plot_dat %>% 
        mutate(SNP1 = replace(SNP1, SNP1=="00", "Missing"), SNP2 = replace(SNP2, SNP2=="00", "Missing")) %>% 
        mutate(SNP1 = str_replace_all(SNP1, "1", "A"), SNP2 = str_replace_all(SNP2, "1", "A")) %>% 
        mutate(SNP1 = str_replace_all(SNP1, "2", "C"), SNP2 = str_replace_all(SNP2, "2", "C")) %>% 
        mutate(SNP1 = str_replace_all(SNP1, "3", "G"), SNP2 = str_replace_all(SNP2, "3", "G")) %>% 
        mutate(SNP1 = str_replace_all(SNP1, "4", "T"), SNP2 = str_replace_all(SNP2, "4", "T"))
      
    }else{
      query = sprintf("SELECT %s FROM main WHERE SNP_ID='%s'", sample_list, input$SNP_choice1)
      res = dbSendQuery(db, query)
      SNP_dat = dbFetch(res)
      dbClearResult(res) 
      
      SNP_dat = SNP_dat %>% pivot_longer(everything(), names_to = "MasterID", values_to = "SNP1")
      
      plot_dat = merge(samples_to_plot, SNP_dat, by="MasterID")
      plot_dat = plot_dat[plot_dat$SNP1!="00",]
      
      plot_dat = plot_dat %>% 
        #mutate(SNP1 = replace(SNP1, SNP1=="00", "Missing")) %>% 
        mutate(SNP1 = str_replace_all(SNP1, "1", "A")) %>% 
        mutate(SNP1 = str_replace_all(SNP1, "2", "C")) %>% 
        mutate(SNP1 = str_replace_all(SNP1, "3", "G")) %>% 
        mutate(SNP1 = str_replace_all(SNP1, "4", "T"))
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
    
    dbDisconnect(db)
    
    if(data_type()=="Freq"){
      plot_dat = allele_freq(plot_dat)
    }
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
    
    # MAIN CHART FUNCTION
    
    if(label_show()){
      switch(input$grouping_var,
             "None"={
               leafletProxy("map") %>% 
                 clearMarkers() %>% 
                 addLabelOnlyMarkers(lng = as.numeric(subset_dat$Long), lat = as.numeric(subset_dat$Lat), 
                                     label = sprintf("%.4f, %.4f", as.numeric(subset_dat$Long), as.numeric(subset_dat$Lat)),
                                     labelOptions = labelOptions(noHide = TRUE, textOnly = TRUE, 
                                                                 direction = "bottom", offset = c(0,5)))
             },
             "Country"={
               leafletProxy("map") %>% 
                 clearMarkers() %>% 
                 addLabelOnlyMarkers(lng = as.numeric(subset_dat$Long), lat = as.numeric(subset_dat$Lat), label = subset_dat$Country,
                                     labelOptions = labelOptions(noHide = TRUE, textOnly = TRUE, 
                                                                 direction = "bottom", offset = c(0,5)))
             },
             "Distance"={
               leafletProxy("map") %>% 
                 clearMarkers() %>% 
                 addLabelOnlyMarkers(lng = as.numeric(subset_dat$Long), lat = as.numeric(subset_dat$Lat), label = paste("Cluster",subset_dat$Cluster),
                                     labelOptions = labelOptions(noHide = TRUE, textOnly = TRUE, 
                                                                 direction = "bottom", offset = c(0,5)))
             })
    }
    
    leafletProxy("map") %>% 
      clearMinicharts() %>% 
      addMinicharts(
        lng = subset_dat$Long, lat=subset_dat$Lat,
        type = plot_type(),
        chartdata = subset_dat[, !(colnames(subset_dat) %in% disregard)],
        colorPalette = brewer.pal(n=9, plot_palette()),
        fillColor = brewer.pal(n=3, plot_palette())[3],
        legend = legend_show(),
        legendPosition = legend_pos(),
        opacity = opacity_val(),
        width = plot_size(),
        height = plot_size()
      )
    
    if(length(c(colnames(subset_dat)[!colnames(subset_dat) %in% disregard]))==1 && legend_show()){
      leafletProxy("map") %>% 
        addLegend(legend_pos(),labels=c(colnames(subset_dat)[!colnames(subset_dat) %in% disregard]),
                  colors = brewer.pal(n=3, plot_palette())[3], layerId="minichartsLegend")
    }
  })
  
  # Allele frequency calculations
  
  allele_freq = function(dat){
    uniqchars <- function(x) unique(unlist(strsplit(x, "")))
    switchchr <- function(x) paste(unlist(strsplit(x,""))[2],unlist(strsplit(x,""))[1], sep="")
    SNP1_alleles = c()
    SNP2_alleles = c()
    
    col_names_list = colnames(dat)[!colnames(dat) %in% disregard]
    alleles = str_split(col_names_list, "_", simplify = TRUE) %>% data.frame()
    colnames(alleles) = c("SNP", "Genotype")
    
    alleles = alleles[!alleles$Genotype=="Missing",]
    
    dat = dat %>% 
      select(-contains("Missing"))
    
    if(twoSNPs()){
      SNP1_alleles = uniqchars(alleles$Genotype[alleles$SNP=="SNP1"])
      SNP2_alleles = uniqchars(alleles$Genotype[alleles$SNP=="SNP2"])
      dat = dat %>% 
        mutate(SNP1_total=rowSums(select(.,contains("SNP1")))) %>% 
        mutate(SNP2_total=rowSums(select(.,contains("SNP2"))))
      
      SNP1_tab = dat %>% 
        select(contains("SNP1"))
      
      SNP2_tab = dat %>% 
        select(contains("SNP2"))
      
      if(length(SNP1_alleles)==1){
        allele_name = paste("SNP1_", SNP1_alleles[1], SNP1_alleles[1], sep="")
        col_name = paste("SNP1_", SNP1_alleles[1], sep="")
        
        SNP1_tab = SNP1_tab %>% 
          mutate({{col_name}}:=eval(parse(text={{allele_name}}))/SNP1_total)
      }else{
        allele1_name = paste("SNP1_", SNP1_alleles[1], SNP1_alleles[1], sep="") 
        allele2_name = paste("SNP1_", SNP1_alleles[2], SNP1_alleles[2], sep="") 
        col_name1 = paste("SNP1_", SNP1_alleles[1], sep="") 
        col_name2 = paste("SNP1_", SNP1_alleles[2], sep="") 
        
        SNP1_tab = SNP1_tab %>% 
          mutate({{col_name1}}:=(eval(parse(text={{allele1_name}}))+((SNP1_total - eval(parse(text={{allele1_name}})) - eval(parse(text={{allele2_name}})))/2))/SNP1_total) %>% 
          mutate({{col_name2}}:=(eval(parse(text={{allele2_name}}))+((SNP1_total - eval(parse(text={{allele1_name}})) - eval(parse(text={{allele2_name}})))/2))/SNP1_total) 
      }
      
      if(length(SNP2_alleles)==1){
        allele_name = paste("SNP2_", SNP2_alleles[1], SNP2_alleles[1], sep="")
        col_name = paste("SNP2_", SNP2_alleles[1], sep="")
        
        SNP2_tab = SNP2_tab %>% 
          mutate({{col_name}}:=eval(parse(text={{allele_name}}))/SNP2_total)
      }else{
        allele1_name = paste("SNP2_", SNP2_alleles[1], SNP2_alleles[1], sep="") 
        allele2_name = paste("SNP2_", SNP2_alleles[2], SNP2_alleles[2], sep="") 
        col_name1 = paste("SNP2_", SNP2_alleles[1], sep="") 
        col_name2 = paste("SNP2_", SNP2_alleles[2], sep="") 
        
        SNP2_tab = SNP2_tab %>% 
          mutate({{col_name1}}:=(eval(parse(text={{allele1_name}}))+((SNP2_total - eval(parse(text={{allele1_name}})) - eval(parse(text={{allele2_name}})))/2))/SNP2_total) %>% 
          mutate({{col_name2}}:=(eval(parse(text={{allele2_name}}))+((SNP2_total - eval(parse(text={{allele1_name}})) - eval(parse(text={{allele2_name}})))/2))/SNP2_total) 
      }
      
      SNP1_tab = SNP1_tab %>% 
        mutate(across(everything(), ~replace(.x, is.nan(.x), 0)))
      SNP2_tab = SNP2_tab %>% 
        mutate(across(everything(), ~replace(.x, is.nan(.x), 0)))
      
      dat = dat %>% 
        cbind(SNP1_tab) %>% 
        cbind(SNP2_tab) %>% 
        select(matches("Country|Cluster|Lat|Long|MasterID|^SNP[12]_[ACTG]$"))
      
    }else{
      SNP1_alleles = uniqchars(alleles$Genotype[alleles$SNP=="SNP1"])
      dat = dat %>% 
        mutate(SNP1_total=rowSums(select(.,contains("SNP1"))))
      SNP1_tab = dat %>% 
        select(contains("SNP1"))
      
      if(length(SNP1_alleles)==1){
        allele_name = paste("SNP1_", SNP1_alleles[1], SNP1_alleles[1], sep="")
        col_name = paste("SNP1_", SNP1_alleles[1], sep="")
        
        SNP1_tab = SNP1_tab %>% 
          mutate({{col_name}}:=eval(parse(text={{allele_name}}))/SNP1_total)
      }else{
        allele1_name = paste("SNP1_", SNP1_alleles[1], SNP1_alleles[1], sep="") 
        allele2_name = paste("SNP1_", SNP1_alleles[2], SNP1_alleles[2], sep="") 
        col_name1 = paste("SNP1_", SNP1_alleles[1], sep="") 
        col_name2 = paste("SNP1_", SNP1_alleles[2], sep="") 
        
        SNP1_tab = SNP1_tab %>% 
          mutate({{col_name1}}:=(eval(parse(text={{allele1_name}}))+((SNP1_total - eval(parse(text={{allele1_name}})) - eval(parse(text={{allele2_name}})))/2))/SNP1_total) %>% 
          mutate({{col_name2}}:=(eval(parse(text={{allele2_name}}))+((SNP1_total - eval(parse(text={{allele1_name}})) - eval(parse(text={{allele2_name}})))/2))/SNP1_total) 
      }
      
      SNP1_tab = SNP1_tab %>% 
        mutate(across(everything(), ~replace(.x, is.nan(.x), 0)))
      
      dat = dat %>% 
        cbind(SNP1_tab) %>% 
        select(matches("Country|Cluster|Lat|Long|MasterID|^SNP[12]_[ACTG]$"))
      
    }
    
    return(dat)
  }
  
  # Functions for distance based clustering
  
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
      clusters = cutree(cluster_tree, h=dist_threshold()*1000)
      
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
}
