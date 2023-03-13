library(shiny)
library(shinythemes)
library(tidyverse)
library(leaflet)
library(leaflet.minicharts)
library(RSQLite)

# Retrieve country centroid coordinates from SQL database
res = dbSendQuery(db, "SELECT * FROM country_cords")
country_list = dbFetch(res)
dbClearResult(res)

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
        chartdata=subset_dat[, !(colnames(subset_dat) %in% c("Country", "Lat", "Long"))]
      )
  })
}
