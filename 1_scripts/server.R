library(shiny)
library(shinythemes)
library(tidyverse)
library(leaflet)
library(leaflet.minicharts)
library(readxl)

setwd("~/Dev/school/BINP29/popgen")

db = dbConnect(SQLite(), "0_data/popgen_SNP.sqlite")
res = dbSendQuery(db, "SELECT MAX(CAST(DateMean AS INTEGER)), MIN(CAST(DateMean AS INTEGER)) FROM sample_meta")
time_range = as.numeric(dbFetch(res))
dbClearResult(res)

timestep = 500
SNP_ID = c("rs3094315", "rs6696609")
twoSNPs = FALSE

server = function(input, output){
  filteredData = reactive({
    start_time = input$animation
    end_time = input$animation-timestep
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
      plot_dat = plot_dat[plot_dat$SNP1!="00",]
    }
  })
  
  output$map=renderLeaflet(
    leaflet() %>% 
      addTiles() %>% 
      setView(30, 50, zoom = 3)
  )
  
  observe({
    subset_dat = filteredData()
    leafletProxy("map") %>% 
      clearShapes() %>% 
      addCircles(lng=as.numeric(subset_dat$Long), lat=as.numeric(subset_dat$Lat), radius=5, fillOpacity=0.2)
  })
}