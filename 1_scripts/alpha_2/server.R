library(shiny)
library(shinythemes)
library(tidyverse)
library(leaflet)
library(leaflet.minicharts)
library(RSQLite)

SNP_ID1=""
SNP_ID2=""
twoSNPs = FALSE

server = function(input, output, session){
  output$map = renderLeaflet({
    leaflet() %>% 
      addTiles(options=providerTileOptions(minZoom=2)) %>% 
      setView(30, 50, zoom = 2) %>% 
      setMaxBounds(-90,-180,90,180)
  })

  observeEvent(input$num_SNP, {
    if(input$num_SNP==1){
      twoSNPs = FALSE
    }else{
      twoSNPs = TRUE
    }
  })
  
  observeEvent(input$CHR_choice1, {
    CHR_ID = input$CHR_choice1
    res = dbSendQuery(db, sprintf("SELECT SNP_ID FROM SNP_meta WHERE CHR=%s", CHR_ID))
    SNP_selection = dbFetch(res)
    dbClearResult(res)
    
    updateSelectizeInput(session,"SNP_choice1",choices=unlist(SNP_selection$SNP_ID), server=TRUE)
  })
  
  observeEvent(input$CHR_choice2, {
    CHR_ID = input$CHR_choice2
    res = dbSendQuery(db, sprintf("SELECT SNP_ID FROM SNP_meta WHERE CHR=%s", CHR_ID))
    SNP_selection = dbFetch(res)
    dbClearResult(res)
    
    updateSelectizeInput(session,"SNP_choice2",choices=unlist(SNP_selection$SNP_ID), server=TRUE)
  })
  
  observeEvent(input$SNP_choice1, {
    SNP_ID1 = input$SNP_choice1
  })
  
  observeEvent(input$SNP_choice2, {
    SNP_ID2 = input$SNP_choice2
  }) 
  
  observeEvent(input$timestep, {
    time_step=input$timestep
  })
}
