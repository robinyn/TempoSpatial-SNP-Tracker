library(shiny)
library(shinythemes)
library(tidyverse)
library(leaflet)
library(leaflet.minicharts)
library(RSQLite)

SNP_ID = c("rs3094315", "rs6696609")
twoSNPs = FALSE

server = function(input, output, session){
  output$map = renderLeaflet({
    leaflet() %>% 
      addTiles(options=providerTileOptions(minZoom=2)) %>% 
      setView(30, 50, zoom = 3) %>% 
      setMaxBounds(-90,-180,90,180)
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
  
  observeEvent(input$timestep, {
    time_step=input$timestep
  })
}
