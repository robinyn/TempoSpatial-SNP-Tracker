library(shiny)
library(shinythemes)
library(tidyverse)
library(leaflet)
library(leaflet.minicharts)
library(readxl)

setwd("~/Dev/school/BINP29/popgen")

time_dat = read_excel("0_data/uncompressed/DataS1.xlsx")
SNP_dat = read.delim("temp/subset_3/DataS1_subset.ped", sep=" ", header=FALSE)

SNP_dat = SNP_dat %>% 
  select(-c(V3,V4))

colnames(SNP_dat) = c("CountryID", "SampleID", "Sex", "Phenotype", paste("SNP",seq(1:(ncol(SNP_dat)-4)),sep=""))

time_dat = time_dat[!duplicated(time_dat$MasterID),]

time_dat = time_dat[time_dat$MasterID %in% SNP_dat$SampleID,]
SNP_dat = SNP_dat[SNP_dat$SampleID %in% time_dat$MasterID,]

time_dat = time_dat[order(time_dat$MasterID),]
SNP_dat = SNP_dat[order(SNP_dat$SampleID),]

SNP_dat = as.data.frame(append(SNP_dat, list(time_dat$DateMean, time_dat$Lat, time_dat$Long), after=4))
colnames(SNP_dat) = c("CountryID", "SampleID", "Sex", "Phenotype", "DateMean", "Lat", "Long", paste("SNP",seq(1:(ncol(SNP_dat)-7)),sep=""))

start_time = max(SNP_dat$DateMean)
end_time = min(SNP_dat$DateMean)
time_step = 500

server = function(input, output){
  filteredData = reactive({
    from = input$animation
    till = input$animation-time_step
    SNP_dat %>% filter(DateMean<=from & DateMean>=till)
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