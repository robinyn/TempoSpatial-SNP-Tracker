library(tidyverse)
library(readxl)
library(ggplot2)
library(leaflet)
library(gganimate)
library(gifski)

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

start_time = max(time_dat$DateMean)
end_time = min(time_dat$DateMean)
time_step = 1000

m = leaflet() %>% 
  addTiles() %>% 
  setView(20, 30, zoom = 2)
print(m)

#print(m %>% addPolylines(lat = c(-10,10), lng=c(10,20)))

for(x in seq(start_time, (end_time+time_step), -time_step)){
  subset_dat = SNP_dat[(SNP_dat$DateMean<=x) & (SNP_dat$DateMean>=(x-time_step)),]

  if(nrow(subset_dat)!=0){
    subset_dat = subset_dat[subset_dat$SNP1!=0,]
    print(subset_dat)
    x_cords = as.numeric(subset_dat$Long)
    y_cords = as.numeric(subset_dat$Lat)
    color_vec = c()

    for(dp in subset_dat$SNP1){
      if(dp==11){
        color_vec = c(color_vec, "darkgreen")
      }else if(dp==22){
        color_vec = c(color_vec, "yellow")
      }else if(dp==33){
        color_vec = c(color_vec, "red")
      }else if(dp==44){
        color_vec = c(color_vec, "blue")
      }else if(dp==12 || dp==21){
        color_vec = c(color_vec, "green")
      }else if(dp==13 || dp==31){
        color_vec = c(color_vec, "orange")
      }else if(dp==14 || dp==41){
        color_vec = c(color_vec, "purple")
      }else if(dp==23 || dp==32){
        color_vec = c(color_vec, "skyblue")
      }else if(dp==24 || dp==42){
        color_vec = c(color_vec, "limegreen")
      }else if(dp==34 || dp==43){
        color_vec = c(color_vec, "navy")
      }
    }

    print(m %>% addCircles(lng=x_cords, lat=y_cords, color=color_vec))
  }

  Sys.sleep(2)
}

