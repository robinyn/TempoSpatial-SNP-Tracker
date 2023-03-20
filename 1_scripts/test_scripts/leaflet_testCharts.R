library(tidyverse)
library(leaflet)
library(RSQLite)
library(leaflet.minicharts)

setwd("~/Dev/school/BINP29/popgen")

timestep = 500
SNP_ID = "rs3094315"
#SNP_ID = "rs6696609"
twoSNPs = FALSE

db = dbConnect(SQLite(), "0_data/popgen_SNP.sqlite")
res = dbSendQuery(db, sprintf("SELECT MasterID, Long, Lat, Country, DateMean FROM sample_meta"))
sample_dat = dbFetch(res)
dbClearResult(res)

sample_list = paste0(sprintf("`%s`", sample_dat$MasterID), collapse=",")
query = sprintf("SELECT %s FROM main WHERE SNP_ID='%s'", sample_list, SNP_ID)
res = dbSendQuery(db, query)
SNP_dat = dbFetch(res)
dbClearResult(res)

country_cords = sample_dat %>%
  group_by(Long, Lat, DateMean) %>%
  select(Long, Lat, DateMean)

country_cords = country_cords[!duplicated(country_cords$Long),]
country_cords = country_cords[!duplicated(country_cords$Lat),]

SNP_dat = SNP_dat %>% 
  pivot_longer(cols = everything(), names_to = "MasterID", values_to = "SNP1")

plot_dat = merge(sample_dat, SNP_dat, by="MasterID")

plot_dat = plot_dat %>%
  group_by(Long, Lat, SNP1) %>%
  tally() %>%
  pivot_wider(names_from=SNP1, values_from=n) %>%
  replace(is.na(.), 0)

plot_dat = merge(plot_dat, country_cords, by=c("Long", "Lat"))

#plot_dat = plot_dat[plot_dat$SNP1!="00",]
plot_dat = plot_dat[order(as.numeric(plot_dat$DateMean)),]
m = leaflet() %>% 
  addTiles() %>% 
  setView(30, 50, zoom = 2) %>% 
  setMaxBounds(-90,-180,90,180) 

m = m %>%
  addMinicharts(
    lng=as.numeric(plot_dat$Long), lat=as.numeric(plot_dat$Lat),
    type="pie",
    time=as.numeric(plot_dat$DateMean),
    chartdata=plot_dat[, !(colnames(plot_dat) %in% c("Lat", "Long", "DateMean"))]
  )

print(m)
