library(sf)
library(tidyverse)

countries <- giscoR::gisco_get_countries() %>% 
  st_centroid() 

coordinates = countries %>% 
  st_coordinates

cbind(countries$NAME_ENGL, coordinates) %>% 
  as.data.frame() %>% 
  rename("Country"=V1, "Long"=X, "Lat"=Y) %>% 
  write.table(file="0_data/country_cords.tsv", quote=FALSE, sep="\t", row.names=FALSE)