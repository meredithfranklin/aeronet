#libraries

library(data.table)
library(dplyr)
library(leaflet)

#######################################################3

#upload data

#list of collocated EPA and AERONET stations <5km USA

data_epa <- fread("G:\\My Drive\\AERONET-MISR\\INV data\\QGS\\aeronet_epa_collocated_5km_usa.csv")
data_improve <- fread("G:\\My Drive\\AERONET-MISR\\INV data\\QGS\\aeronet_improve_collocated_5km_usa.csv")

#Join datasets
data_usa <- rbind(data_epa,data_improve)

