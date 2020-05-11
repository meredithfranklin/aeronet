#load libraries
library(data.table)
library(dplyr)

#source functions
source('G:/My Drive/AERONET-MISR/INV data/Code/collocate.inv.spec.R')
source('G:/My Drive/AERONET-MISR/INV data/Code/collocate.inv.pm.R')
#########################
#########################

### Execute functions to have two datasets: 
### (1) inversion products and PM2.5 concentrations 
### (2) inversion products and PM speceiation

#Load dataset with inversion data and pm2.5 data
#Provide the collocation distance you are interested in, e.g. coll.dist=5000 = 5km
inv.pm.data <-collocate.inv.pm(coll.dist=5000)

#Load dataset with inversion data and pm speciation data
#Provide the collocation distance you are interested in, e.g. coll.dist=5000 = 5km
inv.spec.data <-colloacte.inv.spec(coll.dist=5000)

#########################
#########################

### Clean dataset 

#remove '_mean' from variable names
names(inv.pm.data) <- sub("_mean","",names(inv.pm.data))
names(inv.spec.data) <- sub("_mean","",names(inv.spec.data))

#Replace '-999' and 'NAN' with NA
inv.pm.data[inv.pm.data==-999]  <- NA
inv.pm.data[is.na(inv.pm.data)]  <- NA
inv.spec.data[inv.spec.data==-999] <- NA
inv.spec.data[is.na(inv.spec.data)]  <- NA

#########################
#########################

### Merge datasets

#Merge datasets
inv.pm.spec <- merge(inv.spec.data,inv.pm.data,all.y=TRUE)

#########################
#########################

### Save file

#Save file
write.csv(inv.pm.spec,'G:/My Drive/AERONET-MISR/INV data/Data/inv.pm.spec.csv')
