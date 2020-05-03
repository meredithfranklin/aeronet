# colloacte.inv.spec <- function(aeronetf="G:\\My Drive\\AERONET-MISR\\INV data\\QGS\\aeronet_locations_v3.csv",
#                                inv="G:\\My Drive\\AERONET-MISR\\INV data\\Data\\INV_V3\\Almucantar"){

#remove next 2 rows when function works
aeronetf="G:\\My Drive\\AERONET-MISR\\INV data\\QGS\\aeronet_locations_v3.csv"
inv="G:\\My Drive\\AERONET-MISR\\INV data\\Data\\INV_V3\\Almucantar"

library(data.table)
library(dplyr)
library(reshape2)
library(raster)

################################################
#Read EPA PM2.5 speciation data in all US sites#
################################################

#Load dataset with inversion data and pm2.5 data
source('G:/My Drive/AERONET-MISR/INV data/Code/collocate.inv.pm.R')
inv.pm.data<-collocate.inv.pm()

#load allaeronet locations
aeronet <- fread(aeronetf)

#EPA spec dir
epa_spec_dir <- "G:\\My Drive\\AERONET-MISR\\INV data\\Data\\EPA-SPEC\\speciation"

# List files per directory
epa_spec <- list.files(path=epa_spec_dir,pattern="*.csv",full.names = TRUE)

#Read data (by year due to memory issues)
l<-list()
ind=1

for(I in epa_spec){
  file.spec <- fread(I)
  setnames(file.spec,"Local Site Name","spec_Site_Name")
  setnames(file.spec,"Date Local","Date")
  file.spec$Date <- as.Date(file.spec$Date )
  
  #Filter by variables
  #88321 = EC
  #88380 = EC
  #88320 = OC
  #88403 = Sulfate
  #88306 = Nitrate
  #88104 = Al
  #88165 = Si
  #88111  = Ca
  #88161  = Ti
  #88126  = Fe
  parameter.list <- c("88321","88380","88320","88403","88306","88104","88165","88111","88161","88126")
  file.spec.clean <-file.spec %>% filter(`Parameter Code`%in% parameter.list)
  file.spec.clean <- file.spec.clean[,c("Latitude","Longitude","Date" ,"Parameter Name",
                                        "Arithmetic Mean","spec_Site_Name")]
  file.spec.loc <- unique(file.spec.clean[,c("Latitude","Longitude","spec_Site_Name")])          
 
    #calculate distance between EPA spec and aeronet sites
  dist <- pointDistance(aeronet[,c(2:3)], file.spec.loc[,c(2:1)], lonlat=TRUE, allpairs=TRUE)
  
  #distance between sites in meters
  #remove collocations beyond 5km
  dist[dist>5000] <- NA
  dist.ind <- which(!is.na(dist),arr.ind=TRUE)
  dist.value <- as.data.frame(dist[which(!is.na(dist))])
  dist.ind <- cbind(dist.ind,dist.value)
  setnames(dist.ind,"dist[which(!is.na(dist))]","D")
  pairs <- cbind(aeronet[dist.ind[,1],],file.spec.loc[dist.ind[,2],],dist.ind$D)
  
  #save collocated list
  #write.csv(pairs,"G:\\My Drive\\AERONET-MISR\\INV data\\Data\\EPA-SPEC\\speciation\\collocated_SPEC_AERONET.csv")
  
  #reshape by parameter names
  file.spec.melt <- melt(file.spec.clean,id.vars = c("Parameter Name","spec_Site_Name","Date","Latitude","Longitude"))

  #insert into list
  l[[ind]] <- file.spec.melt
    ind=ind+1
}

spec.all <- rbindlist(l) #one variable with ALL speciation data 2000-2018

#save speciation all years list
#write.csv(spec.all,"G:\\My Drive\\AERONET-MISR\\INV data\\Data\\EPA-SPEC\\speciation\\SPEC_data_2000_2018.csv")

######################
# merge with INV data#
######################

#Clean list of INV site names--------------------------------------------------------

#load list of inv files
inv.files <- list.files(path=inv,pattern=".all",full.names = TRUE)
inv.names <- list.files(path=inv,pattern=".all")

names <- gsub("^\\P{L}*|.all","",inv.names,perl = TRUE)

#Merge EPA and INV lists by site name from AERONET-EPA pairs list-------------------------------
spec_inv <- pairs[ pairs$Site_Name %in% names ] #121 pairs - SOME  SITES DON'T HAVE SITE NAME - CHECK!

#If EPA speciation site name missing - aeronet site name is used instead
for(I in 1:dim(spec_inv)[1]){
  if(spec_inv[I]$spec_Site_Name=="")
    spec_inv[I]$spec_Site_Name<-spec_inv$Site_Name[I]
}

#save list of AERONET INVERSION sites collocated with EPA sites
#write.csv(aeronet.inv,"G:\\My Drive\\AERONET-MISR\\INV data\\QGS\\aeronetINV_SPECcolocations.csv")

#checkInversion data availability in the collocated stations-----------

###############################
#Locate AERONET INV data files#
###############################

inv.filenames <- list.files(path=inv)

#check for matching site names
col.data=data.table()
for (x in 1:dim(spec_inv)[1]){
  match <- cbind(spec_inv$Site_Name[x],grep(spec_inv$Site_Name[x], inv.filenames, value = TRUE))
  col.data <- rbind(col.data,match)
}

#82 unique AERONET sites with Inversion data
setnames(col.data,"V1","Site_name")
setnames(col.data,"V2","inv.filenames")
col.data.clean <- list(col.data$inv.filenames)

inv.files <- data.table(list.files(path=inv,full.names = TRUE))
inv.files <- cbind(inv.files,inv.filenames)
setDT(inv.files)

files <- subset(inv.files,inv.files$inv.filenames %in% col.data$inv.filenames )
#################################################

#merge spec data and collocated aeronet INV data
#(1) add inv site name to speciation data
sitenames<-spec_inv[,c("Site_Name","spec_Site_Name")]
inv.spec <- merge(spec.all,sitenames)#642,828
inv.spec <- filter(inv.spec,variable=="Arithmetic Mean")

##################################################
#Read AERONET INV data in all US collocated sites#
##################################################

l<-list()
for (I in files$V1){
  stn.name <- 
    I %>% 
    strsplit("[.]") %>% 
    sapply("[", 1) %>%
    strsplit("[/]") %>% 
    lapply(rev) %>% 
    sapply("[", 1) %>% 
    strsplit("[0-9]+") %>%
    lapply(rev) %>% 
    sapply("[", 1) %>%
    substring( 2, 100)
  l[[stn.name]] <- fread(I,skip=6,fill=TRUE)
}

aer.inv<- setDT(rbindlist(l,idcol=TRUE)) #daily 67810x156vars (82 unique sites)
aer.inv$`.id`<-NULL
setnames(aer.inv,"Site","Site_Name")
setnames(aer.inv,"Date(dd:mm:yyyy)","Date")
setnames(aer.inv,"Time(hh:mm:ss)","hour")
aer.inv$Date <- as.Date(aer.inv$Date, format="%d:%m:%Y")    

#exclude unwanted vars
aer.inv.clean <- aer.inv[,-c(155,154,123:150,89:121,54:76)]
aer.inv.clean$hour <- NULL

#calculate daily mean
aer.inv.clean <- aer.inv.clean%>%group_by(Site_Name,Date)%>%summarise_all(tibble::lst(mean))


#availability
avail <- aer.inv.clean%>%group_by(Site_Name)%>%summarise(N=n())

#merge with list of filenames
#avail <- merge(avail,files)

#save file
#write.csv(avail,"G:\\My Drive\\AERONET-MISR\\INV data\\QGS\\inv.avail.bysite.csv")

#Summary by category of availability, no of days
#<1yr; 1-3; 3-5; 5-10; 10-20
setDT(avail)
avail$N <- as.numeric(avail$N)

avail[N<=365,flag:=1]             #<=1 years of data
avail[(N>365 & N<=1095),flag:=2]  #1-3 years of data
avail[(N>1095 & N<=1825),flag:=3] #3-5 years of data
avail[(N>1825 & N<=3650),flag:=4] #5-10 years of data
avail[(N>3650 & N<=6070),flag:=5] #>10 years of data

avail.sum <- avail %>% group_by(flag) %>% summarise(N=n())
avail.sum #DOES NOT TAKE INTO ACCOUNT SIMILAR LOCATION WITH DIFFERENT SITE NAMES FOR DIFFERENT TIME PERIODS, E.G. FRESNO

#merge availability back to inv data variable
aer.inv.clean <- merge(aer.inv.clean,avail,by="Site_Name")
#################################################

#merge EPA speciation data and collocated aeronet INV data

#reshape to final data table
#NOT WORKING
# final.table.wide <- dcast(inv.spec,
#                           Date + Latitude + Longitude + Site_Name ~ `Parameter Name`,
#                           value.var="value")

#(2) add inv data to spec data
inv.spec.data <- merge(inv.spec,aer.inv.clean,by=c("Site_Name","Date"))#158,68 (21 sites?)

#save file
#write.csv(inv.spec.data,"G:\\My Drive\\AERONET-MISR\\INV data\\QGS\\inv.spec.data.csv")


# return(inv.spec.data)
# }