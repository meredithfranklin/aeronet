#function to collocate inversion data and PM2.5 data from collocated sited <5km
#coll.dist should be inserted in meters

collocate.inv.pm <- function(coll.dist,epa="G:\\My Drive\\AERONET-MISR\\INV data\\Data\\aq.sites.csv",
                             aeronetf="G:\\My Drive\\AERONET-MISR\\INV data\\QGS\\aeronet_locations_v3.csv",
                             inv="G:\\My Drive\\AERONET-MISR\\INV data\\Data\\INV_V3\\Almucantar"){

#libraries----------------------------------------------------------------------

library(data.table)
library(dplyr)
library(ggplot2)
library(stringr)
library(stringi)
library(raster)
#---------------------------------------------------------------------------------

#load list of all EPA PM2.5 sites

epa.site <- fread(epa)
epa.site$start.year <- stri_extract_last_regex(epa.site$`Site Established Date`, "\\d{4}")
epa.site$end.year <- stri_extract_last_regex(epa.site$`Site Closed Date`, "\\d{4}")

#extract only EPA sites running from year 2000
epa.site <- epa.site[start.year>1999,]

#save epa list
#write.csv(epa.site,"C:\\Users\\msorekha\\Documents\\DATA\\EPA\\epa.sites.from2000.csv")

################################################
#Read EPA PM2.5 data in all US collocated sites#
################################################

#EPA dir
epa_daily_dir <- "G:\\My Drive\\AERONET-MISR\\INV data\\Data\\EPA-PM25"

# List files per directory
epa_daily <- list.files(path=epa_daily_dir,pattern="daily_88101",full.names = TRUE)


l<-list()
ind=1
for (I in epa_daily){
  l[[ind]] <- fread(I)
  ind=ind+1
}

pm <- rbindlist(l) #all stations in the USA, 2000-2018, 4,688,508 x 29 vars
setnames(pm,"Local Site Name","EPA_Site_Name")
pm$EPA_Site_Name <- 
  pm$EPA_Site_Name %>% 
  strsplit("\\,") %>% 
  sapply("[", 1) 

setnames(pm,"Date Local","Date")
pm$Date <- as.Date(pm$Date )
setnames(pm,"Arithmetic Mean","PM25")
#fst::write.fst(pm,"C:/Users/msorekha/Documents/Projects/JPL/MISR/Data/WDB/EPA.pm.all.fst")

## pm >> variable with EPA pm2.5 data from all USA sites ##

###################################
#load list of all AERONET V3 sites#
###################################

aeronet <- fread(aeronetf)

#Collocate EPA sites and AERONET INV sites <5km buffer
epa.loc <- epa.site[,c("Latitude","Longitude","Local Site Name")]
setnames(epa.loc,"Latitude","Latitude-EPA")
setnames(epa.loc,"Longitude","Longitude-EPA")
setnames(epa.loc,"Local Site Name","EPA_Site_Name")
#calculate distance between sites
dist <- pointDistance(aeronet[,c(2:3)], epa.loc[,c(2:1)], lonlat=TRUE, allpairs=TRUE)

#distance between sites in meters
#remove collocations beyond 5km
dist[dist>coll.dist] <- NA
dist.ind <- which(!is.na(dist),arr.ind=TRUE)
pairs <- cbind(aeronet[dist.ind[,1],],epa.loc[dist.ind[,2],]) #151 PM2.5 sites collocated with AERONET sites<5km away

#save collocated list
#write.csv(pairs,"C:\\Users\\msorekha\\Documents\\DATA\\EPA\\collocated_EPA_AERONET_2000.csv")


#Clean list of INV site names--------------------------------------------------------

#load list of inv files
inv.files <- list.files(path=inv,pattern=".all",full.names = TRUE)
inv.names <- list.files(path=inv,pattern=".all")

names <- gsub("^\\P{L}*|.all","",inv.names,perl = TRUE)

#Merge EPA and INV lists by site name from AERONET-EPA pairs list-------------------------------
epa_inv <- pairs[ pairs$Site_Name %in% names ] #121 pairs - SOME EPA SITES DON'T HAVE SITE NAME - CHECK!

#save list of AERONET INVERSION sites collocated with EPA sites
#write.csv(aeronet.inv,"G:\\My Drive\\AERONET-MISR\\INV data\\QGS\\aeronetINV_colocations_v3.csv")

#checkInversion data availability in the collocated stations-----------

###############################
#Locate AERONET INV data files#
###############################

inv.filenames <- list.files(path=inv)

#check for matching site names
col.data=data.table()
for (x in 1:dim(epa_inv)[1]){
match <- cbind(epa_inv$Site_Name[x],grep(epa_inv$Site_Name[x], inv.filenames, value = TRUE))
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

#merge EPA data and collocated aeronet INV data
#(1) add inv site name to pm data
inv.pm <- merge(pm,epa_inv,by=c("EPA_Site_Name"))#14,465
inv.pm <- subset(inv.pm,select=c("EPA_Site_Name" ,"Date","PM25","Site_Name"))

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
aer.inv.clean <- aer.inv[,c( "Site_Name"                                                
                             , "Date"                                                     
                             , "hour"                                                     
                             , "Day_of_Year"                                              
                             , "AOD_Coincident_Input[440nm]"                              
                             , "AOD_Coincident_Input[675nm]"                              
                             , "AOD_Coincident_Input[870nm]"                              
                             , "AOD_Coincident_Input[1020nm]"                             
                             ,"Angstrom_Exponent_440-870nm_from_Coincident_Input_AOD"    
                             , "AOD_Extinction-Total[440nm]"                              
                             , "AOD_Extinction-Total[675nm]"                              
                             ,"AOD_Extinction-Total[870nm]"                              
                             , "AOD_Extinction-Total[1020nm]"                             
                             , "AOD_Extinction-Fine[440nm]"                               
                             , "AOD_Extinction-Fine[675nm]"                               
                             , "AOD_Extinction-Fine[870nm]"                               
                             , "AOD_Extinction-Fine[1020nm]"                              
                             , "AOD_Extinction-Coarse[440nm]"                             
                             , "AOD_Extinction-Coarse[675nm]"                             
                             , "AOD_Extinction-Coarse[870nm]"                             
                             , "AOD_Extinction-Coarse[1020nm]"                            
                             , "Extinction_Angstrom_Exponent_440-870nm-Total"             
                             , "Single_Scattering_Albedo[440nm]"                          
                             , "Single_Scattering_Albedo[675nm]"                          
                             , "Single_Scattering_Albedo[870nm]"                          
                             , "Single_Scattering_Albedo[1020nm]"                         
                             , "Absorption_AOD[440nm]"                                    
                             , "Absorption_AOD[675nm]"                                    
                             ,"Absorption_AOD[870nm]"                                    
                             , "Absorption_AOD[1020nm]"                                   
                             , "Absorption_Angstrom_Exponent_440-870nm"                   
                             , "Refractive_Index-Real_Part[440nm]"                        
                             , "Refractive_Index-Real_Part[675nm]"                        
                             , "Refractive_Index-Real_Part[870nm]"                        
                             , "Refractive_Index-Real_Part[1020nm]"                       
                             , "Refractive_Index-Imaginary_Part[440nm]"                   
                             , "Refractive_Index-Imaginary_Part[675nm]"                   
                             , "Refractive_Index-Imaginary_Part[870nm]"                   
                             , "Refractive_Index-Imaginary_Part[1020nm]"                  
                             , "Asymmetry_Factor-Total[440nm]"                            
                             , "Asymmetry_Factor-Total[675nm]"                            
                             , "Asymmetry_Factor-Total[870nm]"                            
                             ,"Asymmetry_Factor-Total[1020nm]"                           
                             , "Asymmetry_Factor-Fine[440nm]"                             
                             , "Asymmetry_Factor-Fine[675nm]"                             
                             , "Asymmetry_Factor-Fine[870nm]"                             
                             , "Asymmetry_Factor-Fine[1020nm]"                            
                             , "Asymmetry_Factor-Coarse[440nm]"                           
                             , "Asymmetry_Factor-Coarse[675nm]"                           
                             , "Asymmetry_Factor-Coarse[870nm]"                           
                             , "Asymmetry_Factor-Coarse[1020nm]", 
                             "Elevation(m)",
                             "Longitude(Degrees)","Latitude(Degrees)",
                             "Coincident_AOD440nm",
                             "Solar_Zenith_Angle_for_Measurement_Start(Degrees)",
                             "Std-C", "VMR-C","REff-C","VolC-C","Std-F", "VMR-F","REff-F", "VolC-F",
                             "Std-T","VolC-T","REff-T", "VMR-T","Sphericity_Factor(%)")]

aer.inv.clean$hour <- NULL

#calculate daily mean
aer.inv.clean <- aer.inv.clean%>%group_by(Site_Name,Date)%>%summarise_all(funs(mean))


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

#merge EPA data and collocated aeronet INV data
#(2) add inv data to pm data
inv.pm.data <- merge(inv.pm,aer.inv.clean,by=c("Site_Name","Date"))

#save file
#write.csv(inv.pm.data,"G:\\My Drive\\AERONET-MISR\\INV data\\QGS\\inv.pm.data.csv")

return(inv.pm.data)
}