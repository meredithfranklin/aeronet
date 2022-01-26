#AOD log-log quadratic interpolation function

# Process the AOD information
#1. Take all the valid wavelengths and valid AERONET AOD V3 information 
#2. Take the natural log of the wavelengths and the AODs.  
#3. Fit a second order polynomial to this result. 
#4. Take the result of this fitting procedure to calculate the (log of) the AOD at the
#(log of) the wanted wavelength (e.g. 550 nm).
#5. Exponentiate the result.

# e.g full path to filename
#filename = "C:/Users/msorekha/Documents/Projects/JPL/MISR/Data/WDB/AODonly.pm.CSN.csv"
#wv1-4 = numeric wavelengths to be used in the interpolation.

aod_interpolation <- function(filename,wv1,wv2,wv3,wv4){
  
  
  #libraries
  library(data.table)
  library(pracma)
  
  #load speciation data - CSN+AOD
  if (is.character(filename)) {
  aod_csn <- fread(filename)
   } else {
    aod_csn <- filename
  }
  setDT(aod_csn)
  
  #choose only AERONET-AOD variables
  aod_pair <- aod_csn[,grep("AOD",names(aod_csn)),with=F]
  aod_pair <- aod_pair[,c(1:4)] #specific for a certain dataset we used
  # make sure all values are positive and fill values '-999' are NA
  aod_pair[aod_pair<0]<-NA   
  
  
  # Calculate AOD at 550nm wavelength using 2nd order interpolation
  setDF(aod_pair)
  aods <- aod_pair[,(substr(names(aod_pair),5,7)== wv1 | 
                       substr(names(aod_pair),5,7)== wv2 |
                       substr(names(aod_pair),5,7)== wv3 |
                       substr(names(aod_pair),5,7)== wv4)] 
  
  logs_aods <- data.matrix(log(aods))
  
  wave <- as.numeric(substr(names(aods),5,7))
  
  logs_wave <- log(wave)
  
  #formula = log_aod~b0+b1*log_wave+b2*(log_wave)^2
  
  for (i in 1:dim(logs_aods)[1]){
    
    if (sum(is.na(logs_aods[i,]))>1){
      
      aod_csn$AOD_550q[i] <- NA
      
    } else if (sum(is.na(logs_aods[i,]))==1) {
        logs_wave_temp <- logs_wave[-which(is.na(logs_aods[i,]))]
        logs_aods_temp <- logs_aods[i,-which(is.na(logs_aods[i,]))]
        
        #calculate 2nd order polynomial coefficients 
        output <- polyfit(logs_wave_temp,logs_aods_temp,2);
        
        #predict for AOD 550nm
        aod_csn$AOD_550q[i] <- exp(polyval(output, log(550)))
        
    } else {
      #calculate 2nd order polynomial coefficients 
      output <- polyfit(logs_wave,logs_aods[i,],2);
      
      #predict for AOD 550nm
      aod_csn$AOD_550q[i] <- exp(polyval(output, log(550)))
    }
  }
  return(aod_csn)
}