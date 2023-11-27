# Packages Vector
packages <- c("tidyverse", "ggplot2", "sf", "raster", "terra", "boot")

# Install packages not yet installed
installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages])
}

# Packages loading
invisible(lapply(packages, library, character.only = TRUE))

##################################################################
##################################################################
#load datasets
LSP_points <- read.csv("Data/LowerSanPedro_Points.csv")
UV_points <- read.csv("Data/UpperVerde_Points.csv")

LSP_points <- LSP_points[-1]
UV_points <- UV_points[-1]

##################################################################
##################################################################

#Calculated basin-averaged BFI for period of record
BFI_calcs <- read.csv("~/Documents/GitHub/BFI_Research/-Hydrograph-Testing/Data/Streamgages/FinalSites_BFI>01.csv")

sites.LSP <- which(BFI_calcs$SiteNum == "9472000" | BFI_calcs$SiteNum == "9473000" & BFI_calcs$Year >= 1991) #streamgage numbers / full period of record
sites.UV <- which(BFI_calcs$SiteNum %in% c("9502960", "9503000", "9503300", "9503700", "9502900", "9504000", "9504500", "9504420", "9505350", "9505300", "9505200") & BFI_calcs$Year >= 1991)

#calculated long-term mean BFI
BFI.LSP <- mean(BFI_calcs$BFI[sites.LSP])
BFI.UV <- mean(BFI_calcs$BFI[sites.UV])

##################################################################
##################################################################

point.test <- function(df, site){
  #Loop through different number of points
  for(i in nrow(df)){ 
    #Estimate basin-averaged BFI with each number of points
    points <- df[1:i,]
    BFI.df <- data.frame(matrix(ncol = 1, nrow = i))
    colnames(BFI.df) <- c("BFI")
      #loop through each point lat/long to predict BFI for each point
      for(j in nrow(points)){
        #calculate BFI estimate at each point and write to table
        lat <- points[j,1]
        long <- points[j,2]
        
        #Feed lat and long into function to predict BFI using model
        
        #Output predicted BFI
        
        BFI.df$BFI[j] <- BFI.predict
        
      } #Output -> table of each point and its respective BFI
    
        #average BFI from all points in BFI.df
        
        #Calculate MSE and save to a matrix
        if(site == "LSP"){ #assign calculated basin-averaged BFI value depending on test basin
          BFI.calc <- 0.377
        }else{BFI.calc <- 0.506}
        
      
  } #Output -> table of NumPoints, BFI.calc, BFI.predict, MSE

#Plot MSE vs number of points to minimize MSE
  
#Return plot and best number of points
}