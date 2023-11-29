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
  BFI.df <- data.frame(matrix(ncol = 4, nrow = nrow(df)))
  colnames(BFI.df) <- c("numPoints", "BFI.predict", "BFI.calc", "MSE")
  
  for(i in 1:nrow(df)){ 
    #Estimate basin-averaged BFI with each number of points
    points <- df[1:i,]
    #loop through each point lat/long to predict BFI for each point
    pointBFI <- BFI.predictor(points, "/Users/caelum/Documents/GitHub/BFI_Research/XGB_Training/10FoldCV_HUC_XGBModel.rda")  
    
    BFI.df$numPoints[i] <- i
    BFI.df$BFI.predict[i] <- mean(pointBFI$mean_predictedBFI)
    
    if(site == "LSP"){ #assign calculated basin-averaged BFI value depending on test basin
      BFI.calc <- 0.377
    }else{BFI.calc <- 0.506}
    
    BFI.df$BFI.calc[i] <- BFI.calc
    BFI.df$MSE[i] <- mean((BFI.df$BFI.calc[i]-BFI.df$BFI.predict[i])^2)
  }#Output -> table of NumPoints, BFI.calc, BFI.predict, MSE
  
  return(BFI.df)
}

LSP_pts <- read.csv("./Data/LSP_Points.csv")

LowerSanPedro <- point.test(LSP_pts, "LSP")

#best num of points to minimize MSE
MSE.min <- min(LowerSanPedro$MSE)
best.n.LSP <- (which(LowerSanPedro$MSE == MSE.min))  

#Plot MSE vs number of points to minimize MSE
LSP.MSE.plot <- ggplot(LowerSanPedro, mapping = aes(numPoints,MSE))+
  geom_line()+
  geom_point()+
  geom_point(mapping = aes(best.n.LSP, MSE[best.n.LSP]), color = 'red')+
  labs(title = "Lower San Pedro")

UV_pts <- read.csv("./Data/UV_Points.csv")

UpperVerde <- point.test(UV_pts, "UV")

#best num of points to minimize MSE
MSE.min <- min(UpperVerde$MSE)
best.n.UV <- (which(UpperVerde$MSE == MSE.min))  

#Plot MSE vs number of points to minimize MSE
UV.MSE.plot <- ggplot(UpperVerde, mapping = aes(numPoints,MSE))+
  geom_line()+
  geom_point()+
  geom_point(mapping = aes(best.n.UV, MSE[best.n.UV]), color = 'red')+
  labs(title = "Upper Verde")

LSP.MSE.plot
UV.MSE.plot

