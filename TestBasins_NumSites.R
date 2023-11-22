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

sites.LSP <- which(BFI_calcs$SiteNum == "9472000" | BFI_calcs$SiteNum == "9473000" & BFI_calcs$Year >= 1991)
sites.UV <- which(BFI_calcs$SiteNum %in% c("9502960", "9503000", "9503300", "9503700", "9502900", "9504000", "9504500", "9504420", "9505350", "9505300", "9505200") & BFI_calcs$Year >= 1991)

#calculated long-term mean BFI
BFI.LSP <- mean(BFI_calcs$BFI[sites.LSP])
BFI.UV <- mean(BFI_calcs$BFI[sites.UV])

##################################################################
##################################################################

#Loop through different number of points
  #Estimate basin-averaged BFI with each number of points
  #Calculate MSE and save to a matrix

#Plot MSE vs number of points to minimize MSE