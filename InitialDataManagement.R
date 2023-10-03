#Caelum Mroczek
#10/2/23

#Initialize packages
packages <- c("tidyverse", "ggplot2","extrafont", "ggthemes", "sf", "raster", "terra")

# Install packages not yet installed
installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages])
}

# Packages loading
invisible(lapply(packages, library, character.only = TRUE))

setwd("~/Documents/GitHub/Base-Flow-Spatial")
set.seed(313)

## Load datasets
streamgages <- read_csv("~/Documents/GitHub/Base-Flow-Spatial/Data/GaugeList_FINAL.csv")
HUC_data <- read_csv("~/Documents/GitHub/Base-Flow-Spatial/Data/HUC_Data_noCO_08252023.csv")
GWBasin_data <- read_csv("~/Documents/GitHub/Base-Flow-Spatial/Data/GWBasin_Data_noCO_08252023.csv")

DEM <- rast("~/Documents/GitHub/Base-Flow-Spatial/Data/DEM_30M/DEM_30M.tif")
DEM <- project(DEM, "+proj=longlat")

###############################################################

#Get elevation data for each streamgage
for (i in 1:length(streamgages$SiteNum)){
  
  point <- vect(streamgages[i,], geom=c("Long","Lat")) #lat long of each gage
  
  e <- extract(DEM,point) #extract elevation to point
  
  elev <- as.numeric(e[,2])
  streamgages$elevation_ft[i] <- round(elev*3.28,3) #convert from meters to feet
  
}

###############################################################

#Replace HUC basin average elevation with site-specific elevation

for(i in 1:length(HUC_data$SITENUM)){
  siteNum <- HUC_data$SITENUM[i]
  index <- which(streamgages$SiteNum == siteNum)
  
  HUC_data$ELEVATION_FT[i] <- streamgages$elevation_ft[index]
}

HUC_data <- HUC_data[,c(1:10,53,12:52)]
write_csv(x = HUC_data,
          file = "~/Documents/GitHub/Base-Flow-Spatial/Data/GWBasin_Data_10022023.csv")

###############################################################

#Replace GW basin average elevation with site-specific elevation

for(i in 1:length(GWBasin_data$SITENUM)){
  siteNum <- GWBasin_data$SITENUM[i]
  index <- which(streamgages$SiteNum == siteNum)
  
  GWBasin_data$ELEVATION_FT[i] <- streamgages$elevation_ft[index]
}

GWBasin_data<- GWBasin_data[,c(1:10,53,12:52)]
write_csv(x = HUC_data,
          file = "~/Documents/GitHub/Base-Flow-Spatial/Data/HUC_Data_10022023.csv")
