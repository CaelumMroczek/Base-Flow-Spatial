#############
# This script will download annual temp data from PRISM and process it.
# The rasters will be averaged over the basins and values for each basin/year will 
# be extracted.
#############

####################################################
# Packages Vector
packages <- c("tidyverse", "sf", "raster", "prism", "exactextractr")

# Install packages not yet installed
installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages])
}

# Packages loading
invisible(lapply(packages, library, character.only = TRUE))

setwd("~/Documents/GitHub/BFI_Research/Base-Flow-Spatial")
prism_set_dl_dir("~/Documents/GitHub/Base-Flow-Spatial/PRISM")

####################################################
#download PRISM data
get_prism_annual("tmean", years = 1901:2022, keepZip = FALSE)

#Format to pull produce raster:
tmp_2013<- pd_to_file(prism_archive_subset("tmean", "annual", years = 2013))
tmp_2013_rast <- raster(tmp_2013)

huc8_shape <- shapefile("~/GitHub/Base-Flow-Spatial/Data/HUC8_shapefile/HUC8_AZ.shp")

#set CRS to the same
huc8_shape <- spTransform(huc8_shape, crs(tmp_2013_rast))

#Assign annual precip to each HUC8 for period of record
HUC_temp <- data.frame(HUC = huc8_shape$HUC8) #initialize
count <- 1 #initialize

for(i in 1901:2022){ #period of record
  count <- count+1
  
  rast_file <- pd_to_file(prism_archive_subset("tmean", "annual", years = i)) #read raster filename
  tmp_rast <- raster(rast_file) #create raster
  
  #produce HUC means for that raster
  tmp_mean <- exact_extract(tmp_rast, huc8_shape, fun = "mean")
  
  HUC_temp[,count] <- round(tmp_mean,2) #input means to dataframe
  colnames(HUC_temp)[count] <- c(as.character(i)) #rename column
}

write_csv(HUC_temp, "~/GitHub/Base-Flow-Spatial/Data/HUC_temp.csv")

####################################################
# Reassign precip values from full dataset with new precip values
####################################################



HUC_Database <- read_csv("Data/HUC_Data_11102023.csv")

for(i in 1:nrow(HUC_Database)){
  huc <- HUC_Database$HUC8[i]
  year <- HUC_Database$YEAR[i]
  
  HUC_Database$PRECIP_MM[i] <- HUC_precip[which(HUC_precip$HUC == huc), which(colnames(HUC_precip) == year)]
  
}

write_csv(HUC_Database, "~/Documents/GitHub/BFI_Research/Base-Flow-Spatial/Data/HUC_Data_newPRECIP.csv")
