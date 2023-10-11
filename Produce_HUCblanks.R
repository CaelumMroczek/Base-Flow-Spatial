# Packages Vector
packages <- c("tidyverse", "sf", "raster", "terra", "ggplot2")

# Install packages not yet installed
installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages])
}

# Packages loading
invisible(lapply(packages, library, character.only = TRUE))
####################################################################################

##Load in data
#dataset with stream reach code and lat/long for desired river
River_Points <- read_csv("/Users/Caelum/Desktop/SaltRiver_points.csv") 

#dataset of all predictors, keyed to HUC8 number
HUC_Predictors <- read_csv("/Users/Caelum/Library/Mobile Documents/com~apple~CloudDocs/NAU/Research/AZBFI_Manuscript/VariableData/HUC_Variables/HUC_Dataset.csv")

HUC8_Shape <- read_sf(dsn = "/Volumes/Mroczek,Caelum", layer = "HUC8_AZ")
####################################################################################
HUC8_Shape_sp <- as(HUC8_Shape, "Spatial")

# Now try using over function with SpatialPoints and Spatial object
point <- st_transform(point, crs = st_crs(HUC8_Shape))
over(point, HUC8_Shape_sp)

point <- SpatialPoints(River_Points[1,])
sp::over(point,HUC8_Shape)


site_calcs <- function(dataset){
  
  dataset = cbind(dataset, BFI = 0) #Create BFI column in dataset
  dataset = cbind(dataset, Discharge_cfs = 0)
  errors = c()
  
  for (i in 1:nrow(dataset)){
    site_no <- as.character(dataset[i,][2]) #retrieve site number for every site
    
    gauge <- readNWISdv(siteNumbers = toString(site_no), 
                        parameterCd = "00060", statCd = "00003",
                        startDate = "1991-01-01",
                        endDate = "2020-12-31") #Retrieves data from USGS stream data based on site number
    
    gauge <- na.omit(gauge)
    
    if (nrow(gauge)!=0){
      bf <- BaseflowSeparation(gauge$X_00060_00003,passes = 3)#conduct baseflow separation
      
      BFsum <- sum(bf$bt)
      Tsum <- sum(bf$qft) + sum(bf$bt)
      bfi <- BFsum/Tsum
      
      dataset$BFI[i] = bfi
    } else{
      errors = append(errors,i)
    }
  }
  
  output = list('dataset' = dataset, 'errors' = errors)
  return(output)
  
}